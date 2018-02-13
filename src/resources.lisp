(in-package #:asteroids)

(defvar *extents* nil)
(defvar *sprites* nil)
(defvar *sampler* nil)

;;; Definitions per sprite-details.lisp
(defparameter *big-asteroids* '(:big1
				:big2
				:big3
				:big4))
(defparameter *medium-asteroids* '(:medium1
				   :medium2
				   :medium3
				   :medium4))
(defparameter *small-asteroids* '(:small1
				  :small2
				  :small3))

;;; The overall scale, which can be tuned by each object as 
(defparameter *scale* 2.0);;(float 1/500)

(defun project-path (path)
  (asdf:system-relative-pathname :asteroids path))

;;------------------------------------------------------------

(defun make-sprite-stream (extents texture &optional prior-stream)
  "Return a buffer stream of the extents, and a sampler of the sprite."
  (destructuring-bind (x y w h) extents
    (let* ((dims (texture-base-dimensions texture))
	   (tex-x (first dims))
	   (tex-y (second dims))
	   (tl-x (float (/ x tex-x)))
	   (tl-y (float (/ y tex-y)))
	   (tr-x (float (/ (+ x w) tex-x)))
	   (tr-y tl-y)
	   (br-x tr-x)
	   (br-y (float (/ (+ y h) tex-y)))
	   (bl-x tl-x)
	   (bl-y br-y)

	   (w/2 (/ w 2))
	   (h/2 (/ h 2))
	   (-w/2 (/ w -2))
	   (-h/2 (/ h -2))

	   (initial-contents
	    (list (list (v! -w/2 -h/2 0.0 1.0) (v! tl-x tl-y)) ;tl
		  (list (v!  w/2 -h/2 0.0 1.0) (v! tr-x tr-y)) ;tr
		  (list (v!  w/2  h/2 0.0 1.0) (v! br-x br-y)) ;br
		  (list (v! -w/2  h/2 0.0 1.0) (v! bl-x bl-y)) ;bl
		  (list (v! -w/2 -h/2 0.0 1.0) (v! tl-x tl-y)) ;tl
		  (list (v!  w/2  h/2 0.0 1.0) (v! br-x br-y))))) ;br
      (if prior-stream
	  (adjust-gpu-array (caar (buffer-stream-gpu-arrays prior-stream)) 6
			    :initial-contents initial-contents)
	  (make-buffer-stream
	   (make-gpu-array initial-contents
	    :element-type 'g-pt
	    :dimensions 6))))))

(defun read-sprites (texture)
  (let ((sprite-specs
	 (with-open-file (in (project-path "src/sprite-details.lisp"))
	   (read in))))
    (setf *sprites* (make-hash-table)
	  *extents* (make-hash-table))
    
    ;; The sprites are split in groups (ships big medium little),
    ;; length of each group giving the variations.
    ;(format t "Loading sprites: ")
    (dolist (set sprite-specs *sprites*)
      ;(format t "~&  - ")
      (dolist (entry set 
	       )
	;; Delete any prior meshes if necessary
	(let ((prior-stream (gethash (car entry) *sprites*)))
	  ;;(format t "~A " (car entry))
	  (setf (gethash (car entry) *extents*)
		(cadr entry)
		
		(gethash (car entry) *sprites*)
		(make-sprite-stream (cadr entry)
				    texture
				    prior-stream))))
      ;(fresh-line)
      )))

(defun load-sprites ()
  ;(setf *sampler* (tex "resources/asteroids.png"))
  (setf *sampler* (tex "resources/asteroids-colored.png"))
  (read-sprites (sampler-texture *sampler*)))


;;------------------------------------------------------------
;; Calculating asteroid normals, and illumination angles
;; with the GPU.
;;------------------------------------------------------------

;;; To shrink wrap an asteroid, we start with initial geometry of a
;;; square at the extents of each object, then pull in each side until
;;; the first bounding edge is met (should only be a pixel or two).
;;; Then we're left with 4 points touching the asteroid, and 4
;;; corners.  Bring each corner toward the normal created by the two
;;; points on either side of the point in question until a point along
;;; the axis of the normal reaches the drawing.
;;;
;;; Keep subdividing until the error is below some threshold.

(defparameter *geometry* (make-hash-table))
(defparameter *geometry-error-threshold* 2.0
  "The error allowable in generated geometry in terms of max-pixels to line.")

(defun garef (array subscripts)
  "Do an aref on array with rounded subscripts."
  (apply #'aref array (loop for subscript across subscripts collect (round subscript))))

(defun geom-point (v4)
  "return t when geom-point is valid geometry"
  (and (> (x v4) (* 0.5 255))
       (> (y v4) (* 0.5 255))
       (> (z v4) (* 0.5 255))))

(defun fan-triangle (geom center)
  "Create a triangle fan from center to each point in the geom."
  (do* ((result ())
	(i 0 (1+ i))
	(j (1- (length geom)) (1- i)))
       ((= i (length geom)) result)
    (setf result (append
		  (list (v! 0 0 0 1)
			(v! (v:- (elt geom i) center) 0 1)
			(v! (v:- (elt geom j) center) 0 1))
		  result))))

(let (data)
  (defun reset-geometry ()
    (setf data nil))
  (defun initialize-geometry ()
    (setf data (make-array (texture-base-dimensions (sampler-texture *sampler*))
			   :initial-contents (pull-g (sampler-texture *sampler*)))))
  (defun find-geom-point (ic vec &optional max-lim)
    "Given a starting position, find the first model-position in the direction of vec."
    (unless data (initialize-geometry))
    (do* ((vnorm (rtg-math.vector2:normalize vec))
	  (pos ic (v:+ pos vnorm))
	  (iter 0 (1+ iter))
	  (value (garef data pos) (garef data pos)))
	 ((or (geom-point value)
	      (and max-lim
		   (>= iter max-lim)))
	  (when (geom-point value)
	    pos))))
  (defun geom-error (p1 p2)
    "Return a sum of errors from p1 to p2."
    (do* ((dx (- (x p2) (x p1)))
	  (dy (- (y p2) (y p1)))
	  (normal (v:normalize (v! (- dy) dx)))
	  (iterations 0 (1+ iterations))
	  (percent 0 (+ percent 0.1))
	  (pos p1 (v:+ p1 (v! (* dx percent)
			      (* dy percent))))
	  (err 0))
	 ((> iterations 11)
	  (/ err (1- iterations)))
      (unless (geom-point (garef data pos))
	(incf err))))
  (defun find-geometry (entity)
    "shrink-wrap the asteroid."
    (unless data (initialize-geometry))
    (let* ((extents (gethash entity *extents*))
	   (center (v:+ (v! (elt extents 0) (elt extents 1))
			(v:/ (v! (elt extents 2) (elt extents 3)) 2)))
	   (initial-geometry
	    (list (find-geom-point (v:+ center (v! (/ (elt extents 2) -2.0) 0))
				   (v! 1 0)) ; left
		  (find-geom-point (v:+ center (v! 0 (/ (elt extents 3)  2.0)))
				   (v! 0 -1)) ; top
		  (find-geom-point (v:+ center (v! (/ (elt extents 2)  2.0) 0))
				   (v! -1 0)) ; right
		  (find-geom-point (v:+ center (v! 0 (/ (elt extents 3) -2.0)))
				   (v! 0 1))))) ; bottom

      ;; TODO: more than just a diamond
      ;(print triangle-fan)
      (list center
	    initial-geometry
	    (make-buffer-stream
	     (make-gpu-array
	      (mapcar (lambda (v) (v:- v center))
		      initial-geometry)
	      :element-type :vec2
	      :dimensions (length initial-geometry))
	     :primitive :line-loop))))
  (defun fill-geometry ()
    (maphash (lambda (k v)
	       (declare (ignore v))
	       (setf (gethash k *geometry*)
		     (find-geometry k)))
	     *extents*))
  (defun geometry-data ()
    (unless data (initialize-geometry))
    data))
