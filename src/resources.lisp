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

;;; WHITE
(defun geom-point (v4)
  "return t when geom-point is valid geometry"
  (and (> (x v4) (* 0.5 255))
       (> (y v4) (* 0.5 255))
       (> (z v4) (* 0.5 255))))

;;; RED
(defun inside-point (v4)
  "return t when geom-point is inside geometry"
  (and (> (x v4) (* 0.5 255))
       (< (y v4) (* 0.5 255))
       (< (z v4) (* 0.5 255))))

;;; BLUE
(defun outside-point (v4)
  "return t when geom-point is outside the geometry"
  (and (< (x v4) (* 0.5 255))
       (< (y v4) (* 0.5 255))
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

(defparameter *geometry-data* nil)

(defun reset-geometry ()
  (setf *geometry-data* nil))

(defun initialize-geometry ()
  (setf *geometry-data*
	(make-array (texture-base-dimensions (sampler-texture *sampler*))
		    :initial-contents (pull-g (sampler-texture *sampler*)))))

(defun find-geom-point (ic vec &key max-lim (point-test 'geom-point))
  "Given a starting position, find the first model-position in the direction of vec."
  (unless *geometry-data* (initialize-geometry))
  (do* ((vnorm (rtg-math.vector2:normalize vec))
	(pos ic (v:+ pos vnorm))
	(iter 0 (1+ iter))
	(value (garef *geometry-data* pos)
	       (garef *geometry-data* pos))
	(point-type  (funcall point-test value)
		     (funcall point-test value)))
       ((or point-type
	    (and max-lim
		 (>= iter max-lim)))
	(when point-type pos))))

(defun geom-error (p1 p2)
  "Return the values of:
- sum of errors from p1 to p2.
- average of erroneous positions.
- the normal facing outwards."
  (declare (optimize debug))
  (do* ((dx (- (x p2) (x p1)))
	(dy (- (y p2) (y p1)))
	(error-location 0)
	(normal (v:normalize (v! dy (- dx))))
	(iterations 0 (1+ iterations))
	(percent 1/11 (+ percent 1/11))
	(pos p1 (v:+ p1 (v! (* dx percent)
			    (* dy percent))))
	(err 0))
       ((>= iterations 11)
	;(break)
	(values (/ err (1- iterations))
		(if (plusp error-location)
		    (v:+ p1 (v! (* dx 0.5) (* dy 0.5)))
		    (v! 0 0))
		normal))
    ;(break)
    (unless (geom-point (garef *geometry-data* pos))
      (incf error-location percent)
      (incf err))))

(defparameter *max-error* 0.05)
(defparameter *min-geom-detail* 2.0)

#+nil
(defun maybe-insert-point (extents center geom i j)
  "Return new geom if a point needs to be added,
and the next index to check."
  (declare (optimize debug))
  ;; Check if it needs to be checked
  (handler-case
      ;; (< (v:distance (elt geom i) (elt geom j))
      ;;    *min-geom-detail*)
					;(values geom j)
      
      ;; Check if it needs to be split
      (multiple-value-bind
	    (geom-err err-center normal)
	  (geom-error (elt geom i)
		      (elt geom j))
	(if (> geom-err *max-error*)
	    ;; Insert a new point.
	    ;; First find the point in the direction of
	    ;; normal that lies on the extents.
	    (let* ((point (if (outside-point (garef *geometry-data* err-center))
			      (find-geom-point err-center (v:negate normal)
					       :point-test 'geom-point)
			      (v:- (find-geom-point err-center normal
						    :point-test 'outside-point)
				   normal))))
	      (values (concatenate 'list
				   (subseq geom 0 j)
				   (list point)
				   (subseq geom j))
		      i))
	    (values geom j)))
    (error () (values geom j))))

(defun insert-geom-point (geometry i err-center normal)
  (let* ((point (if (outside-point (garef *geometry-data* err-center))
		    (find-geom-point err-center (v:negate normal)
				     :point-test 'geom-point)
		    (v:- (find-geom-point err-center normal
					  :point-test 'outside-point)
			 normal))))
    (concatenate 'list
		 (subseq geometry 0 (1+ i))
		 (list point)
		 (subseq geometry (1+ i)))))

(defun shrink-wrap (geometry)
  "Find all the edges."
  (do* ((i 0)
	(j 1 (mod (1+ i) (length geometry))))
       ((= i (length geometry)) geometry)
    (multiple-value-bind
	  (geom-err err-center normal)
	(geom-error (elt geometry i)
		    (elt geometry j))
      (if (plusp geom-err)
	  (setf geometry (insert-geom-point geometry i err-center normal))
	  (setf i (1+ i))))))

(defun remove-elt (list i)
  (if (< i (length list))
      (concatenate 'list
		   (subseq list 0 i)
		   (subseq list (1+ i)))
      (error "Cannot remove-elt with i > length of list")))

(defun simplify-geometry (geometry)
  "Eliminate excess points."
  (declare (optimize debug))
  (do ((i 0 (1+ i)))
      ((>= i (length geometry)) geometry)
    (do ((j (mod (+ 2 i) (length geometry))
	    (mod (+ 2 i) (length geometry))))
	((or (>= i (length geometry))
	     (> (geom-error (elt geometry i)
			    (elt geometry j))
		1/10))
	 #+nil(format t "geom error ~A ~A: ~A~%"
		 i j
		 (geom-error (elt geometry i)
			     (elt geometry j))))
      #+nil(format t "geom error ~A ~A: ~A~%"
		 i j
		 (geom-error (elt geometry i)
			     (elt geometry j)))
      (if (> j i)
	  (setf geometry (remove-elt geometry (1+ i)))
	  (if (> j 0)
	      (setf geometry (remove-elt geometry (1- j)))
	      (setf geometry (remove-elt geometry (1+ i))))))))

(defun find-geometry (entity)
  "Find the center and geometry of the asteroid."
  (declare (optimize debug))
  (unless *geometry-data* (initialize-geometry))
  (let* ((extents (gethash entity *extents*))
	 (center (v:+ (v! (elt extents 0) (elt extents 1))
		      (v:/ (v! (elt extents 2) (elt extents 3)) 2)))
	 (initial-geometry
	  (list (find-geom-point (v:+ center (v! (/ (elt extents 2) -2.0) 0))
				 (v! 1 0)) ; left
		(find-geom-point (v:+ center (v! 0 (/ (elt extents 3)  -2.0)))
				 (v! 0 1)) ; top
		(find-geom-point (v:+ center (v! (/ (elt extents 2)  2.0) 0))
				 (v! -1 0)) ; right
		(find-geom-point (v:+ center (v! 0 (/ (elt extents 3) 2.0)))
				 (v! 0 -1)))) ; bottom
	 (the-geometry (simplify-geometry (shrink-wrap initial-geometry))))
    (list center
	  the-geometry
	  (make-buffer-stream
	   (make-gpu-array
	    (mapcar (lambda (v) (v:- v center))
		    the-geometry)
	    :element-type :vec2
	    :dimensions (length the-geometry))
	   :primitive :line-loop))))

#+nil
(defun find-geometry (entity)
  "shrink-wrap the asteroid."
  (declare (optimize debug))
  (unless *geometry-data* (initialize-geometry))
  (let* ((extents (gethash entity *extents*))
	 (center (v:+ (v! (elt extents 0) (elt extents 1))
		      (v:/ (v! (elt extents 2) (elt extents 3)) 2)))
	 (initial-geometry
	  (list (find-geom-point (v:+ center (v! (/ (elt extents 2) -2.0) 0))
				 (v! 1 0)) ; left
		(find-geom-point (v:+ center (v! 0 (/ (elt extents 3)  -2.0)))
				 (v! 0 1)) ; top
		(find-geom-point (v:+ center (v! (/ (elt extents 2)  2.0) 0))
				 (v! -1 0)) ; right
		(find-geom-point (v:+ center (v! 0 (/ (elt extents 3) 2.0)))
				 (v! 0 -1))))) ; bottom

    ;; initial-geometry is a diamond shape around the object.
    ;; Now check for error between points, to see where the
    ;; geometry needs to be adjusted.
    ;;(break)
    (let ((the-geometry
	   (do* ((i 0 (1+ i))
		 (redo 0)
		 (geom (append initial-geometry (list (first initial-geometry))))
		 (j 1 (1+ i)))
		((= i (1- (length geom))) (butlast geom))
	     (if (> redo 3)
		 (setf redo 0)
		 (multiple-value-bind
		       (new-geom next-i)
		     (maybe-insert-point extents center
					 geom
					 i
					 j)
		   ;; (format t "New Geom from ~D to ~D: ~A~%"
		   ;; 	       i next-i new-geom)
		   (when (and (= i next-i)
			      (> (v:distance (elt new-geom next-i)
					     (elt new-geom (1+ next-i)))
				 *min-geom-detail*)
			      (> (v:distance (elt new-geom (+ 1 next-i))
					     (elt new-geom (+ 2 next-i)))
				 *min-geom-detail*))
		     (incf redo)
		     (setf geom new-geom
			   i (1- next-i))))))))
					;(print the-geometry)
      
      (list center
	    the-geometry
	    (make-buffer-stream
	     (make-gpu-array
	      (mapcar (lambda (v) (v:- v center))
		      the-geometry)
	      :element-type :vec2
	      :dimensions (length the-geometry))
	     :primitive :line-loop)))))

(defun fill-geometry ()
  (maphash (lambda (k v)
	     (declare (ignore v))
	     (setf (gethash k *geometry*)
		   (find-geometry k)))
	   *extents*))

(defun geometry-data ()
  (unless *geometry-data* (initialize-geometry))
  *geometry-data*)
