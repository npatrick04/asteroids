(in-package #:asteroids)

(defun project-relative-path (path)
  (merge-pathnames path (asdf:system-definition-pathname :asteroids)))

(defun load-sprites (renderer)
  (load-texture (project-relative-path "resources/asteroids.png")
		renderer))

;;; Undoubtedtly there is a better way to do this...
(defun read-sprites ()
  (let ((sprite-specs
	 (with-open-file (in (project-relative-path
			      "src/sprite-details.lisp"))
	   (read in)))
	(sprites (make-hash-table)))
    ;; The sprites are split in groups (ships big medium little),
    ;; length of each group giving the variations.
    (dolist (set sprite-specs sprites)
      (dolist (entry set)
	(setf (gethash (car entry) sprites)
	      (apply #'sdl2:make-rect (cadr entry)))))))

;;; Do at initialization eventually
(defparameter *sprites* (read-sprites))

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
				  :small3
				  :small4
				  :small5))

