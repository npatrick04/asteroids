(in-package #:asteroids)

;;; See play-with-verts

(defvar *game-objects* ())

(defun add-object (object)
  (push object *game-objects*))

(defclass thing ()
  ((stream
    :initarg :stream :initform nil :accessor buf-stream)
   (sampler
    :initarg :sampler :initform nil :accessor sampler)
   (specular-sampler
    :initarg :specular :initform nil :accessor specular-sampler)
   (posv
    :initarg :posv :initform (v! 0 0 0) :accessor posv)
   (rotv
    :initarg :rotv :initform (q:identity) :accessor rotv)
   (scale
    :initarg :scale :initform 1f0 :accessor scale)
   (hit :initform nil :accessor hit)))

(defgeneric get-model->world-space (thing))

(defmethod get-model->world-space ((thing thing))
  (let* ((half-extents (v:/ (screen-extents) 2f0))
	 (world-pos (v:- (posv thing) (v! half-extents 0))))
    (m4:* (m4:translation world-pos)
	  (q:to-mat4 (rotv thing)))))

(defgeneric update (thing delta-time))
(defmethod update ((thing thing) delta-time))

;;; Draw 5 times, center, up, right, down, and left
(defun draw-wrapping (thing)
  (let ((extents (screen-extents))
	(pos (pos thing)))
    (setf (pos thing) (v:+ pos (v! (x extents) 0)))
    (draw thing)

    (setf (pos thing) (v:+ pos (v! (- (x extents)) 0)))
    (draw thing)

    (setf (pos thing) (v:+ pos (v! 0 (y extents))))
    (draw thing)

    (setf (pos thing) (v:+ pos (v! 0 (- (y extents)))))
    (draw thing)

    (setf (pos thing) pos)
    (draw thing)))
#+nil
(defmethod draw ((thing thing))
  (map-g #'asteroid-pipeline (buf-stream thing)
         :scale (scale thing)
         :model->world (get-model->world-space thing)
         :albedo (sampler thing)))

(defmethod draw ((thing thing))
  (map-g #'asteroid-pipeline (buf-stream thing)
         :scale (scale thing)
         :model->world (get-model->world-space thing)
         :albedo (sampler thing))
  (destructuring-bind
	(center geom stream) (gethash (resource thing) *geometry*)
    (declare (ignore center geom))
    (gl:line-width 2.0)
    (map-g #'geom-pipeline stream
					;:center center
	   :hit (if (hit thing) 1.0 0.0)
	   :model->world (get-model->world-space thing))))
