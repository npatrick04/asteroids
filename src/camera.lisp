(in-package :asteroids)

(defclass camera ()
  ((pos :initform (v! -0.43 25.33 43.20)
        :accessor pos)
   (rot :initform (v! 0.97 -0.20 -0.01 0.0)
        :accessor rot)
  ;; ((pos :initform (v! 0.0 0.0 50.0)
  ;; 	:accessor pos)
  ;;  (rot :initform (q:from-axis-angle
  ;;  		   (v! 1f0 0f0 0f0)
  ;;  		   (radians 0f0))
  ;;       :accessor rot)
   (near :initform 0.1f0
         :accessor near)
   (far :initform 400f0
	:accessor far)))

(defun cv (p ax ang)
  (setf (pos *camera*) p
	(rot *camera*) (q:from-axis-angle
			ax
			(radians ang))))

(defclass orthographic-camera (camera) ())

(defparameter *camera* (make-instance 'orthographic-camera))

(defmethod update ((camera camera) dt)
  (when (keyboard-button (keyboard) key.w)
    (v3:incf (pos camera)
             (v3:*s (q:to-direction (rot camera))
                    (* 10 dt))))

  (when (keyboard-button (keyboard) key.s)
    (v3:decf (pos camera)
             (v3:*s (q:to-direction (rot camera))
                    (* 10 dt))))

  (when (mouse-button (mouse) mouse.left)
    (let ((move (v2:*s (mouse-move (mouse))
                       0.03)))
      (setf (rot camera)
            (q:normalize
             (q:* (rot camera)
                  (q:normalize
                   (q:* (q:from-axis-angle
                         (v! 1 0 0) (- (y move)))
                        (q:from-axis-angle
                         (v! 0 1 0) (- (x move)))))))))))

(defun get-world->view-space (camera)
  (m4:* (q:to-mat4 (q:inverse (rot camera)))
        (m4:translation (v3:negate (pos camera)))))

(defmethod projection ((camera orthographic-camera) width height)
  (rtg-math.projection:orthographic
   width
   height
   (near camera)
   (far camera)))
