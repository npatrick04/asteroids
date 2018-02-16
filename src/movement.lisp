(in-package #:asteroids)
(declaim (optimize debug))

(defparameter *screen-scale* 10.0)
(define-constant +full-circle+ 360f0)

(defun vmod (vec mvec-or-scalar)
  "Mod each element of vec by each element of mvec or scalar"
  (let ((mvec (if (vectorp mvec-or-scalar)
		  mvec-or-scalar
		  (make-array (dimensions vec)
			      :initial-element mvec-or-scalar))))
    (map 'vector #'mod vec mvec)))


(defun screen-extents ()
  "A vector of x and y size."
  (v:* (viewport-resolution
	(current-viewport))
       *screen-scale*))

(defun center-screen ()
  "Return the center-point of the screen in world coordinates"
  (v:/ (screen-extents) 2f0))

(defclass moving-object (thing)
  ((pos :accessor pos		       ;Meters
	:initform (v! 0f0 0f0)	       ;#(0 0) is center of the screen
	:initarg :pos)
   (vel :accessor vel	       ;Meters per second
	:initform (v! 0f0 0f0) ;Same directional orientation as position
	:initarg :vel)
   (accel :accessor accel
	  :initform 0f0
	  :initarg :accel)
   (attitude :accessor attitude         ;Degrees
	     :initform 0f0     ;Attitude is +degrees counter-clockwise
	     :initarg :attitude)
   (attitude-rate :accessor attitude-rate ;Degrees per second
                  :initform 0f0		  ;Same convention as attitude
                  :initarg :attitude-rate)
   (timestamp :accessor timestamp       ;The valid time of the object
	      :type 'real-time
              :initarg :timestamp)
   (resource :accessor resource
             :initform (error "Objects must be created with resources")
             :initarg :resource)))

;;; Update the quaternion thingies.
(defmethod draw :before ((obj moving-object))
  (declare (optimize debug))
  ;; Add in the rotation of the object in degrees, and attitude-rate since timestamp.
  ;(format t "foo")
  (with-slots (pos vel accel attitude attitude-rate timestamp)
      obj
    (let* ((delta-time (real-time (f- *game-time* timestamp)))
	   (delta-attitude (* attitude-rate delta-time))
	   (total-attitude (+ attitude delta-attitude))
	   (attitude-rad (float (deg->rad total-attitude) 0f0))
	   (accel-vec (v:* (v! (cos attitude-rad)
			       (sin attitude-rad))
			   accel))
	   (delta-pos (v:+ (v:* vel delta-time)
			   (v:* accel-vec
				(* 1/2 delta-time delta-time))))
	   (total-pos (v:+ pos delta-pos)))
      (setf (rotv obj) (q:from-axis-angle
			(v! 0.0 0.0 1.0)
			(float (deg->rad attitude) 0f0)
			;; (float (deg->rad total-attitude) 0f0)
			)
	    (posv obj) (v! pos 0.0)
	    ;; (v! total-pos 0.0)
	    ))))


;;; --------------------------------------------

(defclass massy-object (moving-object)
  ((mass :accessor mass
	 :initform 100.0		;what is a good unit...tons?
	 :type 'single-float
	 :initarg :mass)))

(defun propagate-coasting-object (object deltat)
  "Calculate new position and attitude given a deltat and constant rates."
  (with-slots (pos vel attitude attitude-rate) object
    (values (v:+ pos (v:* vel deltat))
	    (mod (+ attitude attitude-rate)
		 +full-circle+))))

(defgeneric move (object deltat)
  (:documentation "Move an object with constant rates for deltat seconds."))
(defmethod move ((object moving-object) deltat)
  (with-slots (pos vel attitude attitude-rate) object
    (setf pos (vmod (v:+ pos (v:* vel deltat)) (screen-extents))
          attitude (mod (+ attitude
			   (* attitude-rate deltat))
                        +full-circle+))))

(defmethod update ((object massy-object) delta-time)
  (let ((dt (real-time delta-time)))
    (move object dt)))

;;; Given a float...it's not optimal for all floats, but works for
;;; this use case.  
(defun zero-ish (float)
  (< (abs float) 0.001))

#+nil(defun normalize (angle &optional (range +full-circle+))
  (mod angle range))

(defgeneric force (object force deltat))
(defmethod force (object force deltat)
  )

(defgeneric propel (object deltat))

(defun deg->rad (deg)
  (* deg (/ pi 180.0)))
(defun rad->deg (rad)
  (* rad (/ 180.0 pi)))

