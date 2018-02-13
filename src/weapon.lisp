(in-package #:asteroids)
(declaim (optimize debug))

(deftype gun-command-types ()
  `(member fire cease-fire))

(defclass projectile (massy-object)
  ())

(defparameter *bullet-velocity* 1000.0)
(defvar *bullets* nil)

;; Maybe add explosive force
(defclass bullet (projectile)
  ())

(defparameter *max-bullet-age* (make-real-time 2.0))
(defparameter *expire-bullets* t)

(defun expire-bullet (bullet)
  (setf *bullets* (remove bullet *bullets*)
	*game-objects* (remove bullet *game-objects*)))

(defmethod update :around ((bullet bullet) delta-time)
  (if (and *expire-bullets*
	   (f> (f- *game-time* (timestamp bullet))
	       *max-bullet-age*))
      (expire-bullet bullet)
      (progn
	(call-next-method)
	;(format t "Check collisions...~%")
	(let ((collision (collision bullet *asteroids*)))
	  (when collision
	    #+nil(format t "Bullet ~A collided with asteroid ~A~%"
			 bullet collision)
	    ;;(setf *time-running* nil)
	    (setf (hit collision) t)
	    (expire-bullet bullet)
	    (setf *asteroids* (remove collision *asteroids*))
	    (setf *game-objects* (remove collision *game-objects*))
	    )))))

(defmethod draw ((thing bullet))
  (map-g #'bullet-pipeline (buf-stream thing)
         :scale (scale thing)
         :model->world (get-model->world-space thing)))

(defun make-bullet (pos vel attitude)
  (let ((bullet (make-instance 'bullet
			       :resource nil
			       :stream (box 20.0 10.0)
			       ;; :stream (box 0.1 0.1)
			       :pos pos
			       :attitude attitude
			       ;:attitude-rate 0.0
			       :timestamp *game-time*))
	(total-velocity
	 (v:+ vel
	      (v! (* *bullet-velocity* (cos (radians attitude)))
		  (* *bullet-velocity* (sin (radians attitude)))))))
    (setf (vel bullet) total-velocity)
    (push bullet *bullets*)
    (add-object bullet)
    bullet))

(defparameter *gun-reset-time* (make-real-time 0.35)
  "seconds between firing shots")

;;; Maybe add # of rounds, gun temperature, etc
(defclass gun ()
  ((last-fire-time :accessor last-fire-time
		   :type 'real-time
		   :initform (f- *gun-reset-time*))
   (status :accessor gun-status
	   :initform 'cease-fire)
   (round-type :accessor round-type
	       :initform 'bullet)))

(defgeneric fire (gun time command))
(defmethod fire ((gun gun) time command)
  (ecase command
    (fire
     (when (f>= (f- time (last-fire-time gun))
		*gun-reset-time*)
       #+nil(format *debug-out* "Fire w/ deltat ~A!~%"
	       (f- time (last-fire-time gun)))
       (force-output *debug-out*)
       (setf (last-fire-time gun) time)
       (make-bullet (pos *ship*)
		    (vel *ship*)
		    (attitude *ship*))))
    (cease-fire
     ))
    (setf (gun-status gun) command))

(defgeneric reload (gun round &key &allow-other-keys))
(defmethod reload ((gun gun) round &key)
  ;(assert (member round '(bullet) :key #'eq))
  (setf (round-type gun) round))

(defgeneric weapon-command (gun command))
(defmethod weapon-command ((gun gun) command)
  (fire gun (command-time command) (command-content command)))

