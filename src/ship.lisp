(in-package #:asteroids)
(declaim (optimize debug))

(deftype thrust-type ()
  `(member fwd bkwd ccw cw))

(defclass ship-specification ()
  ((fwd-thrust :accessor fwd-thrust
	       :initarg :fwd-thrust
	       :initform 40000)
   (bkwd-thrust :accessor bkwd-thrust
		:initarg :bkwd-thrust
		:initform -20000)
   (turn-rate :accessor turn-rate	;deg/s
	      :initarg :turn-rate
	      :initform 30.0)))
(defmethod print-object ((object ship-specification) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (fwd-thrust bkwd-thrust turn-rate) object
      (format stream "~A/~A/~A"
	      fwd-thrust
	      bkwd-thrust
	      turn-rate))))

;;; Commands are added to a queue with their timestamp for later
;;; processing by TICK. 
(defclass ship (massy-object)
  ((specs :accessor ship-specs
	  :initarg :ship-specs
	  :initform (make-instance 'ship-specification))
   (thrust :accessor thrust
	   :initform 0.0)
   (thrust-command :accessor thrust-command ;The user command direction
                   :initform ())            ;Commands are a list of all active things.
   (command-start-time :accessor command-start-time
                       :initform 0)
   (gun :accessor gun
        :initform (make-instance 'gun))))

(defmethod print-object ((object ship) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~A @ ~A deg"
	    (pos object)
	    (attitude object))))

(defmethod weapon-command ((ship ship) command)
  (weapon-command (gun ship) command))

(defmethod propel ((ship ship) thrust deltat)
  "Handle the attitude change over deltat, returning the new position,
  velocity, and attitude values."
  (declare (optimize debug))
  (let (new-attitude delta-attitude avg-attitude-r)
    (if (zero-ish (attitude-rate ship))
	(setf delta-attitude 0.0
	      avg-attitude-r (attitude ship)
	      new-attitude (attitude ship))
	(setf delta-attitude (normalize
			      (* (attitude-rate ship) deltat))
	      avg-attitude-r (deg->rad
			      (normalize
			       (+ (/ delta-attitude 2.0)
				  (attitude ship))))
	      new-attitude (normalize (+ (attitude ship)
					 delta-attitude))))

    ;; Then apply whatever the thrust is on the object in the direction
    ;; of half of delta-attitude from the current attitude.
    ;; First, find the x and y components of thrust.
    (let ((thrust-components
	   (make-vec2 :x (coerce (* thrust (cos avg-attitude-r)) 'single-float)
		      :y (coerce (* thrust (sin avg-attitude-r)) 'single-float))))
      (let* ((accel (vec2/ thrust-components (mass ship)))
	     (vel (vec2+ (vel ship) (vec2* accel deltat))))
	(values (vec2+
		 (vec2+ (pos ship) ;integral of velocity/accel over deltat
			(vec2* (vel ship) deltat))
		 (vec2* accel (* deltat deltat 0.5)))
		vel
		new-attitude)))))

(defgeneric movement-command (ship command))
(defmethod movement-command ((ship ship) command)
  (let* ((deltat-sec (game-time (f- (command-time command)
				    (timestamp ship))))
	 command-change)
    
    ;; Figure out what the total command is
    (case (command-content command)
      ((fwd bkwd ccw cw)
       (setf command-change
	     (if (command-set? command)
		 (unless
		     (member (command-content command)
			     (thrust-command ship))
		   (push (command-content command)
			 (thrust-command ship)))
		 (when (member (command-content command)
			       (thrust-command ship))
		   (setf (thrust-command ship)
			 (remove (command-content command)
				 (thrust-command ship)))
		   ;; Remove t so that an emtpy list isn't ignored
		   t)))))
    (when command-change
      ;; (format *debug-out* "~:[Add~;Delete~] command ~A => ~A~%"
      ;; 	       (not (command-set? command))
      ;; 	       (command-content command)
      ;; 	       (thrust-command ship))

      ;; First set up the ship internal state to represent where we are right now.
      (multiple-value-bind
	    (pos vel att) (propel ship (thrust ship) (float deltat-sec))
	(setf (pos ship) (vec2mod2 pos +screen-vec2+)
	      (vel ship) vel
	      (attitude ship) att
	      (timestamp ship) (command-time command)))

      ;; Then apply whatever movement offset to the rates.
      ;; Forward thrust
      (with-accessors ((spec ship-specs)
		       (thrust thrust)
		       (thrust-command thrust-command)
		       (rate attitude-rate)) ship
	(setf thrust 
	      (if (member 'fwd thrust-command)
		  (if (member 'bkwd thrust-command)
		      0.0			;cancel each other out
		      (fwd-thrust spec))
		  (if (member 'bkwd thrust-command)
		      (bkwd-thrust spec)
		      0.0)))
	(setf rate
	      (if (member 'cw thrust-command)
		  (if (member 'ccw thrust-command)
		      0.0			;cancel each other out
		      (turn-rate spec))
		  (if (member 'ccw thrust-command)
		      (- (turn-rate spec))
		      0.0)))
	;; (format *debug-out* "Thrust command: ~A~%Attitude Rate: ~A~%Ship: ~A~%"
	;; 	      thrust
	;; 	      rate
	;; 	      ship)
	))))
