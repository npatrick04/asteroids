(in-package #:asteroids)
(declaim (optimize debug))

(defstruct vec2
  (x 0.0f0 :type single-float)
  (y 0.0f0 :type single-float))
(defmacro vec2-fn-s (fn &optional name)
  "vector operation with scalar"
  (let ((name (if name name
		  (intern (concatenate 'string
				       "VEC2"
				       (string-upcase (symbol-name fn)))))))
    `(defun ,name (a s)
       (make-vec2 :x (,fn (vec2-x a) s)
		  :y (,fn (vec2-y a) s)))))

(defmacro vec2-fn (fn &optional name)
  "vector operation"
  (let ((name (if name name
		  (intern (concatenate 'string
				       "VEC2"
				       (string-upcase (symbol-name fn)))))))
    `(defun ,name (a b)
       (make-vec2 :x (,fn (vec2-x a) (vec2-x b))
		  :y (,fn (vec2-y a) (vec2-y b))))))
(vec2-fn-s *)
(vec2-fn-s /)
(vec2-fn   +)
(vec2-fn   -)
(vec2-fn   mod vec2mod2)
(vec2-fn-s mod)

(defconstant +full-circle+ 360.0)
(defconstant +screen-vec2+ (make-vec2 :x (float +screen-width+)
				      :y (float +screen-height+)))

(defclass moving-object ()
  ((pos :accessor pos                   ;Meters
	:initform (make-vec2)           ;#(0 0) is center of the screen
	:type 'vec2                     ;#(+ +) is up and right
	:initarg :pos)
   (vel :accessor vel                   ;Meters per second
	:initform (make-vec2)           ;Same directional orientation as position
	:type 'vec2
	:initarg :vel)
   (attitude :accessor attitude         ;Degrees
	     :initform 0.0              ;Attitude is +degrees counter-clockwise
	     :type 'single-float        ;Zero to the right
	     :initarg :attitude)
   (attitude-rate :accessor attitude-rate  ;Degrees per second
                  :initform 0.0            ;Same convention as attitude
                  :type 'single-float
                  :initarg :attitude)
   (timestamp :accessor timestamp       ;The valid time of the object
              :initarg :timestamp)
   (resource :accessor resource
             :initform (error "Objects must be created with resources")
             :initarg :resource)))

(defclass massy-object (moving-object)
  ((mass :accessor mass
	 :initform 100.0		;what is a good unit...tons?
	 :type 'single-float
	 :initarg :mass)))

(defun propagate-coasting-object (object deltat)
  "Calculate new position and attitude given a deltat and constant rates."
  (with-slots (pos vel attitude attitude-rate) object
    (values (vec2+ pos (vec2* vel deltat))
	    (mod (+ attitude attitude-rate)
		 +full-circle+))))

(defgeneric move (object deltat)
  (:documentation "Move an object with constant rates for deltat seconds."))
(defmethod move ((object moving-object) deltat)
  (with-slots (pos vel attitude attitude-rate) object
    (setf pos (vec2+ pos (vec2* vel deltat))
          attitude (mod (+ attitude attitude-rate)
                        +full-circle+))))

;;; Given a float...it's not optimal for all floats, but works for
;;; this use case.  
(defun zero-ish (float)
  (< (abs float) 0.001))

(defun normalize (angle &optional (range +full-circle+))
  (mod angle range))

(defgeneric force (object force deltat))
(defmethod force (object force deltat)
  )

(defgeneric propel (object force deltat))

(defun deg->rad (deg)
  (* deg (/ pi 180.0)))
(defun rad->deg (rad)
  (* rad (/ 180.0 pi)))

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
		 (vec2* accel (* deltat deltat)))
		vel
		new-attitude)))))



(deftype gun-command-types ()
  `(member fire cease-fire))

(defclass projectile (massy-object)
  ())

;; Maybe add explosive force
(defclass bullet (projectile)
  ())		

(defparameter *gun-reset-time* 0.25 "seconds between firing shots")

;;; Maybe add # of rounds, gun temperature, etc
(defclass gun ()
  ((last-fire-time :accessor last-fire-time
		   :initform (- *gun-reset-time*))
   (round-type :accessor round-type
	       :initform 'bullet)))

(defgeneric fire (gun time))
(defmethod fire ((gun gun) time)
  (let ((reset-diff (* *gun-reset-time*
		       internal-time-units-per-second)))
    (when (> (- time (last-fire-time gun))
	     reset-diff)
      ;; (format *debug-out* "Fire w/ deltat ~D!~%"
      ;; 	      (- time (last-fire-time gun)))
      (setf (last-fire-time gun) time)
      ;; TODO: actually do something
      )))

(defgeneric reload (gun round &key &allow-other-keys))
(defmethod reload ((gun gun) round &key)
  ;(assert (member round '(bullet) :key #'eq))
  (setf (round-type gun) round))

(defgeneric weapon-command (gun command))
(defmethod weapon-command ((gun gun) command)
  (when (eq (command-content command)
	    'fire)
    (fire gun (command-time command))))

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
	      :initform 5.0)))
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

(defgeneric movement-command (ship command))
(defmethod movement-command ((ship ship) command)
  (let* ((deltat (- (command-time command)
		    (timestamp ship)))
	 (deltat-sec (/ deltat internal-time-units-per-second)))
    ;; First set up the ship internal state to represent where we are right now.
    (multiple-value-bind
	  (pos vel att) (propel ship (thrust ship) (float deltat-sec))
      (setf (pos ship) (vec2mod2 pos
				 +screen-vec2+)
	    (vel ship) vel
	    (attitude ship) att
	    (timestamp ship) (command-time command)))

    ;; Figure out what the total command is
    (case (command-content command)
      ((fwd bkwd ccw cw)
       (if (command-set? command)
	   (pushnew (command-content command)
		    (thrust-command ship))
	   (setf (thrust-command ship)
		 (remove (command-content command)
			 (thrust-command ship))))))
    ;; (format *debug-out* "~:[Add~;Delete~] command ~A => ~A~%"
    ;; 	       (not (command-set? command))
    ;; 	       (command-content command)
    ;; 	       (thrust-command ship))

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
      )))
