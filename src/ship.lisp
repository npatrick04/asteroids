(in-package #:asteroids)
(declaim (optimize debug))

(deftype thrust-type ()
  `(member fwd bkwd ccw cw))

(defclass ship-specification ()
  ((fwd-thrust :accessor fwd-thrust
	       :initarg :fwd-thrust
	       :initform 800.0f0)
   (bkwd-thrust :accessor bkwd-thrust
		:initarg :bkwd-thrust
		:initform 400.0f0)
   (turn-rate :accessor turn-rate	;deg/s
	      :initarg :turn-rate
	      :initform 300.0f0)))

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
   (thrust-command :accessor thrust-command ;The user command direction
                   :initform ())            ;Commands are a list of all active things.
   ;; (command-start-time :accessor command-start-time
   ;;                     :initform 0)
   (gun :accessor gun
        :initform (make-instance 'gun))))

(defun update-ship-spec (spec value)
  (setf (slot-value (ship-specs *ship*) spec)
	(coerce value 'float)))

(defmethod print-object ((object ship) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~A @ ~A deg"
	    (pos object)
	    (attitude object))))

(defvar *ship* nil)

(defun make-ship ()
  (let ((ship (make-instance 'ship 
			     :resource :ship
			     :stream (gethash :ship *sprites*)
			     :sampler *sampler*
			     :timestamp *game-time*)))
    (setf (pos ship) (center-screen))
    
    ;; Add the new ship to game-objects with old ship removed.
    (setf *game-objects* (delete *ship* *game-objects*)
	  *ship* ship)
    (add-object ship)
    ship))

;;; Weapon stuffs

(defmethod weapon-command ((ship ship) command)
  (weapon-command (gun ship) command))

(defun manage-weapons (ship)
  (case (gun-status (gun *ship*))
    (fire (fire (gun ship) *game-time* 'fire))))

;;; Movement

(defun stopping (ship)
  (member 'stop (thrust-command ship)))

(defparameter *stopping-rate* 2.0)

(defmethod propel ((ship ship) deltat)
  "Handle the attitude change over deltat, returning the new position,
  velocity, and attitude values."
  (declare (optimize debug))
  (let* ((delta-attitude (* (attitude-rate ship) deltat))
	 (new-attitude (mod (+ (attitude ship) delta-attitude)
			    +full-circle+))
	 (new-attitude-rads (radians new-attitude)))

    ;; Then apply whatever the accel is on the object in the direction it's facing
    ;; First, find the x and y components of thrust.
    (let ((thrust-components
	   (v! (* (accel ship) (cos new-attitude-rads))
	       (* (accel ship) (sin new-attitude-rads)))))
      (let* ((accel thrust-components) ;;(v:/ thrust-components (mass ship))
	     (new-vel (v:+ (vel ship) (v:* accel deltat)))
	     (delta-pos (v:* (vel ship) deltat))
	     (new-pos (v:+ (pos ship) delta-pos))) ;and this? (v:* accel (* deltat deltat 0.5f0))

	(when (stopping ship)
	  (setf new-vel
		(v:- new-vel
		     (v:* new-vel (* *stopping-rate* deltat)))))
	
	(values (vmod new-pos (screen-extents))
	        new-vel
		new-attitude)))))


(defgeneric movement-command (ship command))
(defmethod movement-command ((ship ship) command)
  (let (;; (deltat-sec (real-time (f- (command-time command)
	;; 			   (timestamp ship))))
	command-change)
    
    ;; Figure out what the total command is
    (case (command-content command)
      ((fwd bkwd ccw cw stop)
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

    ;; Regardless of fwd or bkwd, remove them if STOP is set!
    (when (member 'stop (thrust-command ship))
      (setf (thrust-command ship)
	    (set-difference (thrust-command ship)
			    '(fwd bkwd))))
    
    (when command-change
					
      #+ (or) (format *debug-out* "~:[Add~;Delete~] command ~A => ~A~%"
		    (not (command-set? command))
		    (command-content command)
		    (thrust-command ship))

      ;; Then apply whatever movement offset to the rates.
      ;; Forward thrust
      (with-accessors ((spec ship-specs)
		       (accel accel)
		       (thrust-command thrust-command)
		       (rate attitude-rate)) ship
	(setf accel 
	      (if (member 'fwd thrust-command)
		  (if (member 'bkwd thrust-command)
		      0.0			;cancel each other out
		      (fwd-thrust spec))
		  (if (member 'bkwd thrust-command)
		      (- (abs (bkwd-thrust spec)))
		      0.0)))
	(setf rate
	      (if (member 'cw thrust-command)
		  (if (member 'ccw thrust-command)
		      0.0			;cancel each other out
		      (- (turn-rate spec)))
		  (if (member 'ccw thrust-command)
		      (turn-rate spec)
		      0.0)))
	;; (format *debug-out* "Thrust command: ~A~%Attitude Rate: ~A~%Ship: ~A~%"
	;; 	      accel
	;; 	      rate
	;; 	      ship)
	))))

(defun ship-commands ()
  ;; Recursively perform ship command until their done.
  (loop :until (queue-empty-p *user-commands*)
     :do
     (let ((command (dequeue *user-commands*)))
       ;; (print command)
       (case (command-type command)
	 (weapon (weapon-command *ship* command))
	 (movement (movement-command *ship* command))))))


(defmethod update ((ship ship) delta-time)
  #+ (or)
  (let ((the-time (real-time *game-time*)))
    ;; circle!
    (setf (pos ship)
	  (v:*
	   (v! (sin (* 2.0f0 the-time))
	       (cos (* 2.0f0 the-time)))
	   1000.0f0)))
  ;(incf (attitude ship) 0.1)

  ;; First set up the ship internal state to represent where we are right now.
  (let ((dt (real-time delta-time)))
    (when (plusp dt)
      (multiple-value-bind
	    (pos vel att) (propel ship dt)
	(setf (pos ship) pos
	      (vel ship) vel
	      (attitude ship) att
	      (timestamp ship) *game-time*))))

  (ship-commands)

  (manage-weapons ship))

(defvar key-states (make-array 256 :element-type 'bit))

(defun key-edge-p (key)
  "return :down for key-down edge and :up for key-up edge"
  (declare (type (integer 0 255) key))
  (if (key-down-p key)
      (when (zerop (elt key-states key))
	(setf (elt key-states key) 1)
	:down)
      (when (plusp (elt key-states key))
	(setf (elt key-states key) 0)
	:up)))

;; TODO: make this simpler!
;;(defmacro key-cmd (key))

(defun handle-user-commands ()
  (let ((command-time (current-time)))
    (case (key-edge-p key.w)
      (:down (enqueue (make-command :time command-time
				    :type 'movement
				    :set? t
				    :content 'fwd)
		      *user-commands*))
      (:up (enqueue (make-command :time command-time
				  :type 'movement
				  :set? nil
				  :content 'fwd)
		    *user-commands*)))

    (case (key-edge-p key.s)
      (:down (enqueue (make-command :time command-time
				    :type 'movement
				    :set? t
				    :content 'bkwd)
		      *user-commands*))
      (:up (enqueue (make-command :time command-time
				  :type 'movement
				  :set? nil
				  :content 'bkwd)
		    *user-commands*)))

    (case (key-edge-p key.a)
      (:down (enqueue (make-command :time command-time
				    :type 'movement
				    :set? t
				    :content 'ccw)
		      *user-commands*))
      (:up (enqueue (make-command :time command-time
				  :type 'movement
				  :set? nil
				  :content 'ccw)
		    *user-commands*)))

    (case (key-edge-p key.d)
      (:down (enqueue (make-command :time command-time
				    :type 'movement
				    :set? t
				    :content 'cw)
		      *user-commands*))
      (:up (enqueue (make-command :time command-time
				  :type 'movement
				  :set? nil
				  :content 'cw)
		    *user-commands*)))
    (case (key-edge-p key.z)
      (:down (enqueue (make-command :time command-time
				    :type 'movement
				    :set? t
				    :content 'stop)
		      *user-commands*))
      (:up (enqueue (make-command :time command-time
				  :type 'movement
				  :set? nil
				  :content 'stop)
		      *user-commands*)))
    (case (key-edge-p key.space)
      (:down (enqueue (make-command :time command-time
				    :type 'weapon
				    :set? t
				    :content 'fire)
		      *user-commands*))
      (:up (enqueue (make-command :time command-time
				  :type 'weapon
				  :set? t
				  :content 'cease-fire)
		    *user-commands*)))

    (when (eq (key-edge-p key.g) :down)
      (setf *draw-geometry* (not *draw-geometry*)))
    (when (eq (key-edge-p key.escape) :down)
      (format t "Figure out how to quit!~%")
      ;; maybe (sdl2:push-event :quit)
      )))

