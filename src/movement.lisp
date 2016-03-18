(in-package #:asteroids)

(defparameter *turn-rate*  180.0)	;deg/s
(defparameter *accel*      10.0)	;screen coordinates...so pixels/sec/sec?

(defstruct vec2
  (x 0.0 :type single-float)
  (y 0.0 :type single-float))
(defun vec2* (a s)
  "vector times scalar"
  (make-vec2 :x (* (vec2-x a) s)
	     :y (* (vec2-y a) s)))
(defun vec2/ (a s)
  "vector divided by scalar"
  (make-vec2 :x (/ (vec2-x a) s)
	     :y (/ (vec2-y a) s)))
(defun vec2+ (a b)
  "vector + vector"
  (make-vec2 :x (+ (vec2-x a) (vec2-x b))
	     :y (+ (vec2-y a) (vec2-y b))))
(defun vec2- (a b)
  "vector - vector"
  (make-vec2 :x (- (vec2-x a) (vec2-x b))
	     :y (- (vec2-y a) (vec2-y b))))

(defconstant +full-circle+ 365.0)

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
	 :initform 1.0
	 :type 'single-float
	 :initarg :mass)))

(defgeneric move (object deltat))
(defmethod move ((object moving-object) deltat)
  (with-slots (pos vel attitude attitude-rate) object
    (setf pos (vec2+ pos (vec2* vel deltat))

          attitude (mod (+ attitude attitude-rate)
                        +full-circle+))))

(defgeneric force (object force deltat))
(defmethod force ((object massy-object) (force vec2) deltat)
  (let ((accel (vec2/ force (mass object))))
    (setf (vel object) (vec2+ (vel object) (vec2* accel deltat)))))

(deftype gun-command-types ()
  `(member fire cease-fire))

(defclass projectile (massy-object)
  ())

;; Maybe add explosive force
(defclass bullet (projectile)
  ())		

(defparameter *gun-reset-time* 0.5 "seconds between firing shots")

;;; Maybe add # of rounds, gun temperature, etc
(defclass gun ()
  ((last-fire-time :accessor last-fire-time
		   :initform (- *gun-reset-time*))
   (round-type :accessor round-type
	       :initform 'bullet)))

(defgeneric fire (gun time))
(defmethod fire ((gun gun) time)
  (setf (last-fire-time gun) time))

(defgeneric reload (gun round &key &allow-other-keys))
(defmethod reload ((gun gun) round &key)
  (assert (member round '(bullet) :key #'eq))
  (setf (round-type gun) round))

(deftype thrust-type ()
  `(member fwd bkwd ccw cw coast))

;;; Commands are added to a queue with their timestamp for later
;;; processing by TICK. 
(defclass ship (massy-object)
  ((thrust-command :accessor thrust-command ;The user command direction
                   :initform nil
                   :type (or nil thrust-type))
   (command-start-time :accessor command-start-time
                       :initform 0)
   (gun :accessor gun
        :initform (make-instance 'gun))))
