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

(defclass moving-object ()
  ((pos :accessor pos
	:initform (make-vec2)
	:type 'vec2
	:initarg :pos)
   (vel :accessor vel
	:initform (make-vec2)
	:type 'vec2
	:initarg :vel)
   (attitude :accessor attitude
	     :initform 0.0
	     :type 'single-float
	     :initarg :attitude)))

(defclass massy-object (moving-object)
  ((mass :accessor mass
	 :initform 1.0
	 :type 'single-float
	 :initarg :mass)))

(defgeneric move (object deltat))
(defgeneric force (object force deltat))

(defmethod move ((object moving-object) deltat)
  (setf (pos object) (vec2+ (pos object)
			    (vec2* (vel object) deltat))))

(defmethod force ((object massy-object) (force vec2) deltat)
  (let ((accel (vec2/ force (mass object))))
    (setf (vel object) (vec2+ (vel object) (vec2* accel deltat)))))

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

(defgeneric load (gun round &key &allow-other-keys))
(defmethod load ((gun gun) (round :eql bullet) &key)
  (setf (round-type gun 'bullet)))

(defclass ship (massy-object)
  ())
