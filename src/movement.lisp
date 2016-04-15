(in-package #:asteroids)
(declaim (optimize debug))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct vec2
    (x 0.0f0 :type single-float)
    (y 0.0f0 :type single-float)))
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

(define-constant +full-circle+ 360.0)
(define-constant +screen-vec2+ (make-vec2 :x (float +screen-width+)
					  :y (float +screen-height+))
  :test #'equalp)

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
	      :type 'real-time
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

