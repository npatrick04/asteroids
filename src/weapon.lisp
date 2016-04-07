(in-package #:asteroids)
(declaim (optimize debug))

(deftype gun-command-types ()
  `(member fire cease-fire))

(defclass projectile (massy-object)
  ())

;; Maybe add explosive force
(defclass bullet (projectile)
  ())		

(defparameter *gun-reset-time* (make-game-time 0.25) "seconds between firing shots")

;;; Maybe add # of rounds, gun temperature, etc
(defclass gun ()
  ((last-fire-time :accessor last-fire-time
		   :type 'game-time
		   :initform (f- *gun-reset-time*))
   (round-type :accessor round-type
	       :initform 'bullet)))

(defgeneric fire (gun time))
(defmethod fire ((gun gun) time)
  (when (f> (f- time (last-fire-time gun))
	    *gun-reset-time*)
    (format *debug-out* "Fire w/ deltat ~A!~%"
    	       (f- time (last-fire-time gun)))
    (setf (last-fire-time gun) time)
    ;; TODO: actually do something
    ))

(defgeneric reload (gun round &key &allow-other-keys))
(defmethod reload ((gun gun) round &key)
  ;(assert (member round '(bullet) :key #'eq))
  (setf (round-type gun) round))

(defgeneric weapon-command (gun command))
(defmethod weapon-command ((gun gun) command)
  (when (eq (command-content command)
	    'fire)
    (fire gun (command-time command))))
