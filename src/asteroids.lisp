(in-package :asteroids)

(defvar *asteroids* nil)

(defclass asteroid (massy-object)
  ())

(defun rpick (list)
  "Pick an element from the list with random."
  (elt list (random (length list))))

(defun pick-asteroid (size)
  (ecase size
    (:big (rpick *big-asteroids*))
    (:medium (rpick *medium-asteroids*))
    (:small (rpick *small-asteroids*))
    (:any (pick-asteroid (rpick '(:big :medium :small))))))

(defparameter *min-initial-asteroid-dist* 500.0)
(defparameter *initial-asteroid-dist-range* 1000.0)
(defparameter *initial-asteroid-vel-range* 100.0)
(defparameter *asteroid-rate-limit* 1700.0)

(defun make-asteroid (&optional (size :big))
  (let* ((specific-asteroid (pick-asteroid size))
	 (asteroid (make-instance 'asteroid
				  :resource specific-asteroid
				  :stream (gethash specific-asteroid
						   *sprites*)
				  :sampler *sampler*
				  :timestamp *game-time*))
	 (angle (random 2pi))
	 (radius (+ *min-initial-asteroid-dist*
		    (random *initial-asteroid-dist-range*)))
	 (attitude-rate (- *asteroid-rate-limit*
			   (* 2 (random *asteroid-rate-limit*)))))
    (setf (pos asteroid)
	  (v:+ (center-screen)
	       (v! (* radius (cos angle))
		   (* radius (sin angle))))

	  (vel asteroid)
	  (v! (- (/ *initial-asteroid-vel-range* 2.0)
		 (random *initial-asteroid-vel-range*))
	      (- (/ *initial-asteroid-vel-range* 2.0)
		 (random *initial-asteroid-vel-range*)))

	  (attitude asteroid) (random +full-circle+)

	  (attitude-rate asteroid) (radians attitude-rate))    
    
    (add-object asteroid)
    (push asteroid *asteroids*)
    asteroid))


(defun make-asteroids (&optional (number 10))
  (loop for i below number do
       (make-asteroid :any)))
