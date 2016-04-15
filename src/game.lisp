(in-package #:asteroids)

;;; The main game logic happens here

(defparameter *game-objects* ())
(defparameter *the-ship* ())

(defun add-object (object)
  (push object *game-objects*))

(defun initialize-game (time)
  (declare (type real-time time))
  ;; Refresh the system!
  (setf *game-objects* ())

  (setf *the-ship* (make-instance 'ship
				  :resource :ship
				  :timestamp time))
  (add-object *the-ship*)

  ;; TODO: initialize some asteroids
  )

(defun ship-commands ()
  ;; Recursively perform ship command until their done.
  (multiple-value-bind
	(command present) (qpop *user-commands*)
    (when present
      (case (command-type command)
	(weapon (weapon-command *the-ship* command))
	(movement (movement-command *the-ship* command))))))


(defun tick (time)
  ;; TODO: Detect collisions

  ;; Handle ship commands
  (ship-commands))
