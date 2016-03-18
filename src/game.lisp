(in-package #:asteroids)

;;; The main game logic happens here

(defparameter *game-objects* ())

(defun add-object (object)
  (push object *game-objects*))

(defun tick (deltat)
  ;; TODO: Detect collisions

  ;; TODO: Process user commands
  )
