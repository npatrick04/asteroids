;;;; asteroids.lisp

(in-package #:asteroids)

(declaim (optimize debug))

(defparameter *debug-out* *standard-output*)

;;; Some value to be updated each frame
(defvar *screen-dimensions* (v! 240.0f0 480.0f0))
(defparameter *blending* (make-blending-params))

;;; a fixed point real-time type, because i can
(defvar *game-time* (current-time))

(defparameter *time-running* t)

(deftype command-type ()
  `(member movement weapon))

(defstruct command time type set? content)

(defparameter *user-commands* (make-queue 32))

(defun initialize-game ()
  (setf *screen-dimensions* (viewport-resolution
			     (current-viewport))
	*game-time* (current-time)
	*user-commands* (make-queue 32))

  (load-sprites)
  (fill-geometry)

  ;; Refresh the system!
  (reset)

  ;; Maybe give time
  (make-ship))

(defun reset ()
  (setf *game-objects* nil
	*asteroids* nil
	*bullets* nil
	*time-running* t)

  (make-ship)
  (make-asteroids))

(defun tick (delta-time)
  (step-host)
  (update-repl-link)

  (clear)

  (handle-user-commands)

  ;; Disable depth test as a hack to get around
  ;; blending requiring depths.  There is no depth
  ;; in this game!
  (gl:disable :depth-test)

  (with-blending *blending*
    (loop :for object in *game-objects* :do
       (update object delta-time)
       (draw-wrapping object)))

  (when (zerop (length *asteroids*))
    (format t "~&You Won!~%")
    (reset))

  (swap)
  (decay-events))

(defun run-step ()
  (if *time-running*
      (let* ((next-time (current-time))
	     (delta-time (f- next-time *game-time*)))
	(setf *game-time* next-time)
	(tick delta-time))
      (tick (make-real-time 0))))

(def-simple-main-loop asteroids (:on-start #'initialize-game)
  (run-step))
