;;;; asteroids.lisp

(in-package #:asteroids)

(declaim (optimize debug))

;;; "asteroids" goes here. Hacks and glory await!

(defconstant +screen-width+  640)
(defconstant +screen-height+ 480)

(defconstant +half-screen-width+  320)
(defconstant +half-screen-height+ 240)

(defparameter *debug-out* *standard-output*)

;; (defparameter *frame-rate* 50)		;cause it's an easier number of
;; 					;milliseconds to reason about than 60.
;; (defparameter *seconds-per-frame* (/ 1.0 *frame-rate*))
;; (defparameter *ireal-time-per-frame* (/ internal-time-units-per-second *frame-rate*))

(deftype command-type ()
  `(member movement weapon))

(defstruct command time type set? content)
(defparameter *user-commands* (make-queue :simple-queue))

(defun asteroids ()
  (sdl2:with-init (:everything)
    ;; (format *debug-out* "Init Done!~%")
    ;; (force-output *debug-out*)
    (sdl2:with-window (win :title "Asteroids" :w +screen-width+ :h +screen-height+ :flags '(:shown))
      ;; (format *debug-out* "window Done!~%")
      ;; (force-output *debug-out*)
      (sdl2:with-renderer (ren win :flags '(:accelerated :presentvsync))
	;; (format *debug-out* "renderer Done!~%")
	;; (force-output *debug-out*)
	(let ((sprites (load-sprites ren))
	      (game-time (current-time)))
	  ;; (format *debug-out* "sprites loaded!~%")
	  ;; (force-output *debug-out*)
	  (initialize-game game-time)
	  ;; (format *debug-out* "game initialized!~%")
	  ;; (force-output *debug-out*)

	  ;; Avoid window/sdl window showing issues...
	  ;; https://github.com/lispgames/cl-sdl2/issues/23
	  #+win32
	  (progn
	    (sdl2:hide-window win)
	    (sdl2:show-window win))
	  
	  (sdl2:with-event-loop (:method :poll)
	    (:keydown
	     (:keysym keysym)

	     ;; Is there a better and less-verbose way to do this?
	     (let ((scancode (sdl2:scancode-value keysym))
		   (command-time (current-time)))
	       (cond
		 ;; possibly make these configurable...probably not
		 ((sdl2:scancode= scancode :scancode-w) (qpush *user-commands*
							       (make-command :time command-time
									     :type 'movement
									     :set? t
									     :content 'fwd)))
		 ((sdl2:scancode= scancode :scancode-s) (qpush *user-commands*
							       (make-command :time command-time
									     :type 'movement
									     :set? t
									     :content 'bkwd)))
		 ((sdl2:scancode= scancode :scancode-a) (qpush *user-commands*
							       (make-command :time command-time
									     :type 'movement
									     :set? t
									     :content 'ccw)))
		 ((sdl2:scancode= scancode :scancode-d) (qpush *user-commands*
							       (make-command :time command-time
									     :type 'movement
									     :set? t
									     :content 'cw)))
		 ((sdl2:scancode= scancode :scancode-space) (qpush *user-commands*
								   (make-command :time command-time
										 :type 'weapon
										 :set? t
										 :content 'fire))))))
	    (:keyup
	     (:keysym keysym)
	     (let ((scancode (sdl2:scancode-value keysym))
		   (command-time (current-time)))
	       (cond
		 ;; possibly make these configurable...probably not
		 ((sdl2:scancode= scancode :scancode-w) (qpush *user-commands*
							       (make-command :time command-time
									     :type 'movement
									     :set? nil
									     :content 'fwd)))
		 ((sdl2:scancode= scancode :scancode-s) (qpush *user-commands*
							       (make-command :time command-time
									     :type 'movement
									     :set? nil
									     :content 'bkwd)))
		 ((sdl2:scancode= scancode :scancode-a) (qpush *user-commands*
							       (make-command :time command-time
									     :type 'movement
									     :set? nil
									     :content 'ccw)))
		 ((sdl2:scancode= scancode :scancode-d) (qpush *user-commands*
							       (make-command :time command-time
									     :type 'movement
									     :set? nil
									     :content 'cw)))
		 ((sdl2:scancode= scancode :scancode-space) (qpush *user-commands*
								   (make-command :time command-time
										 :type 'weapon
										 :set? nil
										 :content 'cease-fire)))
		 ((sdl2:scancode= scancode :scancode-escape) (sdl2:push-event :quit)))))
	    (:idle
	     ()
	     ;; (format *debug-out* "idle call~%")
	     ;; (force-output *debug-out*)
	     (sdl2:render-clear ren)
	     (set-current-time game-time)
	     (tick game-time)
	     (render sprites ren game-time)
	     (sdl2:render-present ren))
	    (:quit () t)))))))
