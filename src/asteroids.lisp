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
    (sdl2:with-window (win :title "Asteroids" :w +screen-width+ :h +screen-height+ :flags '(:shown))
      (sdl2:with-renderer (ren win :flags '(:accelerated :presentvsync))
	(let ((sprites (load-sprites ren))
	      (time (get-internal-real-time)))
	  (initialize-game time)
	  
	  (sdl2:with-event-loop (:method :poll)
	    (:keydown
	     (:keysym keysym)

	     ;; Is there a better and less-verbose way to do this?
	     (let ((scancode (sdl2:scancode-value keysym))
                   (the-time (get-internal-real-time)))
               (cond
                 ;; possibly make these configurable...probably not
                 ((sdl2:scancode= scancode :scancode-w) (qpush *user-commands*
                                                               (make-command :time the-time
                                                                             :type 'movement
									     :set? t
                                                                             :content 'fwd)))
                 ((sdl2:scancode= scancode :scancode-s) (qpush *user-commands*
                                                               (make-command :time the-time
                                                                             :type 'movement
									     :set? t
                                                                             :content 'bkwd)))
                 ((sdl2:scancode= scancode :scancode-a) (qpush *user-commands*
                                                               (make-command :time the-time
                                                                             :type 'movement
									     :set? t
                                                                             :content 'ccw)))
                 ((sdl2:scancode= scancode :scancode-d) (qpush *user-commands*
                                                               (make-command :time the-time
                                                                             :type 'movement
									     :set? t
                                                                             :content 'cw)))
                 ((sdl2:scancode= scancode :scancode-space) (qpush *user-commands*
                                                                   (make-command :time the-time
                                                                                 :type 'weapon
										 :set? t
                                                                                 :content 'fire))))))
	    (:keyup
             (:keysym keysym)
	     (let ((scancode (sdl2:scancode-value keysym))
                   (the-time (get-internal-real-time)))
               (cond
                 ;; possibly make these configurable...probably not
                 ((sdl2:scancode= scancode :scancode-w) (qpush *user-commands*
                                                               (make-command :time the-time
                                                                             :type 'movement
									     :set? nil
                                                                             :content 'fwd)))
                 ((sdl2:scancode= scancode :scancode-s) (qpush *user-commands*
                                                               (make-command :time the-time
                                                                             :type 'movement
									     :set? nil
                                                                             :content 'bkwd)))
                 ((sdl2:scancode= scancode :scancode-a) (qpush *user-commands*
                                                               (make-command :time the-time
                                                                             :type 'movement
									     :set? nil
                                                                             :content 'ccw)))
                 ((sdl2:scancode= scancode :scancode-d) (qpush *user-commands*
                                                               (make-command :time the-time
                                                                             :type 'movement
									     :set? nil
                                                                             :content 'cw)))
                 ((sdl2:scancode= scancode :scancode-space) (qpush *user-commands*
                                                                   (make-command :time the-time
                                                                                 :type 'weapon
										 :set? nil
                                                                                 :content 'cease-fire)))
                 ((sdl2:scancode= scancode :scancode-escape) (sdl2:push-event :quit)))))
	    (:idle
	     ()
	     (sdl2:render-clear ren)
	     ;; (render-texture-centered sprites ren :ship 320 240)
	     (let* ((new-time (get-internal-real-time))
		    (delta-real-time (- new-time time)))
	       (tick delta-real-time)
	       (render sprites ren new-time)

	       ;; Set up for next time
	       (setf time new-time))
	     (sdl2:render-present ren))
	    (:quit () t)))))))
