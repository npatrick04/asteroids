;;;; asteroids.lisp

(in-package #:asteroids)

;;; "asteroids" goes here. Hacks and glory await!

(defconstant +screen-width+  640)
(defconstant +screen-height+ 480)

(defparameter *frame-rate* 50)		;cause it's an easier number of
					;milliseconds to reason about than 60.
(defparameter *seconds-per-frame* (/ 1.0 *frame-rate*))
(defparameter *ireal-time-per-frame* (/ internal-time-units-per-second *frame-rate*))

(defun asteroids ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :title "Asteroids" :w +screen-width+ :h +screen-height+ :flags '(:shown))
      (sdl2:with-renderer (ren win :flags '(:accelerated :presentvsync))
	(let ((sprites (load-texture (merge-pathnames
				      "resources/asteroids.png"
				      (ql:where-is-system :asteroids))
				     ren))
	      (time (get-internal-real-time)))
	  (sdl2:with-event-loop (:method :poll)
	    (:keydown
	     (:keysym keysym)

	     ;; Is there a better and less-verbose way to do this?
	     (let ((scancode (sdl2:scancode-value keysym)))
               (cond
		 ;; possibly make these configurable...probably not
                 ((sdl2:scancode= scancode :scancode-w) )
                 ((sdl2:scancode= scancode :scancode-s) )
                 ((sdl2:scancode= scancode :scancode-a) )
                 ((sdl2:scancode= scancode :scancode-d) )
		 ;; TODO: Arrow keys
		 )))

	    (:keyup
             (:keysym keysym)
             (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
               (sdl2:push-event :quit)))
	    
	    (:idle
	     ()
	     (sdl2:render-clear ren)
	     (render-texture-centered sprites ren :ship 320 240)
	     (sdl2:render-present ren))
	    (:quit () t)))))))
