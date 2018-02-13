;;;; package.lisp

(defpackage #:asteroids
  (:use #:cl #:fixed #:fixed/real-time
	#:cepl #:rtg-math #:nineveh #:vari
	#:livesupport #:cepl.skitter
	#:cl-speedy-queue)
  (:import-from #:alexandria
		#:define-constant)
  (:export #:asteroids))

