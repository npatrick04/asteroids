;;;; package.lisp

(defpackage #:asteroids
  (:use #:cl #:queues #:fixed #:fixed/real-time)
  (:import-from #:alexandria
		#:define-constant)
  (:export #:asteroids))

