;;;; package.lisp

(defpackage #:asteroids
  (:use #:cl #:queues #:fixed)
  (:import-from #:alexandria
		#:define-constant)
  (:export #:asteroids))

