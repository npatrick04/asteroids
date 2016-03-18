;;;; asteroids.asd

(asdf:defsystem #:asteroids
  :description "Describe asteroids here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:sdl2
               #:sdl2-image)
  :serial t
  :components ((:file "package")
	       (:file "resources")
	       (:file "render")
               (:file "asteroids")
	       (:file "movement")))

