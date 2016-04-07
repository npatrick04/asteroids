;;;; asteroids.asd

(asdf:defsystem #:asteroids
  :description "Describe asteroids here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:sdl2
               #:sdl2-image
               #:queues.simple-queue
	       #:alexandria
	       #:fixed)
  :serial t
  :components ((:module "src"
                        :components
                        ((:file "package")
                         (:file "resources")
                         (:file "asteroids")
                         (:file "movement")
			 (:file "ship")
			 (:file "weapon")
                         (:file "render")
                         (:file "game")))))

