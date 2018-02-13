;;;; asteroids.asd

(asdf:defsystem #:asteroids
  :description "Describe asteroids here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:cepl.sdl2
	       #:nineveh
	       #:dirt
	       #:cepl.skitter.sdl2
	       #:cl-speedy-queue
	       ;;#:livesupport
	       #:fixed/real-time)
  :serial t
  :components ((:module "src"
                        :components
                        ((:file "package")
			 (:file "things")
                         (:file "resources")
			 (:file "assets")
                         (:file "movement")
			 (:file "ship")
			 (:file "weapon")
			 (:file "asteroids")
			 (:file "camera")
                         (:file "game")
                         (:file "render")))))

