(in-package :asteroids)

(defun load-texture (filename renderer)
  (let ((image (sdl2-image:load-image filename)))
    (unwind-protect
	 (or (sdl2:create-texture-from-surface renderer image)
	     (error "Unable to create texture with image ~A" image))
      (sdl2:free-surface image))))

(defun render-texture (texture renderer source x y)
  "Render the texture"
  (let ((source-rect (gethash source *sprites*)))
    (let ((w (sdl2:rect-width source-rect))
	  (h (sdl2:rect-height source-rect)))
      (sdl2:render-copy renderer texture
			:source-rect source-rect
			:dest-rect (sdl2:make-rect x y w h)))))

(defun render-texture-centered (texture renderer source x y)
  "Render the texture centered"
  (let ((source-rect (gethash source *sprites*)))
    (let ((x (- x (truncate (sdl2:rect-width source-rect) 2)))
	  (y (- y (truncate (sdl2:rect-height source-rect) 2)))
	  (w (sdl2:rect-width source-rect))
	  (h (sdl2:rect-height source-rect)))
      (sdl2:render-copy renderer texture
			:source-rect source-rect
			:dest-rect (sdl2:make-rect x y w h)))))

(defgeneric render-object (texture renderer object deltat))
(defmethod render-object (texture renderer object deltat)
  (format *debug-out* "render object~%")
  (let* ((position (pos object))
	 (x (mod (round (+ (vec2-x position)
			   +half-screen-width+))
		 +screen-width+))
	 (y (mod (round (+ (vec2-y position)
			   +half-screen-height+))
		 +screen-width+)))
    (render-texture-centered texture
			     renderer
			     (resource object)
			     x
			     y)))
(defmethod render-object (texture renderer (ship ship) new-time)
  (declare (type game-time new-time))
  (let ((deltat-sec (game-time (f- new-time (timestamp ship)))))
    (multiple-value-bind
	  (pos vel att) (propel ship (thrust ship) (float deltat-sec))
      (declare (ignore vel att))
      (let* ((x (mod (round (+ (vec2-x pos)
			       +half-screen-width+))
		     +screen-width+))
	     (y (mod (round (+ (vec2-y pos)
			       +half-screen-height+))
		     +screen-width+)))
	(render-texture-centered texture
				 renderer
				 (resource ship)
				 x
				 y)))))

(defun render (texture renderer new-time)
  (dolist (object *game-objects*)
    (render-object texture renderer object new-time)))
