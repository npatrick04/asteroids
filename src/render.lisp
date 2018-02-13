(in-package :asteroids)

(defparameter *world-x-scale* (/ 1 320.0 5.0))
(defparameter *world-y-scale* (/ 1 240.0 5.0))

(defun-g asteroid-vert ((vert g-pt)
			&uniform
			(scale :float)
			(depth :float)
			(model->world :mat4)
			;; (world->view :mat4)
			;; (view->clip :mat4)
			)
  (let* ( ;; Unpack some data
	 ;; Model space data
	 (pos (* (pos vert) scale *scale*))
	 ;; (normal (norm vert))
	 (uv (tex vert))

	 ;; Convert to world space
	 (model-pos (v! pos 1))
	 (world-pos (* model->world model-pos))
	 ;; (world-norm (* (m4:to-mat3 model->world)
	 ;; 		normal))

	 ;; World to view space
	 ;;(view-pos (* world->view world-pos))
	 (view-pos world-pos)

	 ;; View to clip space
	 ;;(clip-pos (* view->clip view-pos))
	 (clip-pos (v! (* (x view-pos) *world-x-scale*)
		       (* (y view-pos) *world-y-scale*)
		       0
		       1)))

    ;; Return the clip-space position, and other values
    ;; for the fragment shader. 
    (values
     clip-pos
     uv)))

(defun-g asteroid-frag ((uv :vec2)
			&uniform
			(albedo :sampler-2d))
  (let* ((color (texture albedo uv)))
    (cond
      ;; Inside is red
      ((and (> (x color) 0.5)
	    (< (y color) 0.5))
       (v! 0 0 0 1))
      ;; Outside is blue
      ((and (> (z color) 0.5)
	    (< (y color) 0.5))
       (v! 0 0 0 0))
      ;; Other is white
      (t (v! 1 1 1 1)))))

(defpipeline-g asteroid-pipeline ()
  (asteroid-vert g-pt)
  (asteroid-frag :vec2))

(defun-g geom-vert ((vert :vec2)
		    &uniform
		    (model->world :mat4))
  (let* (;; Center the geometry at zero,
	 ;; and scale it to world scaling levels.
	 ;;(pos (* (- vert center) *scale*))
	 (pos (* vert *scale*))
	 
	 ;; Convert to world space
	 (model-pos (v! pos 0 1))
	 
	 (world-pos (* model->world model-pos))
	 
	 ;; World to view space
	 (view-pos world-pos)

	 ;; View to clip space
	 (clip-pos (v! (* (x view-pos) *world-x-scale*)
		       (* (y view-pos) *world-y-scale*)
		       0
		       1)))

    ;;(v! pos 0 1)
    ;;model-pos
    ;;world-pos
    clip-pos
    ))

(defun-g geom-frag (&uniform (hit :float))
  (v! hit 1 0 1))

(defpipeline-g geom-pipeline (:line-loop)
  (geom-vert :vec2)
  (geom-frag))

(defun-g bullet-vert ((vert :vec3)
		      &uniform
		      (scale :float)
		      (model->world :mat4)
		      (world->view :mat4)
		      (view->clip :mat4))
  (let* ((pos (* vert (* scale *scale*)))
	 (model-pos (v! pos 1))
	 
	 ;; Convert to world space
	 (world-pos (* model->world model-pos))
	 
	 ;; World to view space
	 ;;(view-pos (* world->view world-pos))
	 (view-pos world-pos)

	 ;; View to clip space
	 ;;(clip-pos (* view->clip view-pos))
	 (clip-pos (v! (* (x view-pos) *world-x-scale*)
		       (* (y view-pos) *world-y-scale*)
		       0
		       1))
	 )

    clip-pos
    ;(v! (/ (x world-pos) 3000.0) (/ (y world-pos) 3000.0) 0 1)
    ))

(defun-g bullet-frag ()
  (v! 1.0 1.0 1.0 1.0))

(defpipeline-g bullet-pipeline ()
  (bullet-vert :vec3)
  (bullet-frag))

(defparameter *screen-scale* 10.0)

(defun upload-uniforms-for-camera (camera)
  (setf *screen-dimensions*
	(viewport-resolution
	 (current-viewport)))
  #+nil(let ((scale (the single-float *screen-scale*)))
    (map-g #'asteroid-pipeline nil
	   :world->view (get-world->view-space camera)
	   :view->clip (projection
			camera
			(* scale 320.0)
			(* scale 240.0)))))
