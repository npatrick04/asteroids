(in-package :asteroids)

;;; Cribbed from play-with-verts

;;------------------------------------------------------------
;; Textures & Samplers
;;
;; We cache the data based on the the path so we don't
;; get lots of instances in memory

(defvar *samplers* (make-hash-table :test #'equal))

(defun tex (path &optional (force nil) (mipmap t))
  (when force
    (let ((s (gethash path *samplers*)))
      (when s
        (free (sampler-texture s)))
      (remhash path *samplers*)))
  (or (gethash path *samplers*)
      (setf (gethash path *samplers*)
            (sample
             (dirt:load-image-to-texture
              (project-path path)
              :rgba8
              mipmap
	      t)))))

(defparameter *meshes* (make-hash-table :test #'equal))

(defun box (&optional (width 1f0) (height 1f0))
  (let ((key (list 'box width height)))
    (or (gethash key *meshes*)
        (setf (gethash key *meshes*)
	      (let ((w/2 (/ width 2.0))
		    (h/2 (/ height 2.0))
		    (-w/2 (/ width -2.0))
		    (-h/2 (/ height -2.0)))
		(make-buffer-stream
		 (make-gpu-array
		  (print (list (v! -w/2 -h/2 0.0) ;tl
			       (v!  w/2 -h/2 0.0) ;tr
			       (v!  w/2  h/2 0.0) ;br
			       (v! -w/2  h/2 0.0) ;bl
			       (v! -w/2 -h/2 0.0) ;tl
			       (v!  w/2  h/2 0.0)))
		  :element-type :vec3
		  :dimensions 6)
		 :primitive :triangles
		 :retain-arrays t))))))

(defun cylinder (&optional (radius 1f0) (height 1f0))
  (let ((key (list radius height)))
    (or (gethash key *meshes*)
        (destructuring-bind (vert index)
            (nineveh.mesh.data.primitives:cylinder-gpu-arrays :radius radius
                                                              :height height)
          (setf (gethash key *meshes*)
                (make-buffer-stream vert :index-array index))))))
