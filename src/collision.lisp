(in-package :asteroids)

(defun point-in-polygon (point polygon)
  "Ray cast to find number of intersections, returning true when odd."
  (declare (optimize debug))
  (do* ((cross 0)
	(i 0 (1+ i))
	(j (1- (length polygon)) (1- i)))
       ((= i (length polygon))
	(oddp cross))
    (let* ((verti (elt polygon i))
	   (vertj (elt polygon j))
	   (y-constraint (not (eq (>= (y verti) (y point))
				  (> (y vertj) (y point)))))
	   (x-constraint (< (x point)
			    (+ (/ (* (- (x vertj) (x verti))
				     (- (y point) (y verti)))
				  (- (y vertj) (y verti)))
			       (x verti)))))
      #+nil(format t "Index ~A: X: ~A, Y: ~A~%"
	      i x-constraint y-constraint)
      (when (and y-constraint x-constraint)
	;(format t "~A crosses index ~A ~A~%" point i (list vertj verti))
	(incf cross)))))

(defgeneric collision (bullet asteroid))
(defmethod collision (b a) nil)
(defmethod collision ((bullet bullet) (asteroids cons))
  (declare (optimize debug))
  ;; TODO: cull the collisions using quadtree or something
  (find-if (lambda (asteroid) (collision bullet asteroid))
	   asteroids))

(defun collision-geom (asteroid)
  (destructuring-bind (center geometry &rest rest) (gethash (resource asteroid) *geometry*)
    (declare (ignore rest))
    (let* ((geom-centered (mapcar (lambda (v) (v:- v center)) geometry))
	   (geom-scaled (mapcar (lambda (v) (v:* v 0.5)) geom-centered))
	   ;; TODO rotate geom
	   (geom-world (mapcar (lambda (v) (v:+ v (pos asteroid))) geom-scaled)))
      geom-world)))

;;; TODO: this is not very good at all!
(defmethod collision ((bullet bullet) (asteroid asteroid))
  (declare (optimize debug))
  (destructuring-bind (center geometry &rest rest) (gethash (resource asteroid) *geometry*)
    (declare (ignore rest))
    (let* ((geom-centered (mapcar (lambda (v)
				    (v:- v center))
				  geometry))
	   (geom-scaled (mapcar (lambda (v)
				  (v! (v:* v *scale*) 0 1))
				geom-centered))

	   (world-pos (posv asteroid))
	   (model->world (m4:* (m4:translation world-pos)
			       (q:to-mat4 (rotv asteroid))))

	   (geom-world (mapcar (lambda (v)
				 (m4:*v model->world v))
			       geom-scaled))
	   
	   ;; (geom-world (mapcar (lambda (v) (v:+ v (pos asteroid)))
	   ;; 		       geom-scaled))
	   )
      ;; TODO: Rotate geometry by asteroid attitude
      ;(break)
      (point-in-polygon (pos bullet) geom-world))))
