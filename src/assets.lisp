;;; Only the box function is not covered by the following copyright.

;;; Copyright (c) 2016, Baggers
;;; All rights reserved.
;;; 
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are met:
;;; 
;;; * Redistributions of source code must retain the above copyright notice, this
;;; list of conditions and the following disclaimer.
;;; 
;;; * Redistributions in binary form must reproduce the above copyright notice,
;;; this list of conditions and the following disclaimer in the documentation
;;; and/or other materials provided with the distribution.
;;; 
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
;;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
;;; OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;; 
;;; ------------------------------------------------------------


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


;;; My function for creating bullets

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
		  (list (v! -w/2 -h/2 0.0) ;tl
			(v!  w/2 -h/2 0.0) ;tr
			(v!  w/2  h/2 0.0) ;br
			(v! -w/2  h/2 0.0) ;bl
			(v! -w/2 -h/2 0.0) ;tl
			(v!  w/2  h/2 0.0))
		  :element-type :vec3
		  :dimensions 6)
		 :primitive :triangles
		 :retain-arrays t))))))

