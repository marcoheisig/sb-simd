;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Benchmarking code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ql:quickload :sb-simd :silent t)
(in-package :sb-simd)
(defmacro time-total (n &body body)
  "N-average the execution time of BODY in seconds"
  (declare (optimize (speed 0)))
  (alexandria:with-gensyms (start end)
    `(let (,start ,end)
       (sb-ext:gc :full t)
       (setf ,start (get-internal-real-time))
       (loop for i below ,n
	     do ,@body)
       (setf ,end (get-internal-real-time))
       (coerce (/ (- ,end ,start) internal-time-units-per-second)
	       'float))))

(defun benchmark-avx-double (&rest v-lengths)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (notinline f64.4-vdot))
  (loop for len in v-lengths
	do (format t "Doing dot product of ~A long double float vector 1e6 x~%" len)
	collect (let ((u (make-array len :element-type 'double-float
                                         :initial-contents
					 (mapcar (lambda (i) (+ i 0.1d0))
						 (alexandria:iota len))))
		      (v (make-array len :element-type 'double-float
                                         :initial-contents
					 (mapcar (lambda (i) (+ i 0.2d0))
						 (alexandria:iota len)))))
		  (declare (type (simple-array double-float (*)) u v))
  		  (time-total 1e6 (f64.4-vdot u v)))))

(defun benchmark-avx-single (&rest v-lengths)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (notinline f32.8-vdot))
  (loop for len in v-lengths
	do (format t "Doing dot product of ~A long double float vector 1e6 x~%" len)
	collect (let ((u (make-array len :element-type 'single-float
                                         :initial-contents
					 (mapcar (lambda (i) (+ i 0.1f0))
						 (alexandria:iota len))))
		      (v (make-array len :element-type 'single-float
                                         :initial-contents
					 (mapcar (lambda (i) (+ i 0.2f0))
						 (alexandria:iota len)))))
		  (declare (type (simple-array single-float (*)) u v))
  		  (time-total 1e6 (f32.8-vdot u v)))))

(defun benchmark-sse-double (&rest v-lengths)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (notinline f64.2-vdot))
  (loop for len in v-lengths
	do (format t "Doing dot product of ~A long double float vector 1e6 x~%" len)
	collect (let ((u (make-array len :element-type 'double-float
                                         :initial-contents
					 (mapcar (lambda (i) (+ i 0.1d0))
						 (alexandria:iota len))))
		      (v (make-array len :element-type 'double-float
                                         :initial-contents
					 (mapcar (lambda (i) (+ i 0.2d0))
						 (alexandria:iota len)))))
		  (declare (type (simple-array double-float (*)) u v))
  		  (time-total 1e6 (f64.2-vdot u v)))))

(defun benchmark-sse-single (&rest v-lengths)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (notinline f32.4-vdot))
  (loop for len in v-lengths
	do (format t "Doing dot product of ~A long double float vector 1e6 x~%" len)
	collect (let ((u (make-array len :element-type 'single-float
                                         :initial-contents
					 (mapcar (lambda (i) (+ i 0.1f0))
						 (alexandria:iota len))))
		      (v (make-array len :element-type 'single-float
                                         :initial-contents
					 (mapcar (lambda (i) (+ i 0.2f0))
						 (alexandria:iota len)))))
		  (declare (type (simple-array single-float (*)) u v))
  		  (time-total 1e6 (f32.4-vdot u v)))))

(defun benchmark-vsum-double (&rest v-lengths)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (notinline f64.4-vsum))
  (loop for len in v-lengths
	do (format t "Doing dot product of ~A long double float vector 1e6 x~%" len)
	collect (let ((u (make-array len :element-type 'double-float
                                         :initial-contents
					 (mapcar (lambda (i) (+ i 0.1d0))
						 (alexandria:iota len)))))
		  (declare (type (simple-array double-float (*)) u))
  		  (time-total 1e6 (f64.4-vsum u)))))

(defun benchmark-vsum-single (&rest v-lengths)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (notinline f32.8-vsum))
  (loop for len in v-lengths
	do (format t "Doing dot product of ~A long double float vector 1e6 x~%" len)
	collect (let ((u (make-array len :element-type 'single-float
                                         :initial-contents
					 (mapcar (lambda (i) (+ i 0.1f0))
						 (alexandria:iota len)))))
		  (declare (type (simple-array single-float (*)) u))
  		  (time-total 1e6 (f32.8-vsum u)))))

;(benchmark-avx-double 10 100 200 400 800 1200 2400 4800 9600)
;(benchmark-avx-single 10 100 200 400 800 1200 2400 4800 9600)
;(benchmark-sse-double 10 100 200 400 800 1200 2400 4800 9600)
;(benchmark-sse-single 10 100 200 400 800 1200 2400 4800 9600)
;(benchmark-vsum-double 10 100 200 400 800 1200 2400 4800 9600)
;(benchmark-vsum-single 10 100 200 400 800 1200 2400 4800 9600)
;(apply 'benchmark-avx-double (loop for i from 1 to 10 collect (* 1000 i)))


