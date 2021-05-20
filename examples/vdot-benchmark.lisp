;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Benchmarking code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ql:quickload :sb-simd :silent t)
(in-package :sb-simd)

(defun simd-sum1 (array &aux (n (array-total-size array)))
  (declare (type (simple-array double-float 1) array)
           (optimize speed (safety 0)))
  (do ((index 0 (the (integer 0 #.(- array-total-size-limit 16)) (+ index 16)))
       (acc1 (make-f64.4 0 0 0 0) (f64.4+ acc1 (f64.4-row-major-aref array (+ index 0))))
       (acc2 (make-f64.4 0 0 0 0) (f64.4+ acc2 (f64.4-row-major-aref array (+ index 4))))
       (acc3 (make-f64.4 0 0 0 0) (f64.4+ acc3 (f64.4-row-major-aref array (+ index 8))))
       (acc4 (make-f64.4 0 0 0 0) (f64.4+ acc4 (f64.4-row-major-aref array (+ index 12)))))
      ((>= index (- n 16))
       (do ((result (multiple-value-call #'+ (f64.4-values (f64.4+ acc1 acc2 acc3 acc4)))
                    (+ result (row-major-aref array index)))
            (index index (1+ index)))
           ((>= index n) result)))))

(defun simd-sum2 (array &aux (n (array-total-size array)))
  (declare (type (simple-array double-float (*)) array)
           (optimize speed (safety 0)))
  (do ((index 0 (the (integer 0 #.(- array-total-size-limit 16)) (+ index 16)))
       (acc1 (make-f64.4 0 0 0 0) (f64.4+ acc1 (f64.4-ref array (+ index 0))))
       (acc2 (make-f64.4 0 0 0 0) (f64.4+ acc2 (f64.4-ref array (+ index 4))))
       (acc3 (make-f64.4 0 0 0 0) (f64.4+ acc3 (f64.4-ref array (+ index 8))))
       (acc4 (make-f64.4 0 0 0 0) (f64.4+ acc4 (f64.4-ref array (+ index 12)))))
      ((>= index (- n 16))
       (do ((result (multiple-value-call #'+ (f64.4-values (f64.4+ acc1 acc2 acc3 acc4)))
                    (+ result (row-major-aref array index)))
            (index index (1+ index)))
           ((>= index n) result)))))

(defun simd-vdot1 (array1 array2 &aux (n (min (array-total-size array1) (array-total-size array2))))
  (declare (type (simple-array double-float 1) array1 array2)
           (optimize speed (safety 0)))
  (do ((index 0 (the (integer 0 #.(- array-total-size-limit 16)) (+ index 16)))
       (acc1 (make-f64.4 0 0 0 0) (f64.4+ acc1 (f64.4* (f64.4-row-major-aref array1 (+ index 0))
						       (f64.4-row-major-aref array2 (+ index 0)))))
       (acc2 (make-f64.4 0 0 0 0) (f64.4+ acc2 (f64.4* (f64.4-row-major-aref array1 (+ index 4))
						       (f64.4-row-major-aref array2 (+ index 4)))))
       (acc3 (make-f64.4 0 0 0 0) (f64.4+ acc3 (f64.4* (f64.4-row-major-aref array1 (+ index 8))
						       (f64.4-row-major-aref array2 (+ index 8)))))
       (acc4 (make-f64.4 0 0 0 0) (f64.4+ acc4 (f64.4* (f64.4-row-major-aref array1 (+ index 12))
						       (f64.4-row-major-aref array2 (+ index 12))))))
      ((>= index (- n 16))
       (do ((result (multiple-value-call #'+ (f64.4-values (f64.4+ acc1 acc2 acc3 acc4)))
                    (+ result (* (row-major-aref array1 index)
				 (row-major-aref array2 index))))
            (index index (1+ index)))
           ((>= index n) result)))))

(defun simd-vdot2 (array1 array2 &aux (n (min (array-total-size array1) (array-total-size array2))))
  (declare (type (simple-array double-float 1) array1 array2)
           (optimize speed (safety 0)))
  (do ((index 0 (the (integer 0 #.(- array-total-size-limit 16)) (+ index 16)))
       (acc1 (make-f64.4 0 0 0 0) (f64.4-incf acc1 (f64.4* (f64.4-ref array1 (+ index 0))
						       (f64.4-ref array2 (+ index 0)))))
       (acc2 (make-f64.4 0 0 0 0) (f64.4-incf acc2 (f64.4* (f64.4-ref array1 (+ index 4))
						       (f64.4-ref array2 (+ index 4)))))
       (acc3 (make-f64.4 0 0 0 0) (f64.4-incf acc3 (f64.4* (f64.4-ref array1 (+ index 8))
						       (f64.4-ref array2 (+ index 8)))))
       (acc4 (make-f64.4 0 0 0 0) (f64.4-incf acc4 (f64.4* (f64.4-ref array1 (+ index 12))
						       (f64.4-ref array2 (+ index 12))))))
      ((>= index (- n 16))
       (do ((result (f64.4-hsum (f64.4+ acc1 acc2 acc3 acc4))
                    (+ result (* (row-major-aref array1 index)
				 (row-major-aref array2 index))))
            (index index (1+ index)))
           ((>= index n) result)))))

(defun benchmark-avx-double (&rest v-lengths)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (notinline f64.4-vdot))
  (loop for len in v-lengths
	do (format t "Doing dot product of two ~A long double float vectors 1e6 times~%" len)
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

(defun benchmark-simd-vdot1-double (&rest v-lengths)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (notinline f64.4-vdot))
  (loop for len in v-lengths
	do (format t "Doing dot product of two ~A long double float vectors 1e6 times~%" len)
	collect (let ((u (make-array len :element-type 'double-float
                                         :initial-contents
					 (mapcar (lambda (i) (+ i 0.1d0))
						 (alexandria:iota len))))
		      (v (make-array len :element-type 'double-float
                                         :initial-contents
					 (mapcar (lambda (i) (+ i 0.2d0))
						 (alexandria:iota len)))))
		  (declare (type (simple-array double-float (*)) u v))
  		  (time-total 1e6 (simd-vdot2 u v)))))

(defun benchmark-simd-vdot2-double (&rest v-lengths)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (notinline f64.4-vdot))
  (loop for len in v-lengths
	do (format t "Doing dot product of two ~A long double float vectors 1e6 times~%" len)
	collect (let ((u (make-array len :element-type 'double-float
                                         :initial-contents
					 (mapcar (lambda (i) (+ i 0.1d0))
						 (alexandria:iota len))))
		      (v (make-array len :element-type 'double-float
                                         :initial-contents
					 (mapcar (lambda (i) (+ i 0.2d0))
						 (alexandria:iota len)))))
		  (declare (type (simple-array double-float (*)) u v))
  		  (time-total 1e6 (simd-vdot2 u v)))))

(defun benchmark-avx-single (&rest v-lengths)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (notinline f32.8-vdot))
  (loop for len in v-lengths
	do (format t "Doing dot product of two ~A long single float vectors 1e6 times~%" len)
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
	do (format t "Doing dot product of two ~A long double float vectors 1e6 times~%" len)
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
	do (format t "Doing dot product of two ~A long single float vectors 1e6 times~%" len)
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

(defun benchmark-simd-vdot1-single (&rest v-lengths)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (notinline f32.4-vdot))
  (loop for len in v-lengths
	do (format t "Doing dot product of two ~A long single float vectors 1e6 times~%" len)
	collect (let ((u (make-array len :element-type 'single-float
                                         :initial-contents
					 (mapcar (lambda (i) (+ i 0.1f0))
						 (alexandria:iota len))))
		      (v (make-array len :element-type 'single-float
                                         :initial-contents
					 (mapcar (lambda (i) (+ i 0.2f0))
						 (alexandria:iota len)))))
		  (declare (type (simple-array single-float (*)) u v))
  		  (time-total 1e6 (simd-vdot1 u v)))))

(defun benchmark-simd-vdot2-single (&rest v-lengths)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (notinline f32.4-vdot))
  (loop for len in v-lengths
	do (format t "Doing dot product of two ~A long single float vectors 1e6 times~%" len)
	collect (let ((u (make-array len :element-type 'single-float
                                         :initial-contents
					 (mapcar (lambda (i) (+ i 0.1f0))
						 (alexandria:iota len))))
		      (v (make-array len :element-type 'single-float
                                         :initial-contents
					 (mapcar (lambda (i) (+ i 0.2f0))
						 (alexandria:iota len)))))
		  (declare (type (simple-array single-float (*)) u v))
  		  (time-total 1e6 (simd-vdot2 u v)))))


(defun benchmark-vsum-double (&rest v-lengths)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (notinline f64.4-vsum))
  (loop for len in v-lengths
	do (format t "Doing sum of a ~A long double float vector 1e6 times~%" len)
	collect (let ((u (make-array len :element-type 'double-float
                                         :initial-contents
					 (mapcar (lambda (i) (+ i 0.1d0))
						 (alexandria:iota len)))))
		  (declare (type (simple-array double-float (*)) u))
  		  (time-total 1e6 (f64.4-vsum u)))))

(defun benchmark-simd-sum1-double (&rest v-lengths)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (notinline simd-sum1))
  (loop for len in v-lengths
	do (format t "Doing sum of a ~A long double float vector 1e6 times~%" len)
	collect (let ((u (make-array len :element-type 'double-float
                                         :initial-contents
					 (mapcar (lambda (i) (+ i 0.1d0))
						 (alexandria:iota len)))))
		  (declare (type (simple-array double-float (*)) u))
  		  (time-total 1e6 (simd-sum1 u)))))

(defun benchmark-simd-sum2-double (&rest v-lengths)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (notinline simd-sum2))
  (loop for len in v-lengths
	do (format t "Doing dot product of ~A long double float vector 1e6 x~%" len)
	collect (let ((u (make-array len :element-type 'double-float
                                         :initial-contents
					 (mapcar (lambda (i) (+ i 0.1d0))
						 (alexandria:iota len)))))
		  (declare (type (simple-array double-float (*)) u))
  		  (time-total 1e6 (simd-sum2 u)))))

(defun benchmark-vsum-single (&rest v-lengths)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (notinline f32.8-vsum))
  (loop for len in v-lengths
	do (format t "Doing sum of a ~A long single float vector 1e6 x~%" len)
	collect (let ((u (make-array len :element-type 'single-float
                                         :initial-contents
					 (mapcar (lambda (i) (+ i 0.1f0))
						 (alexandria:iota len)))))
		  (declare (type (simple-array single-float (*)) u))
  		  (time-total 1e6 (f32.8-vsum u)))))

(defun rsqr13-bench ()
  (declare (optimize speed)
           (notinline f64.4-rsqrt13))
  (let ((x (make-f64.4 1 2 3 4)))
    (time-total 1e9 (f64.4-rsqrt13 x))))

(defun rsqr9-bench ()
  (declare (optimize speed)
           (notinline f64.4-rsqrt9))
  (let ((x (make-f64.4 1 2 3 4)))
    (time-total 1e9 (f64.4-rsqrt9 x))))

(defun rec13-bench ()
  (declare (optimize speed)
           (notinline f64.4-rec13))
  (let ((x (make-f64.4 1 2 3 4)))
    (time-total 1e9 (f64.4-rec13 x))))

(defun rec9-bench ()
  (declare (optimize speed)
           (notinline f64.4-rec9))
  (let ((x (make-f64.4 1 2 3 4)))
    (time-total 1e9 (f64.4-rec9 x))))

(defun reciprocal-bench ()
  (declare (optimize speed)
           (notinline f32.4-reciprocal))
  (let ((x (make-f32.4 1 2 3 4)))
    (time-total 1e9 (f32.4-reciprocal x))))


(defun rsqr-bench ()
  (declare (optimize speed)
           (notinline f64.4-sqrt))
  (let ((x (make-f64.4 1 2 3 4)))
    (time-total 1e9 (f64.4-sqrt x))))

(defun broadcast-bench ()
  (declare (optimize speed)
           (notinline f64.4-broadcast))
  (let ((x 0d0))
    (time-total 1e9 (f64.4-broadcast x))))

(defun make-bench ()
  (declare (optimize speed)
           (notinline make-f64.4))
  (let ((x 0d0))
    (time-total 1e9 (make-f64.4 x x x x))))

;(benchmark-avx-double 10 100 200 400 800 1200 2400 4800 9600)
;(benchmark-avx-single 10 100 200 400 800 1200 2400 4800 9600)
;(benchmark-sse-double 10 100 200 400 800 1200 2400 4800 9600)
;(benchmark-sse-single 10 100 200 400 800 1200 2400 4800 9600)
;(benchmark-vsum-double 10 100 200 400 800 1200 2400 4800 9600)
;(benchmark-vsum-single 10 100 200 400 800 1200 2400 4800 9600)
;(apply 'benchmark-avx-double (loop for i from 1 to 10 collect (* 1000 i)))


