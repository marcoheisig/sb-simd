(declaim (sb-ext:muffle-conditions style-warning))
(in-package #:sb-vm)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro macro-when (condition &body body)
    (when condition `(progn ,@body)))
  (macro-when
	  (if (and (find-symbol "VFMADD231PD" sb-assem::*backend-instruction-set-package*)
			   (sb-alien:extern-alien "avx2_supported" sb-alien:int)))
	(defknown (%f64.4-vdot) ((simple-array double-float (*))
							 (simple-array double-float (*))
							 (integer 0 #.most-positive-fixnum))
		(simd-pack double-float)
		(movable flushable always-translatable)
	  :overwrite-fndb-silently t)
	(define-vop (%f64.4-vdot)
	  (:translate %f64.4-vdot)
	  (:policy :fast-safe)
	  (:args (u  :scs (descriptor-reg))
			 (v  :scs (descriptor-reg))
			 (n0-tn :scs (signed-reg)))
	  (:arg-types simple-array-double-float simple-array-double-float
				  tagged-num)
	  (:temporary (:sc signed-reg) i)
	  (:temporary (:sc signed-reg) n0)
	  (:temporary (:sc double-avx2-reg) ymm0)
	  (:temporary (:sc double-avx2-reg) ymm1)
	  (:temporary (:sc double-avx2-reg) ymm2)
	  (:temporary (:sc double-avx2-reg) ymm3)
	  (:temporary (:sc double-avx2-reg) ymm4)
	  (:temporary (:sc double-avx2-reg) ymm5)
	  (:temporary (:sc double-avx2-reg) ymm6)
	  (:temporary (:sc double-avx2-reg) ymm7)
	  (:temporary (:sc double-sse-reg)  xmm8)
	  (:temporary (:sc double-sse-reg)  xmm9)
	  (:results (result :scs (double-sse-reg)))
	  (:result-types simd-pack-double)
	  (:generator 22
				  (move n0 n0-tn)
				  (inst vxorpd ymm4 ymm4 ymm4)
				  (inst vxorpd ymm5 ymm5 ymm5)
				  (inst vxorpd ymm6 ymm6 ymm6)
				  (inst vxorpd ymm7 ymm7 ymm7)
				  (inst xor i i)
				  LOOP
				  (inst vmovupd ymm0 (float-ref-ea u i 0 8 :scale 8))
				  (inst vmovupd ymm1 (float-ref-ea u i 4 8 :scale 8))
				  (inst vmovupd ymm2 (float-ref-ea u i 8 8 :scale 8))
				  (inst vmovupd ymm3 (float-ref-ea u i 12 8 :scale 8))
				  (inst vfmadd231pd ymm4 ymm0 (float-ref-ea v i 0 8 :scale 8))
				  (inst vfmadd231pd ymm5 ymm1 (float-ref-ea v i 4 8 :scale 8))
				  (inst vfmadd231pd ymm6 ymm2 (float-ref-ea v i 8 8 :scale 8))
				  (inst vfmadd231pd ymm7 ymm3 (float-ref-ea v i 12 8 :scale 8))
				  (inst add i 16)
				  (inst cmp i n0)
				  (inst jmp :b LOOP)
				  DONE
				  (inst vaddpd ymm4 ymm4 ymm5)
				  (inst vaddpd ymm6 ymm6 ymm7)
				  (inst vaddpd ymm4 ymm4 ymm6)
				  (inst vmovapd xmm8 ymm4)
				  (inst vextractf128 xmm9 ymm4 1)
				  (inst vzeroupper)
				  (inst vaddpd xmm8 xmm8 xmm9)
				  (inst vunpckhpd xmm9 xmm8 xmm8)
				  (inst vaddsd result xmm8 xmm9)))
	
	(defknown (%f64.4-hsum) ((simd-pack-256 double-float))
		(simd-pack double-float)
		(movable flushable always-translatable)
	  :overwrite-fndb-silently t)
	(define-vop (%f64.4-hsum)
      (:translate %f64.4-hsum)
      (:policy :fast-safe)
      (:args (x :scs (double-avx2-reg)))
      (:arg-types simd-pack-256-double)
      (:temporary (:sc double-sse-reg) %xmm0)
      (:temporary (:sc double-sse-reg) %xmm1)
      (:results (result :scs (double-sse-reg)))
      (:result-types simd-pack-double)
      (:generator 4 ;; what should be the cost?
				  (inst vmovapd %xmm0 x)
				  (inst vextractf128 %xmm1 x 1)
				  (inst vaddpd %xmm0 %xmm0 %xmm1)
				  (inst vunpckhpd %xmm1 %xmm0 %xmm0)
				  (inst vaddsd result %xmm0 %xmm1)))

	(defknown (%f64.2-hsum) ((simd-pack double-float))
		(simd-pack double-float)
		(movable flushable always-translatable)
      :overwrite-fndb-silently t)
	(define-vop (%f64.2-hsum)
      (:translate %f64.2-hsum)
      (:policy :fast-safe)
      (:args (x :scs (double-sse-reg)))
      (:arg-types simd-pack-double)
      (:temporary (:sc double-sse-reg) %xmm0)
      (:temporary (:sc double-sse-reg) %xmm1)
      (:results (dest :scs (double-sse-reg)))
      (:result-types simd-pack-double)
      (:generator 4 ;; what should be the cost?
				  (inst vmovapd %xmm0 x)
				  (inst vextractf128 %xmm1 x 1)
				  (inst vaddpd %xmm0 %xmm0 %xmm1)
				  (inst vunpckhpd %xmm1 %xmm0 %xmm0)
				  (inst vaddsd dest %xmm0 %xmm1))))

  (defknown (%f32.4-hsum) ((simd-pack single-float))
	  (simd-pack single-float)
      (movable flushable always-translatable)
    :overwrite-fndb-silently t)
  (define-vop (%f32.4-hsum)
    (:translate %f32.4-hsum)
    (:policy :fast-safe)
    (:args (%xmm0 :scs (single-sse-reg)))
    (:arg-types simd-pack-single)
    (:temporary (:sc single-sse-reg) %xmm1)
    (:results (dest :scs (single-sse-reg)))
    (:result-types simd-pack-single)
    (:generator 4 ;; what should be the cost?
				(inst movshdup %xmm1 %xmm0)
				(inst addps %xmm0 %xmm1)
				(inst movhlps %xmm1 %xmm0)
				(inst addss dest %xmm1))))

(in-package #:sb-simd)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro macro-when (condition &body body)
    (when condition `(progn ,@body)))
  (declaim (ftype (function (f64.2) double-float) %sse-double-high))
  (define-inline %sse-double-high (%x)
	(declare (optimize (speed 3))
			 (type f64.2 %x))
	(sb-vm::%simd-pack-double-item %x 0))
  
  (declaim (ftype (function (f32.4) single-float) %sse-single-high))
  (define-inline %sse-single-high (%x)
	(declare (optimize (speed 3))
			 (type f32.4 %x))
	(sb-vm::%simd-pack-single-item %x 0))
  
  (declaim (ftype (function (f64.2) double-float) f64.2-hsum))
  (define-inline f64.2-hsum (%x)
	(declare (optimize (speed 3)))
	(%sse-double-high (sb-vm::%f64.2-hsum %x)))
  
  (declaim (ftype (function (f32.4) single-float) f32.4-hsum))
  (define-inline f32.4-hsum (%x)
	(declare (optimize (speed 3)))
	(%sse-single-high (sb-vm::%f32.4-hsum %x)))

  (macro-when
	  (sb-alien:extern-alien "avx2_supported" sb-alien:int)
  	(declaim (ftype (function (f64.4) double-float) f64.4-hsum))
	(define-inline f64.4-hsum (%x)
	  (declare (optimize (speed 3))
			   (type f64.4 %x))
	  (%sse-double-high (sb-vm::%f64.4-hsum %x))))
  
  (macro-when
	  (if (and (find-symbol "VFMADD231PDs" sb-assem::*backend-instruction-set-package*)
		   (sb-alien:extern-alien "avx2_supported" sb-alien:int)))
	(declaim (ftype (function ((simple-array double-float (*))
							   (simple-array double-float (*)))
							  double-float) f64.4-vdot))
	(define-inline f64.4-vdot (u v)
	  (declare (optimize (speed 3))
			   (type (simple-array double-float (*)) u v))
	  (let* ((n  (min (array-total-size u) (array-total-size v)))
			 (n0 (- n (mod n 16))))
		(if (< n 16)
			(loop for i of-type fixnum below n
				  summing (* (aref u i) (aref v i))
				  into sum of-type double-float
				  finally (return sum))
			(+ (sb-vm::%simd-pack-double-item (sb-vm::%f64.4-vdot u v n0) 0)
			   (loop for i of-type fixnum from n0 below n
					 summing (* (aref u i) (aref v i))
					 into sum of-type double-float
					 finally (return sum))))))
	(export 'f64.4-vdot))
  
  (macro-when
	  (if (and (not (find-symbol "VFMADD231PD" sb-assem::*backend-instruction-set-package*))
			   (sb-alien:extern-alien "avx2_supported" sb-alien:int))
	(declaim (ftype (function ((simple-array double-float (*))
							   (simple-array double-float (*)))
							  double-float) f64.4-vdot))
	(define-inline f64.4-vdot (u v)
	  (declare (optimize (speed 3))
			   (type (simple-array double-float (*)) u v))
	  (let* ((n  (min (array-total-size u) (array-total-size v)))
			 (n0 (- n (mod n 4))))
		(if (< n 4)
			(loop for i of-type fixnum below n
				  summing (* (aref u i) (aref v i))
				  into sum of-type double-float
				  finally (return sum))
			(+ (loop with %sum of-type f64.4 = (make-f64.4 0 0 0 0)
					 for i of-type fixnum below n0 by 4
					 do (f64.4-incf %sum (f64.4* (f64.4-ref u i) (f64.4-ref v i)))
					 finally (return (f64.4-hsum %sum)))
			   (loop for i of-type fixnum from n0 below n
					 summing (* (aref u i) (aref v i))
					 into sum of-type double-float
					 finally (return sum))))))
	(export 'f64.4-vdot)))

	(declaim (ftype (function ((simple-array double-float (*))
							   (simple-array double-float (*)))
							  double-float) f64.2-vdot))
	(define-inline f64.2-vdot (u v)
	  (declare (optimize (speed 3))
			   (type (simple-array double-float (*)) u v))
	  (let* ((n  (min (array-total-size u) (array-total-size v)))
			 (n0 (- n (mod n 2))))
		(if (< n 2)
			(loop for i of-type fixnum below n
				  summing (* (aref u i) (aref v i))
				  into sum of-type double-float
				  finally (return sum))
			(+ (loop with %sum of-type f64.2 = (make-f64.2 0 0)
					 for i of-type fixnum below n0 by 2
					 do (f64.2-incf %sum (f64.2* (f64.2-ref u i)
												 (f64.2-ref v i)))
					 finally (return (f64.2-hsum %sum)))
			   (loop for i of-type fixnum from n0 below n
					 summing (* (aref u i) (aref v i))
					 into sum of-type double-float
					 finally (return sum))))))

	(declaim (ftype (function ((simple-array single-float (*))
							   (simple-array single-float (*)))
							  double-float) f32.4-vdot))
	(define-inline f32.4-vdot (u v)
	  (declare (optimize (speed 3))
			   (type (simple-array single-float (*)) u v))
	  (let* ((n  (min (array-total-size u) (array-total-size v)))
			 (n0 (- n (mod n 4))))
		(if (< n 4)
			(loop for i of-type fixnum below n
				  summing (* (aref u i) (aref v i))
				  into sum of-type double-float
				  finally (return sum))
			(+ (loop with %sum of-type f32.4 = (make-f32.4 0 0 0 0)
					 for i of-type fixnum below n0 by 4
					 do (f32.4-incf %sum (f32.4* (f32.4-ref u i)
												 (f32.4-ref v i)))
					 finally (return (f32.4-hsum %sum)))
			   (loop for i of-type fixnum from n0 below n
					 summing (* (aref u i) (aref v i))
					 into sum of-type double-float
					 finally (return sum))))))
	)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Benchmarking code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro time-total (n &body body)
		  "N-average the execution time of BODY in seconds"
		  (declare (optimize (speed 0)))
		  (alexandria:with-gensyms (start end)
			`(let (,start ,end)
			   (sb-ext:gc :full t)
			   (setq ,start (get-internal-real-time))
			   (loop for i below ,n
					 do ,@body)
			   (setq ,end (get-internal-real-time))
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

;(benchmark-avx-double 10 100 200 400 800 1200 2400 4800 9600)
;(benchmark-sse-double 10 100 200 400 800 1200 2400 4800 9600)
;(benchmark-sse-single 10 100 200 400 800 1200 2400 4800 9600)
;(apply 'benchmark-avx-double (loop for i from 1 to 10 collect (* 1000 i)))

