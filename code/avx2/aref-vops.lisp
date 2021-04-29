(in-package #:sb-vm)

(defknown sb-simd::f64.4-data-vector-ref-with-offset
    (simple-array fixnum fixnum)
    sb-simd:f64.4
    (movable unsafely-flushable always-translatable)
  :overwrite-fndb-silently
  t)

(defknown sb-simd::f64.4-data-vector-set-with-offset
    (simple-array fixnum fixnum (simd-pack-256 double-float))
    sb-simd:f64.4
    (always-translatable)
  :overwrite-fndb-silently
  t)

(define-vop (f64.4-data-vector-ref-with-offset)
  (:note "inline array access")
  (:translate sb-simd::f64.4-data-vector-ref-with-offset)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg)))
  (:info offset)
  (:arg-types simple-array-double-float tagged-num
              (:constant (constant-displacement other-pointer-lowtag 8 vector-data-offset)))
  (:results (result :scs (double-avx2-reg)))
  (:result-types simd-pack-256-double)
  (:generator 7
    (inst vmovupd result (float-ref-ea object index offset 8
                                      :scale (ash 1 (- word-shift n-fixnum-tag-bits))))))

(define-vop (f64.4-data-vector-ref-c)
  (:note "inline array access")
  (:translate sb-simd::f64.4-data-vector-ref-with-offset)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:info index offset)
  (:arg-types simple-array-double-float
              (:constant low-index)
              (:constant (constant-displacement other-pointer-lowtag
                                                8 vector-data-offset)))
  (:results (result :scs (double-avx2-reg)))
  (:result-types simd-pack-256-double)
  (:generator 6
              (inst vmovupd result (float-ref-ea object index offset 8))))

(define-vop (f64.4-data-vector-set-with-offset)
  (:note "inline array access")
  (:translate sb-simd::f64.4-data-vector-set-with-offset)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (double-avx2-reg) :target result))
  (:info offset)
  (:arg-types simple-array-double-float tagged-num
              (:constant (constant-displacement other-pointer-lowtag 8 vector-data-offset))
              simd-pack-256-double)
  (:results (result :scs (double-avx2-reg)))
  (:result-types simd-pack-256-double)
  (:generator 20
              (inst vmovupd
                    (float-ref-ea object index offset 8
                                  :scale (ash 1 (- word-shift n-fixnum-tag-bits)))
                    value)
              (move result value)))

(define-vop (f64.4-data-vector-set-with-offset-c)
  (:note "inline array access")
  (:translate sb-simd::f64.4-data-vector-set-with-offset)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (value :scs (double-avx2-reg) :target result))
  (:info index offset)
  (:arg-types simple-array-double-float (:constant low-index)
              (:constant (constant-displacement other-pointer-lowtag 8 vector-data-offset))
              simd-pack-256-double)
  (:results (result :scs (double-avx2-reg)))
  (:result-types simd-pack-256-double)
  (:generator 19
              (inst vmovupd (float-ref-ea object index offset 8) value)
              (move result value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Simpler aref functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declaim (inline %f64.4-ref %f64.4-set %vzeroupper))
(defknown %f64.4-ref ((simple-array double-float (*))
					  (integer 0 #.most-positive-fixnum))
	(simd-pack-256 double-float)
    (movable foldable flushable always-translatable)
  :overwrite-fndb-silently t)
(define-vop (%f64.4-ref)
  (:translate %f64.4-ref)
  (:args (v :scs (descriptor-reg))
         (i :scs (any-reg)))
  (:arg-types simple-array-double-float
              tagged-num)
  (:results (dest :scs (double-avx2-reg)))
  (:result-types simd-pack-256-double)
  (:policy :fast-safe)
  (:generator 1 (inst vmovupd dest (float-ref-ea v i 0 0
				  	                             :scale (ash 8 (- n-fixnum-tag-bits))))))

(defknown %f64.4-set ((simple-array double-float (*))
					  (integer 0 #.most-positive-fixnum)
					  (simd-pack-256 double-float))
	(simd-pack-256 double-float)
    (always-translatable)
  :overwrite-fndb-silently t)
(define-vop (%f64.4-set)
  (:translate %f64.4-set)
  (:args (v :scs (descriptor-reg))
         (i :scs (any-reg))
         (x :scs (double-avx2-reg) :target result))
  (:arg-types simple-array-double-float
              tagged-num
              simd-pack-256-double)
  (:results (result :scs (double-avx2-reg) :from (:argument 2)))
  (:result-types simd-pack-256-double)
  (:policy :fast-safe)
  (:generator 4 (inst vmovups (float-ref-ea v i 0 8
						                    :scale (ash 8 (- n-fixnum-tag-bits))) x)))

(defknown %vzeroupper () (integer)
    (always-translatable)
  :overwrite-fndb-silently t)
(define-vop (%vzeroupper)
  (:translate %vzeroupper)
  (:policy :fast-safe)
  (:generator 1 (inst vzeroupper)))
