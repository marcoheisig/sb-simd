;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Simpler aref functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sb-vm)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defknown %f64.2-ref ((simple-array double-float)
                    (integer 0 #.most-positive-fixnum))
      (simd-pack double-float)
      (movable foldable flushable always-translatable)
    :overwrite-fndb-silently t)
  (define-vop (%f64.2-ref)
    (:translate %f64.2-ref)
    (:args (v :scs (descriptor-reg))
           (i :scs (any-reg)))
    (:arg-types simple-array-double-float
                tagged-num)
    (:results (dest :scs (double-sse-reg)))
    (:result-types simd-pack-double)
    (:policy :fast-safe)
    (:generator 4 (inst movapd dest (float-ref-ea v i 0 8 :scale 4))))

  (defknown %f64.2-set ((simple-array double-float)
                        (integer 0 #.most-positive-fixnum)
                        (simd-pack double-float))
      (simd-pack double-float)
      (always-translatable)
    :overwrite-fndb-silently t)
  (define-vop (%f64.2-set)
    (:translate %f64.2-set)
    (:args (v :scs (descriptor-reg))
           (i :scs (any-reg))
           (x :scs (double-sse-reg) :target dest))
    (:arg-types simple-array-double-float
                tagged-num
                simd-pack-double)
    (:results (dest :scs (double-sse-reg) :from (:argument 2)))
    (:result-types simd-pack-double)
    (:policy :fast-safe)
    (:generator 4 (inst movapd (float-ref-ea v i 0 8 :scale 4) x)))

  (defknown %f32.4-ref ((simple-array single-float (*))
                        (integer 0 #.most-positive-fixnum))
      (simd-pack single-float)
      (movable foldable flushable always-translatable)
    :overwrite-fndb-silently t)
  (define-vop (%f32.4-ref)
    (:translate %f32.4-ref)
    (:args (v :scs (descriptor-reg))
           (i :scs (any-reg)))
    (:arg-types simple-array-single-float
                tagged-num)
    (:results (dest :scs (single-sse-reg)))
    (:result-types simd-pack-single)
    (:policy :fast-safe)
    (:generator 4 (inst vmovaps dest
			(float-ref-ea v i 0 8
				          :scale (ash 8 (- n-fixnum-tag-bits))))))

  (defknown %f32.4-set ((simple-array single-float (*))
                        (integer 0 #.most-positive-fixnum)
                        (simd-pack single-float))
      (simd-pack single-float)
      (always-translatable)
    :overwrite-fndb-silently t)
  (define-vop (%f32.4-set)
    (:translate %f32.4-set)
    (:args (v :scs (descriptor-reg))
           (i :scs (any-reg))
           (x :scs (single-sse-reg) :target dest))
    (:arg-types simple-array-single-float
                tagged-num
                simd-pack-single)
    (:results (dest :scs (single-sse-reg) :from (:argument 2)))
    (:result-types simd-pack-single)
    (:policy :fast-safe)
    (:generator 4 (inst vmovaps (float-ref-ea v i 0 8
	      	            :scale (ash 8 (- n-fixnum-tag-bits))) x))))
