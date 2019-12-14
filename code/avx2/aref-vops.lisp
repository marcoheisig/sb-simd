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
