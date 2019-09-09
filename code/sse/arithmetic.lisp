(in-package #:sb-vm)

(defknown (sb-simd:vadd.u64.2) (sb-simd:u64.2 sb-simd:u64.2) sb-simd:u64.2
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)

(defknown (sb-simd:vadd.f64.2
           sb-simd:vsub.f64.2
           sb-simd:vmul.f64.2
           sb-simd:vdiv.f64.2)
    ((simd-pack double-float) (simd-pack double-float))
    (simd-pack double-float)
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)

(define-vop (sb-simd:vadd.u64.2)
  (:translate sb-simd:vadd.u64.2)
  (:policy :fast-safe)
  (:args (x :scs (int-sse-reg) :target r)
         (y :scs (int-sse-reg)))
  (:arg-types simd-pack-int simd-pack-int)
  (:results (r :scs (int-sse-reg)))
  (:result-types simd-pack-int)
  (:generator
   4
   (move r x)
   (inst paddq r y)))

#+(or)
(define-vop (sb-simd:vadd-f64.2)
  (:translate sb-simd:vadd-f64.2)
  (:policy :fast-safe)
  (:args (x :scs (double-sse-reg) :target r)
         (y :scs (double-sse-reg)))
  (:arg-types simd-pack-double simd-pack-double)
  (:results (r :scs (double-sse-reg)))
  (:result-types simd-pack-double)
  (:generator 4 (inst addpd x y)))
