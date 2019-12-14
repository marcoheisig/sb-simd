(in-package #:sb-vm)

(sb-simd::define-simd-vop sb-simd::f32.4-from-f64.4
    (((r :scs (single-sse-reg)) sb-simd:f32.4)
     ((x :scs (double-avx2-reg)) sb-simd:f64.4))
  (:generator 5 (inst vcvtpd2ps r x)))

#+(or)
(sb-simd::define-simd-vop sb-simd::f32.4-from-u64.4
    (((r :scs (single-sse-reg)) sb-simd:f32.4)
     ((x :scs (int-avx2-reg)) sb-simd:u64.4))
  (:generator 5 (inst vcvtdq2ps r x)))

(sb-simd::define-simd-vop sb-simd::f64.4-from-f32.4
    (((r :scs (double-avx2-reg)) sb-simd:f64.4)
     ((x :scs (single-sse-reg)) sb-simd:f32.4))
  (:temporary (:sc single-avx2-reg :target r) tmp)
  (:generator 5
              (move tmp x)
              (inst vcvtps2pd r tmp)))

#+(or)
(sb-simd::define-simd-vop sb-simd::f64.4-from-u64.4
    (((r :scs (double-avx2-reg)) sb-simd:f64.4)
     ((x :scs (int-avx2-reg)) sb-simd:u64.4))
  (:generator 5 (inst vcvtdq2pd r x)))
