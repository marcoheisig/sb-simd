(in-package #:sb-vm)

(sb-simd::define-simple-vop sb-simd::f64.2-from-u64.2
    (((r :scs (double-sse-reg)) sb-simd::f64.2)
     ((x :scs (int-sse-reg)) sb-simd:u64.2))
  (:generator 5 (inst cvtdq2pd r x)))
