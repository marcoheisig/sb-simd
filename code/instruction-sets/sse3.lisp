(in-package #:sb-simd-sse3)

(define-instruction-set :sse3
  (:include :sse2)
  (:test #+x86-64 t #-x86-64 nil)
  (:primitives
   (f32.4-hdup #:movshdup (f32.4) (f32.4) :cost 1)
   (f32.4-ldup #:movsldup (f32.4) (f32.4) :cost 1)))

