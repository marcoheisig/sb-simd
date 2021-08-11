(in-package #:sb-simd-sse4.2)

(define-instruction-set :sse4.2
  (:include :sse4.1)
  (:test #+x86-64 t #-x86-64 nil)
  (:primitives
   ;; u64.2
   (two-arg-u64.2>  #:pcmpgtq (u64.2) (u64.2 u64.2) :cost 3 :encoding :sse)
   (two-arg-u64.2>= nil       (u64.2) (u64.2 u64.2) :cost 4 :encoding :none)
   (two-arg-u64.2<  nil       (u64.2) (u64.2 u64.2) :cost 3 :encoding :none)
   (two-arg-u64.2<= nil       (u64.2) (u64.2 u64.2) :cost 4 :encoding :none)
   ;; s64.2
   (two-arg-s64.2>  #:pcmpgtq (u64.2) (u64.2 u64.2) :cost 3 :encoding :sse)
   (two-arg-s64.2>= nil       (u64.2) (u64.2 u64.2) :cost 4 :encoding :none)
   (two-arg-s64.2<  nil       (u64.2) (u64.2 u64.2) :cost 3 :encoding :none)
   (two-arg-s64.2<= nil       (u64.2) (u64.2 u64.2) :cost 4 :encoding :none)))
