(in-package #:sb-simd-sse)

(define-instruction-set :sse
  (:test #+x86-64 t #-x86-64 nil)
  (:include :x86-64)
  (:simd-packs
   (p128   nil 128 #:simd-pack (#:int-sse-reg #:double-sse-reg #:single-sse-reg))
   (f32.4  f32 128 #:simd-pack-single (#:single-sse-reg)))
  (:primitives
   (f32!-from-p128    nil        (f32) (p128)          :cost 1 :encoding :custom)
   ;; f32.4
   (f32.4!-from-f32   #:movups   (f32.4) (f32)         :cost 1 :encoding :move)
   (make-f32.4        nil        (f32.4) (f32 f32 f32 f32) :cost 1 :encoding :none)
   (f32.4-values      nil        (f32 f32 f32 f32) (f32.4) :cost 1 :encoding :none)
   (f32.4-broadcast   nil        (f32.4) (f32)         :cost 1 :encoding :none)
   (two-arg-f32.4-and #:andps    (f32.4) (f32.4 f32.4) :cost 1 :encoding :sse :commutative t)
   (two-arg-f32.4-or  #:orps     (f32.4) (f32.4 f32.4) :cost 1 :encoding :sse :commutative t)
   (two-arg-f32.4-xor #:xorps    (f32.4) (f32.4 f32.4) :cost 1 :encoding :sse :commutative t)
   (two-arg-f32.4-max #:maxps    (f32.4) (f32.4 f32.4) :cost 3 :encoding :sse :commutative t)
   (two-arg-f32.4-min #:minps    (f32.4) (f32.4 f32.4) :cost 3 :encoding :sse :commutative t)
   (two-arg-f32.4+    #:addps    (f32.4) (f32.4 f32.4) :cost 2 :encoding :sse :commutative t)
   (two-arg-f32.4-    #:subps    (f32.4) (f32.4 f32.4) :cost 2 :encoding :sse)
   (two-arg-f32.4*    #:mulps    (f32.4) (f32.4 f32.4) :cost 2 :encoding :sse :commutative t)
   (two-arg-f32.4/    #:divps    (f32.4) (f32.4 f32.4) :cost 8 :encoding :sse)
   (two-arg-f32.4=    #:cmpps    (f32.4) (f32.4 f32.4) :cost 4 :encoding :sse :prefix :eq :commutative t)
   (two-arg-f32.4/=   #:cmpps    (f32.4) (f32.4 f32.4) :cost 4 :encoding :sse :prefix :neq :commutative t)
   (two-arg-f32.4<    #:cmpps    (f32.4) (f32.4 f32.4) :cost 4 :encoding :sse :prefix :lt)
   (two-arg-f32.4<=   #:cmpps    (f32.4) (f32.4 f32.4) :cost 4 :encoding :sse :prefix :le)
   (two-arg-f32.4>    #:cmpps    (f32.4) (f32.4 f32.4) :cost 4 :encoding :sse :prefix :nle)
   (two-arg-f32.4>=   #:cmpps    (f32.4) (f32.4 f32.4) :cost 4 :encoding :sse :prefix :nlt)
   (f32.4-andnot      #:andnps   (f32.4) (f32.4 f32.4) :cost 1 :encoding :sse)
   (f32.4-not         nil        (f32.4) (f32.4)       :cost 1 :encoding :none)
   (f32.4-reciprocal  #:rcpps    (f32.4) (f32.4)       :cost 5)
   (f32.4-rsqrt       #:rsqrtps  (f32.4) (f32.4)       :cost 5)
   (f32.4-sqrt        #:sqrtps   (f32.4) (f32.4)       :cost 15)
   (f32.4-shuffle     #:shufps   (f32.4) (f32.4 sb-simd-x86-64::imm8) :cost 1)
   (f32.4-unpacklo    #:unpcklps (f32.4) (f32.4 f32.4) :cost 1 :encoding :sse)
   (f32.4-unpackhi    #:unpckhps (f32.4) (f32.4 f32.4) :cost 1 :encoding :sse))
  (:loads
   (f32.4-load        #:movups  f32.4 f32vec f32.4-aref f32.4-row-major-aref))
  (:stores
   (f32.4-store       #:movups  f32.4 f32vec f32.4-aref f32.4-row-major-aref)
   (f32.4-ntstore     #:movntps f32.4 f32vec f32.4-non-temporal-aref f32.4-non-temporal-row-major-aref)))

