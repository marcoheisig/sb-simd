(in-package #:sb-simd-sse4.1)

(define-instruction-set :sse4.1
  (:include :ssse3)
  (:test #+x86-64 t #-x86-64 nil)
  (:primitives
   ;; f32.4
   (f32.4-blend         #:blendps   (f32.4) (f32.4 f32.4 imm4) :cost 1 :encoding :sse)
   #+(or) ; The result of extractps is a float but must not reside in an XMM register.
   (f32.4-elt           #:extractps (f32)   (f32.4 imm2)       :cost 1)
   (f32.4-insert        #:insertps  (f32.4) (f32.4 f32.4 imm8) :cost 1 :encoding :sse)
   ;; f64.2
   (f64.2-blend         #:blendpd   (f64.2) (f64.2 f64.2 imm2) :cost 1 :encoding :sse)
   ;; u8.16
   (u8.16-elt           #:pextrb    (u8)    (u8.16 imm4)       :cost 1)
   (u8.16-insert        #:pinsrb    (u8.16) (u8.16 u8 imm4)    :cost 1 :encoding :sse)
   ;; u16.8
   (two-arg-u16.8-max   #:pmaxuw    (u16.8) (u16.8 u16.8)      :cost 2 :encoding :sse :commutative t)
   (two-arg-u16.8-min   #:pminuw    (u16.8) (u16.8 u16.8)      :cost 2 :encoding :sse :commutative t)
   (u16.8-blend         #:pblendw   (u16.8) (u16.8 u16.8 imm8) :cost 1 :encoding :sse)
   (u16.8-minpos        #:pminuw    (u16.8) (u16.8)            :cost 5)
   ;; u32.4
   (two-arg-u32.4-max   #:pmaxud    (u32.4) (u32.4 u32.4)      :cost 2 :encoding :sse :commutative t)
   (two-arg-u32.4-min   #:pminud    (u32.4) (u32.4 u32.4)      :cost 2 :encoding :sse :commutative t)
   (u32.4-elt           #:pextrd    (u32)   (u32.4 imm2)       :cost 1)
   (u32.4-insert        #:pinsrd    (u32.4) (u32.4 u32 imm2)   :cost 1 :encoding :sse)
   ;; u64.2
   (two-arg-u64.2=      #:pcmpeqq   (u64.2) (u64.2 u64.2)      :cost 1 :encoding :sse :commutative t)
   (two-arg-u64.2/=     nil         (u64.2) (u64.2 u64.2)      :cost 2 :encoding :none :commutative t)
   (u64.2-elt           #:pextrq    (u64)   (u64.2 imm1)       :cost 1)
   #+(or)                      ; TODO: PINSRQ is currently missing in SBCL.
   (u64.2-insert        #:pinsrq    (u64.2) (u64.2 u64 imm1)   :cost 1 :encoding :sse)
   ;; s8.16
   (two-arg-s8.16-max   #:pmaxsb    (s8.16) (s8.16 s8.16)      :cost 2 :encoding :sse :commutative t)
   (two-arg-s8.16-min   #:pminsb    (s8.16) (s8.16 s8.16)      :cost 2 :encoding :sse :commutative t)
   (s8.16-elt           #:pextrb    (s8)    (s8.16 imm4)       :cost 1)
   (s8.16-insert        #:pinsrb    (s8.16) (s8.16 s8 imm4)    :cost 1 :encoding :sse)
   ;; s16.8
   (s16.8-from-u8.16    #:pmovsxbw  (s16.8) (u8.16)            :cost 5)
   (s16.8-from-s8.16    #:pmovsxbw  (s16.8) (s8.16)            :cost 5)
   (s16.8-pack          #:packusdw  (s16.8) (s32.4 s32.4)      :cost 1 :encoding :sse)
   (s16.8-blend         #:pblendw   (s16.8) (s16.8 s16.8 imm8) :cost 1 :encoding :sse)
   ;; s32.4
   (two-arg-s32.4-max   #:pmaxsd    (s32.4) (s32.4 s32.4)      :cost 2 :encoding :sse :commutative t)
   (two-arg-s32.4-min   #:pminsd    (s32.4) (s32.4 s32.4)      :cost 2 :encoding :sse :commutative t)
   (s32.4-from-u8.16    #:pmovsxbd  (s32.4) (u8.16)            :cost 5)
   (s32.4-from-s8.16    #:pmovsxbd  (s32.4) (s8.16)            :cost 5)
   (s32.4-from-u16.8    #:pmovsxwd  (s32.4) (u16.8)            :cost 5)
   (s32.4-from-s16.8    #:pmovsxwd  (s32.4) (s16.8)            :cost 5)
   (two-arg-s32.4-mullo #:pmulld    (s32.4) (s32.4 s32.4)      :cost 1 :encoding :sse :commutative t)
   (s32.4-elt           #:pextrd    (s32)   (s32.4 imm2)       :cost 1)
   (s32.4-insert        #:pinsrd    (s32.4) (s32.4 s32 imm2)   :cost 1 :encoding :sse)
   ;; s64.2
   (s64.2-from-u8.16    #:pmovsxbq  (s64.2) (u8.16)            :cost 5)
   (s64.2-from-s8.16    #:pmovsxbq  (s64.2) (s8.16)            :cost 5)
   (s64.2-from-u16.8    #:pmovsxwq  (s64.2) (u16.8)            :cost 5)
   (s64.2-from-s16.8    #:pmovsxwq  (s64.2) (s16.8)            :cost 5)
   (s64.2-from-u32.4    #:pmovsxdq  (s64.2) (u32.4)            :cost 5)
   (s64.2-from-s32.4    #:pmovsxdq  (s64.2) (s32.4)            :cost 5)
   (two-arg-s64.2-mul   #:pmuldq    (s64.2) (s64.2 s64.2)      :cost 2 :encoding :sse :commutative t)
   (two-arg-s64.2=      #:pcmpeqq   (s64.2) (s64.2 s64.2)      :cost 1 :encoding :sse :commutative t)
   (two-arg-s64.2/=     nil         (s64.2) (s64.2 s64.2)      :cost 2 :encoding :none :commutative t)
   (s64.2-elt           #:pextrq    (s64)   (s64.2 imm1)       :cost 1)
   #+(or)                      ; TODO: PINSRQ is currently missing in SBCL.
   (s64.2-insert        #:pinsrq    (s64.2) (s64.2 s64 imm1)   :cost 1 :encoding :sse)
   )
  (:loads
   (f32.4-ntload #:movntdqa f32.4 f32vec f32.4-non-temporal-aref f32.4-non-temporal-row-major-aref)
   (f64.2-ntload #:movntdqa f64.2 f64vec f64.2-non-temporal-aref f64.2-non-temporal-row-major-aref)
   ;; u16.8
   (u8.16-ntload #:movntdqa u8.16 u8vec  u8.16-non-temporal-aref u8.16-non-temporal-row-major-aref)
   (u16.8-ntload #:movntdqa u16.8 u16vec u16.8-non-temporal-aref u16.8-non-temporal-row-major-aref)
   ;; u32.4
   (u32.4-ntload #:movntdqa u32.4 u32vec u32.4-non-temporal-aref u32.4-non-temporal-row-major-aref)
   (u64.2-ntload #:movntdqa u64.2 u64vec u64.2-non-temporal-aref u64.2-non-temporal-row-major-aref)
   (s8.16-ntload #:movntdqa s8.16 s8vec  s8.16-non-temporal-aref s8.16-non-temporal-row-major-aref)
   ;; s16.8
   (s16.8-ntload #:movntdqa s16.8 s16vec s16.8-non-temporal-aref s16.8-non-temporal-row-major-aref)
   ;; s32.4
   (s32.4-ntload #:movntdqa s32.4 s32vec s32.4-non-temporal-aref s32.4-non-temporal-row-major-aref)
   (s64.2-ntload #:movntdqa s64.2 s64vec s64.2-non-temporal-aref s64.2-non-temporal-row-major-aref)))
