(in-package #:sb-simd)

(define-instruction-set :sb-simd
  (:scalars
   (any     64  t                  #:t)
   ;; Numbers
   (index   64  sb-simd-internals::index #:signed-num (#:signed-reg))
   (u1      1   (unsigned-byte  1) #:unsigned-num (#:unsigned-reg))
   (u2      2   (unsigned-byte  2) #:unsigned-num (#:unsigned-reg))
   (u4      4   (unsigned-byte  4) #:unsigned-num (#:unsigned-reg))
   (u8      8   (unsigned-byte  8) #:unsigned-num (#:unsigned-reg))
   (u16     16  (unsigned-byte 16) #:unsigned-num (#:unsigned-reg))
   (u32     32  (unsigned-byte 32) #:unsigned-num (#:unsigned-reg))
   (u64     64  (unsigned-byte 64) #:unsigned-num (#:unsigned-reg))
   (s8      8   (signed-byte  8)   #:signed-num   (#:signed-reg))
   (s16     16  (signed-byte 16)   #:signed-num   (#:signed-reg))
   (s32     32  (signed-byte 32)   #:signed-num   (#:signed-reg))
   (s64     64  (signed-byte 64)   #:signed-num   (#:signed-reg))
   (f32     32  single-float       #:single-float (#:single-reg))
   (f64     64  double-float       #:double-float (#:double-reg))
   ;; Vectors
   (charvec 64  (simple-array character (*))          #:simple-character-string)
   ( u8vec  64  (simple-array (unsigned-byte 8) (*))  #:simple-array-unsigned-byte-8)
   (u16vec  64  (simple-array (unsigned-byte 16) (*)) #:simple-array-unsigned-byte-16)
   (u32vec  64  (simple-array (unsigned-byte 32) (*)) #:simple-array-unsigned-byte-32)
   (u64vec  64  (simple-array (unsigned-byte 64) (*)) #:simple-array-unsigned-byte-64)
   ( s8vec  64  (simple-array (signed-byte 8) (*))    #:simple-array-signed-byte-8)
   (s16vec  64  (simple-array (signed-byte 16) (*))   #:simple-array-signed-byte-16)
   (s32vec  64  (simple-array (signed-byte 32) (*))   #:simple-array-signed-byte-32)
   (s64vec  64  (simple-array (signed-byte 64) (*))   #:simple-array-signed-byte-64)
   (f32vec  64  (simple-array single-float (*))       #:simple-array-single-float)
   (f64vec  64  (simple-array double-float (*))       #:simple-array-double-float)
   ;; Arrays
   (char-array 64  (array character))
   ( u8-array  64  (array (unsigned-byte 8)))
   (u16-array  64  (array (unsigned-byte 16)))
   (u32-array  64  (array (unsigned-byte 32)))
   (u64-array  64  (array (unsigned-byte 64)))
   ( s8-array  64  (array (signed-byte 8)))
   (s16-array  64  (array (signed-byte 16)))
   (s32-array  64  (array (signed-byte 32)))
   (s64-array  64  (array (signed-byte 64)))
   (f32-array  64  (array single-float))
   (f64-array  64  (array double-float)))
  (:instructions
   ;; ub64 packers and unpackers
   (u64-from-f32  nil (u64) (f32 f32)                 :encoding :fake-vop)
   (u64-from-f64  nil (u64) (f64)                     :encoding :fake-vop)
   (u64-from-u8s  nil (u64) (u8 u8 u8 u8 u8 u8 u8 u8) :encoding :fake-vop)
   (u64-from-u16s nil (u64) (u16 u16 u16 u16)         :encoding :fake-vop)
   (u64-from-u32s nil (u64) (u32 u32)                 :encoding :fake-vop)
   (u64-from-s8s  nil (u64) (s8 s8 s8 s8 s8 s8 s8 s8) :encoding :fake-vop)
   (u64-from-s16s nil (u64) (s16 s16 s16 s16)         :encoding :fake-vop)
   (u64-from-s32s nil (u64) (s32 s32)                 :encoding :fake-vop)
   (u64-from-s64  nil (u64) (s64)                     :encoding :fake-vop)
   ( u8s-from-u64 nil (u8 u8 u8 u8 u8 u8 u8 u8) (u64) :encoding :fake-vop)
   (u16s-from-u64 nil (u16 u16 u16 u16) (u64)         :encoding :fake-vop)
   (u32s-from-u64 nil (u32 u32) (u64)                 :encoding :fake-vop)
   ( s8s-from-u64 nil (s8 s8 s8 s8 s8 s8 s8 s8) (u64) :encoding :fake-vop)
   (s16s-from-u64 nil (s16 s16 s16 s16) (u64)         :encoding :fake-vop)
   (s32s-from-u64 nil (s32 s32) (u64)                 :encoding :fake-vop)
   ( s64-from-u64 nil (s64) (u64)                     :encoding :fake-vop)
   ;; f32
   (f32-if          nil (f32) (u32 f32 f32) :encoding :fake-vop)
   (two-arg-f32-and nil (f32) (f32 f32) :encoding :fake-vop :associative t)
   (two-arg-f32-or  nil (f32) (f32 f32) :encoding :fake-vop :associative t)
   (two-arg-f32-xor nil (f32) (f32 f32) :encoding :fake-vop :associative t)
   (two-arg-f32-max nil (f32) (f32 f32) :encoding :fake-vop :associative t)
   (two-arg-f32-min nil (f32) (f32 f32) :encoding :fake-vop :associative t)
   (two-arg-f32+    nil (f32) (f32 f32) :encoding :fake-vop :associative t)
   (two-arg-f32-    nil (f32) (f32 f32) :encoding :fake-vop)
   (two-arg-f32*    nil (f32) (f32 f32) :encoding :fake-vop :associative t)
   (two-arg-f32/    nil (f32) (f32 f32) :encoding :fake-vop)
   (two-arg-f32=    nil (u32) (f32 f32) :encoding :fake-vop :associative t)
   (two-arg-f32/=   nil (u32) (f32 f32) :encoding :fake-vop :associative t)
   (two-arg-f32<    nil (u32) (f32 f32) :encoding :fake-vop)
   (two-arg-f32<=   nil (u32) (f32 f32) :encoding :fake-vop)
   (two-arg-f32>    nil (u32) (f32 f32) :encoding :fake-vop)
   (two-arg-f32>=   nil (u32) (f32 f32) :encoding :fake-vop)
   (f32-andc1       nil (f32) (f32 f32) :encoding :fake-vop)
   (f32-not         nil (f32) (f32) :encoding :fake-vop)
   (f32-reciprocal  nil (f32) (f32) :encoding :fake-vop)
   (f32-rsqrt       nil (f32) (f32) :encoding :fake-vop)
   (f32-sqrt        nil (f32) (f32) :encoding :fake-vop)
   ;; f64
   (f64-if          nil (f64) (u64 f64 f64) :encoding :fake-vop)
   (two-arg-f64-and nil (f64) (f64 f64) :encoding :fake-vop :associative t)
   (two-arg-f64-or  nil (f64) (f64 f64) :encoding :fake-vop :associative t)
   (two-arg-f64-xor nil (f64) (f64 f64) :encoding :fake-vop :associative t)
   (two-arg-f64-max nil (f64) (f64 f64) :encoding :fake-vop :associative t)
   (two-arg-f64-min nil (f64) (f64 f64) :encoding :fake-vop :associative t)
   (two-arg-f64+    nil (f64) (f64 f64) :encoding :fake-vop :associative t)
   (two-arg-f64-    nil (f64) (f64 f64) :encoding :fake-vop)
   (two-arg-f64*    nil (f64) (f64 f64) :encoding :fake-vop :associative t)
   (two-arg-f64/    nil (f64) (f64 f64) :encoding :fake-vop)
   (two-arg-f64=    nil (u64) (f64 f64) :encoding :fake-vop :associative t)
   (two-arg-f64/=   nil (u64) (f64 f64) :encoding :fake-vop :associative t)
   (two-arg-f64<    nil (u64) (f64 f64) :encoding :fake-vop)
   (two-arg-f64<=   nil (u64) (f64 f64) :encoding :fake-vop)
   (two-arg-f64>    nil (u64) (f64 f64) :encoding :fake-vop)
   (two-arg-f64>=   nil (u64) (f64 f64) :encoding :fake-vop)
   (f64-andc1       nil (f64) (f64 f64) :encoding :fake-vop)
   (f64-not         nil (f64) (f64) :encoding :fake-vop)
   (f64-reciprocal  nil (f64) (f64) :encoding :fake-vop)
   (f64-rsqrt       nil (f64) (f64) :encoding :fake-vop)
   (f64-sqrt        nil (f64) (f64) :encoding :fake-vop)
   ;; u8
   (u8-if          nil (u8) (u8 u8 u8) :encoding :fake-vop)
   (two-arg-u8-and nil (u8) (u8 u8) :encoding :fake-vop :associative t)
   (two-arg-u8-or  nil (u8) (u8 u8) :encoding :fake-vop :associative t)
   (two-arg-u8-xor nil (u8) (u8 u8) :encoding :fake-vop :associative t)
   (two-arg-u8-max nil (u8) (u8 u8) :encoding :fake-vop :associative t)
   (two-arg-u8-min nil (u8) (u8 u8) :encoding :fake-vop :associative t)
   (two-arg-u8+    nil (u8) (u8 u8) :encoding :fake-vop :associative t)
   (two-arg-u8-    nil (u8) (u8 u8) :encoding :fake-vop)
   (two-arg-u8=    nil (u8) (u8 u8) :encoding :fake-vop :associative t)
   (two-arg-u8/=   nil (u8) (u8 u8) :encoding :fake-vop :associative t)
   (two-arg-u8<    nil (u8) (u8 u8) :encoding :fake-vop)
   (two-arg-u8<=   nil (u8) (u8 u8) :encoding :fake-vop)
   (two-arg-u8>    nil (u8) (u8 u8) :encoding :fake-vop)
   (two-arg-u8>=   nil (u8) (u8 u8) :encoding :fake-vop)
   (u8-andc1       nil (u8) (u8 u8) :encoding :fake-vop)
   (u8-not         nil (u8) (u8)    :encoding :fake-vop)
   ;; u16
   (u16-if          nil (u16) (u16 u16 u16) :encoding :fake-vop)
   (two-arg-u16-and nil (u16) (u16 u16) :encoding :fake-vop :associative t)
   (two-arg-u16-or  nil (u16) (u16 u16) :encoding :fake-vop :associative t)
   (two-arg-u16-xor nil (u16) (u16 u16) :encoding :fake-vop :associative t)
   (two-arg-u16-max nil (u16) (u16 u16) :encoding :fake-vop :associative t)
   (two-arg-u16-min nil (u16) (u16 u16) :encoding :fake-vop :associative t)
   (two-arg-u16+    nil (u16) (u16 u16) :encoding :fake-vop :associative t)
   (two-arg-u16-    nil (u16) (u16 u16) :encoding :fake-vop)
   (two-arg-u16=    nil (u16) (u16 u16) :encoding :fake-vop :associative t)
   (two-arg-u16/=   nil (u16) (u16 u16) :encoding :fake-vop :associative t)
   (two-arg-u16<    nil (u16) (u16 u16) :encoding :fake-vop)
   (two-arg-u16<=   nil (u16) (u16 u16) :encoding :fake-vop)
   (two-arg-u16>    nil (u16) (u16 u16) :encoding :fake-vop)
   (two-arg-u16>=   nil (u16) (u16 u16) :encoding :fake-vop)
   (u16-andc1       nil (u16) (u16 u16) :encoding :fake-vop)
   (u16-not         nil (u16) (u16)     :encoding :fake-vop)
   ;; u32
   (u32-if          nil (u32) (u32 u32 u32) :encoding :fake-vop)
   (two-arg-u32-and nil (u32) (u32 u32) :encoding :fake-vop :associative t)
   (two-arg-u32-or  nil (u32) (u32 u32) :encoding :fake-vop :associative t)
   (two-arg-u32-xor nil (u32) (u32 u32) :encoding :fake-vop :associative t)
   (two-arg-u32-max nil (u32) (u32 u32) :encoding :fake-vop :associative t)
   (two-arg-u32-min nil (u32) (u32 u32) :encoding :fake-vop :associative t)
   (two-arg-u32+    nil (u32) (u32 u32) :encoding :fake-vop :associative t)
   (two-arg-u32-    nil (u32) (u32 u32) :encoding :fake-vop)
   (two-arg-u32=    nil (u32) (u32 u32) :encoding :fake-vop :associative t)
   (two-arg-u32/=   nil (u32) (u32 u32) :encoding :fake-vop :associative t)
   (two-arg-u32<    nil (u32) (u32 u32) :encoding :fake-vop)
   (two-arg-u32<=   nil (u32) (u32 u32) :encoding :fake-vop)
   (two-arg-u32>    nil (u32) (u32 u32) :encoding :fake-vop)
   (two-arg-u32>=   nil (u32) (u32 u32) :encoding :fake-vop)
   (u32-andc1       nil (u32) (u32 u32) :encoding :fake-vop)
   (u32-not         nil (u32) (u32)     :encoding :fake-vop)
   ;; u64
   (u64-if          nil (u64) (u64 u64 u64) :encoding :fake-vop)
   (two-arg-u64-and nil (u64) (u64 u64) :encoding :fake-vop :associative t)
   (two-arg-u64-or  nil (u64) (u64 u64) :encoding :fake-vop :associative t)
   (two-arg-u64-xor nil (u64) (u64 u64) :encoding :fake-vop :associative t)
   (two-arg-u64-max nil (u64) (u64 u64) :encoding :fake-vop :associative t)
   (two-arg-u64-min nil (u64) (u64 u64) :encoding :fake-vop :associative t)
   (two-arg-u64+    nil (u64) (u64 u64) :encoding :fake-vop :associative t)
   (two-arg-u64-    nil (u64) (u64 u64) :encoding :fake-vop)
   (two-arg-u64=    nil (u64) (u64 u64) :encoding :fake-vop :associative t)
   (two-arg-u64/=   nil (u64) (u64 u64) :encoding :fake-vop :associative t)
   (two-arg-u64<    nil (u64) (u64 u64) :encoding :fake-vop)
   (two-arg-u64<=   nil (u64) (u64 u64) :encoding :fake-vop)
   (two-arg-u64>    nil (u64) (u64 u64) :encoding :fake-vop)
   (two-arg-u64>=   nil (u64) (u64 u64) :encoding :fake-vop)
   (u64-andc1       nil (u64) (u64 u64) :encoding :fake-vop)
   (u64-not         nil (u64) (u64)     :encoding :fake-vop)
   ;; s8
   (s8-if          nil (s8) (u8 s8 s8) :encoding :fake-vop)
   (two-arg-s8-and nil (s8) (s8 s8) :encoding :fake-vop :associative t)
   (two-arg-s8-or  nil (s8) (s8 s8) :encoding :fake-vop :associative t)
   (two-arg-s8-xor nil (s8) (s8 s8) :encoding :fake-vop :associative t)
   (two-arg-s8-max nil (s8) (s8 s8) :encoding :fake-vop :associative t)
   (two-arg-s8-min nil (s8) (s8 s8) :encoding :fake-vop :associative t)
   (two-arg-s8+    nil (s8) (s8 s8) :encoding :fake-vop :associative t)
   (two-arg-s8-    nil (s8) (s8 s8) :encoding :fake-vop)
   (two-arg-s8=    nil (u8) (s8 s8) :encoding :fake-vop :associative t)
   (two-arg-s8/=   nil (u8) (s8 s8) :encoding :fake-vop :associative t)
   (two-arg-s8<    nil (u8) (s8 s8) :encoding :fake-vop)
   (two-arg-s8<=   nil (u8) (s8 s8) :encoding :fake-vop)
   (two-arg-s8>    nil (u8) (s8 s8) :encoding :fake-vop)
   (two-arg-s8>=   nil (u8) (s8 s8) :encoding :fake-vop)
   (s8-andc1       nil (s8) (s8 s8) :encoding :fake-vop)
   (s8-not         nil (s8) (s8)    :encoding :fake-vop)
   ;; s16
   (s16-if          nil (s16) (u16 s16 s16) :encoding :fake-vop)
   (two-arg-s16-and nil (s16) (s16 s16) :encoding :fake-vop :associative t)
   (two-arg-s16-or  nil (s16) (s16 s16) :encoding :fake-vop :associative t)
   (two-arg-s16-xor nil (s16) (s16 s16) :encoding :fake-vop :associative t)
   (two-arg-s16-max nil (s16) (s16 s16) :encoding :fake-vop :associative t)
   (two-arg-s16-min nil (s16) (s16 s16) :encoding :fake-vop :associative t)
   (two-arg-s16+    nil (s16) (s16 s16) :encoding :fake-vop :associative t)
   (two-arg-s16-    nil (s16) (s16 s16) :encoding :fake-vop)
   (two-arg-s16=    nil (u16) (s16 s16) :encoding :fake-vop :associative t)
   (two-arg-s16/=   nil (u16) (s16 s16) :encoding :fake-vop :associative t)
   (two-arg-s16<    nil (u16) (s16 s16) :encoding :fake-vop)
   (two-arg-s16<=   nil (u16) (s16 s16) :encoding :fake-vop)
   (two-arg-s16>    nil (u16) (s16 s16) :encoding :fake-vop)
   (two-arg-s16>=   nil (u16) (s16 s16) :encoding :fake-vop)
   (s16-andc1       nil (s16) (s16 s16) :encoding :fake-vop)
   (s16-not         nil (s16) (s16)     :encoding :fake-vop)
   ;; s32
   (s32-if          nil (s32) (u32 s32 s32) :encoding :fake-vop)
   (two-arg-s32-and nil (s32) (s32 s32) :encoding :fake-vop :associative t)
   (two-arg-s32-or  nil (s32) (s32 s32) :encoding :fake-vop :associative t)
   (two-arg-s32-xor nil (s32) (s32 s32) :encoding :fake-vop :associative t)
   (two-arg-s32-max nil (s32) (s32 s32) :encoding :fake-vop :associative t)
   (two-arg-s32-min nil (s32) (s32 s32) :encoding :fake-vop :associative t)
   (two-arg-s32+    nil (s32) (s32 s32) :encoding :fake-vop :associative t)
   (two-arg-s32-    nil (s32) (s32 s32) :encoding :fake-vop)
   (two-arg-s32=    nil (u32) (s32 s32) :encoding :fake-vop :associative t)
   (two-arg-s32/=   nil (u32) (s32 s32) :encoding :fake-vop :associative t)
   (two-arg-s32<    nil (u32) (s32 s32) :encoding :fake-vop)
   (two-arg-s32<=   nil (u32) (s32 s32) :encoding :fake-vop)
   (two-arg-s32>    nil (u32) (s32 s32) :encoding :fake-vop)
   (two-arg-s32>=   nil (u32) (s32 s32) :encoding :fake-vop)
   (s32-andc1       nil (s32) (s32 s32) :encoding :fake-vop)
   (s32-not         nil (s32) (s32)     :encoding :fake-vop)
   ;; s64
   (s64-if          nil (s64) (u64 s64 s64) :encoding :fake-vop)
   (two-arg-s64-and nil (s64) (s64 s64) :encoding :fake-vop :associative t)
   (two-arg-s64-or  nil (s64) (s64 s64) :encoding :fake-vop :associative t)
   (two-arg-s64-xor nil (s64) (s64 s64) :encoding :fake-vop :associative t)
   (two-arg-s64-max nil (s64) (s64 s64) :encoding :fake-vop :associative t)
   (two-arg-s64-min nil (s64) (s64 s64) :encoding :fake-vop :associative t)
   (two-arg-s64+    nil (s64) (s64 s64) :encoding :fake-vop :associative t)
   (two-arg-s64-    nil (s64) (s64 s64) :encoding :fake-vop)
   (two-arg-s64=    nil (u64) (s64 s64) :encoding :fake-vop :associative t)
   (two-arg-s64/=   nil (u64) (s64 s64) :encoding :fake-vop :associative t)
   (two-arg-s64<    nil (u64) (s64 s64) :encoding :fake-vop)
   (two-arg-s64<=   nil (u64) (s64 s64) :encoding :fake-vop)
   (two-arg-s64>    nil (u64) (s64 s64) :encoding :fake-vop)
   (two-arg-s64>=   nil (u64) (s64 s64) :encoding :fake-vop)
   (s64-andc1       nil (s64) (s64 s64) :encoding :fake-vop)
   (s64-not         nil (s64) (s64)     :encoding :fake-vop))
  (:reffers
   (f32 f32-array f32-aref f32-row-major-aref)
   (f64 f64-array f64-aref f64-row-major-aref)
   ( u8  u8-array  u8-aref  u8-row-major-aref)
   (u16 u16-array u16-aref u16-row-major-aref)
   (u32 u32-array u32-aref u32-row-major-aref)
   (u64 u64-array u64-aref u64-row-major-aref)
   ( s8  s8-array  s8-aref  s8-row-major-aref)
   (s16 s16-array s16-aref s16-row-major-aref)
   (s32 s32-array s32-aref s32-row-major-aref)
   (s64 s64-array s64-aref s64-row-major-aref))
  (:associatives
   (f32-and two-arg-f32-and +f32-true+)
   (f32-or  two-arg-f32-or  +f32-false+)
   (f32-xor two-arg-f32-xor +f32-false+)
   (f32-max two-arg-f32-max nil)
   (f32-min two-arg-f32-min nil)
   (f32+    two-arg-f32+ 0f0)
   (f32*    two-arg-f32* 1f0)
   (f64-and two-arg-f64-and +f64-true+)
   (f64-or  two-arg-f64-or  +f64-false+)
   (f64-xor two-arg-f64-xor +f64-false+)
   (f64-max two-arg-f64-max nil)
   (f64-min two-arg-f64-min nil)
   (f64+    two-arg-f64+ 0d0)
   (f64*    two-arg-f64* 1d0)
   ( u8-and two-arg-u8-and +u8-true+)
   ( u8-or  two-arg-u8-or  +u8-false+)
   ( u8-xor two-arg-u8-xor +u8-false+)
   ( u8-max two-arg-u8-max nil)
   ( u8-min two-arg-u8-min nil)
   ( u8+    two-arg-u8+ 0)
   (u16-and two-arg-u16-and +u16-true+)
   (u16-or  two-arg-u16-or  +u16-false+)
   (u16-xor two-arg-u16-xor +u16-false+)
   (u16-max two-arg-u16-max nil)
   (u16-min two-arg-u16-min nil)
   (u16+    two-arg-u16+ 0)
   (u32-and two-arg-u32-and +u32-true+)
   (u32-or  two-arg-u32-or  +u32-false+)
   (u32-xor two-arg-u32-xor +u32-false+)
   (u32-max two-arg-u32-max nil)
   (u32-min two-arg-u32-min nil)
   (u32+    two-arg-u32+ 0)
   (u64-and two-arg-u64-and +u64-true+)
   (u64-or  two-arg-u64-or  +u64-false+)
   (u64-xor two-arg-u64-xor +u64-false+)
   (u64-max two-arg-u64-max nil)
   (u64-min two-arg-u64-min nil)
   (u64+    two-arg-u64+ 0)
   ( s8-and two-arg-s8-and +s8-true+)
   ( s8-or  two-arg-s8-or  +s8-false+)
   ( s8-xor two-arg-s8-xor +s8-false+)
   ( s8-max two-arg-s8-max nil)
   ( s8-min two-arg-s8-min nil)
   ( s8+    two-arg-s8+ 0)
   (s16-and two-arg-s16-and +s16-true+)
   (s16-or  two-arg-s16-or  +s16-false+)
   (s16-xor two-arg-s16-xor +s16-false+)
   (s16-max two-arg-s16-max nil)
   (s16-min two-arg-s16-min nil)
   (s16+    two-arg-s16+ 0)
   (s32-and two-arg-s32-and +s32-true+)
   (s32-or  two-arg-s32-or  +s32-false+)
   (s32-xor two-arg-s32-xor +s32-false+)
   (s32-max two-arg-s32-max nil)
   (s32-min two-arg-s32-min nil)
   (s32+    two-arg-s32+ 0)
   (s64-and two-arg-s64-and +s64-true+)
   (s64-or  two-arg-s64-or  +s64-false+)
   (s64-xor two-arg-s64-xor +s64-false+)
   (s64-max two-arg-s64-max nil)
   (s64-min two-arg-s64-min nil)
   (s64+    two-arg-s64+ 0))
  (:comparisons
   (f32=  two-arg-f32=  u32-and +u32-true+)
   (f32<  two-arg-f32<  u32-and +u32-true+)
   (f32<= two-arg-f32<= u32-and +u32-true+)
   (f32>  two-arg-f32>  u32-and +u32-true+)
   (f32>= two-arg-f32>= u32-and +u32-true+)
   (f64=  two-arg-f64=  u64-and +u64-true+)
   (f64<  two-arg-f64<  u64-and +u64-true+)
   (f64<= two-arg-f64<= u64-and +u64-true+)
   (f64>  two-arg-f64>  u64-and +u64-true+)
   (f64>= two-arg-f64>= u64-and +u64-true+)
   (u8=  two-arg-u8=  u8-and +u8-true+)
   (u8<  two-arg-u8<  u8-and +u8-true+)
   (u8<= two-arg-u8<= u8-and +u8-true+)
   (u8>  two-arg-u8>  u8-and +u8-true+)
   (u8>= two-arg-u8>= u8-and +u8-true+)
   (u16=  two-arg-u16=  u16-and +u16-true+)
   (u16<  two-arg-u16<  u16-and +u16-true+)
   (u16<= two-arg-u16<= u16-and +u16-true+)
   (u16>  two-arg-u16>  u16-and +u16-true+)
   (u16>= two-arg-u16>= u16-and +u16-true+)
   (u32=  two-arg-u32=  u32-and +u32-true+)
   (u32<  two-arg-u32<  u32-and +u32-true+)
   (u32<= two-arg-u32<= u32-and +u32-true+)
   (u32>  two-arg-u32>  u32-and +u32-true+)
   (u32>= two-arg-u32>= u32-and +u32-true+)
   (u64=  two-arg-u64=  u64-and +u64-true+)
   (u64<  two-arg-u64<  u64-and +u64-true+)
   (u64<= two-arg-u64<= u64-and +u64-true+)
   (u64>  two-arg-u64>  u64-and +u64-true+)
   (u64>= two-arg-u64>= u64-and +u64-true+)
   (s8=  two-arg-s8=  u8-and +u8-true+)
   (s8<  two-arg-s8<  u8-and +u8-true+)
   (s8<= two-arg-s8<= u8-and +u8-true+)
   (s8>  two-arg-s8>  u8-and +u8-true+)
   (s8>= two-arg-s8>= u8-and +u8-true+)
   (s16=  two-arg-s16=  u16-and +u16-true+)
   (s16<  two-arg-s16<  u16-and +u16-true+)
   (s16<= two-arg-s16<= u16-and +u16-true+)
   (s16>  two-arg-s16>  u16-and +u16-true+)
   (s16>= two-arg-s16>= u16-and +u16-true+)
   (s32=  two-arg-s32=  u32-and +u32-true+)
   (s32<  two-arg-s32<  u32-and +u32-true+)
   (s32<= two-arg-s32<= u32-and +u32-true+)
   (s32>  two-arg-s32>  u32-and +u32-true+)
   (s32>= two-arg-s32>= u32-and +u32-true+)
   (s64=  two-arg-s64=  u64-and +u64-true+)
   (s64<  two-arg-s64<  u64-and +u64-true+)
   (s64<= two-arg-s64<= u64-and +u64-true+)
   (s64>  two-arg-s64>  u64-and +u64-true+)
   (s64>= two-arg-s64>= u64-and +u64-true+))
  (:reducers
   (f32- two-arg-f32- 0f0)
   (f32/ two-arg-f32/ 1f0)
   (f64- two-arg-f64- 0d0)
   (f64/ two-arg-f64/ 1d0)
   (u8- two-arg-u8- 0)
   (u16- two-arg-u16- 0)
   (u32- two-arg-u32- 0)
   (u64- two-arg-u64- 0)
   (s8- two-arg-s8- 0)
   (s16- two-arg-s16- 0)
   (s32- two-arg-s32- 0)
   (s64- two-arg-s64- 0))
  (:unequals
   (f32/= two-arg-f32/= u32-and +u32-true+)
   (f64/= two-arg-f64/= u64-and +u64-true+)
   (u8/= two-arg-u8/= u8-and +u8-true+)
   (u16/= two-arg-u16/= u16-and +u16-true+)
   (u32/= two-arg-u32/= u32-and +u32-true+)
   (u64/= two-arg-u64/= u64-and +u64-true+)
   (s8/= two-arg-s8/= u8-and +u8-true+)
   (s16/= two-arg-s16/= u16-and +u16-true+)
   (s32/= two-arg-s32/= u32-and +u32-true+)
   (s64/= two-arg-s64/= u64-and +u64-true+)))
