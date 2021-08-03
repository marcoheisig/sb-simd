(in-package #:cl-user)

(defpackage #:sb-simd-internals
  (:use #:common-lisp)
  (:export
   ;; utilities.lisp
   #:type-specifier
   #:non-nil-symbol
   #:function-name
   #:index
   #:ensure-package
   #:mksym
   #:argument-symbols
   #:result-symbols
   #:touch
   #:define-inline
   #:define-notinline
   #:with-constant-arguments
   #:with-constant-argument
   #:+u8-true+
   #:+u16-true+
   #:+u32-true+
   #:+u64-true+
   #:+u8-false+
   #:+u16-false+
   #:+u32-false+
   #:+u64-false+
   #:+s8-true+
   #:+s16-true+
   #:+s32-true+
   #:+s64-true+
   #:+s8-false+
   #:+s16-false+
   #:+s32-false+
   #:+s64-false+
   #:+f32-true+
   #:+f64-true+
   #:+f32-false+
   #:+f64-false+
   ;; instruction-set.lisp
   #:instruction-set
   #:instruction-set-p
   #:instruction-set-name
   #:instruction-set-package
   #:instruction-set-includes
   #:instruction-set-available-p
   #:find-instruction-set
   #:included-instruction-sets
   #:record
   #:record-p
   #:record-name
   #:record-instruction-set
   #:value-record
   #:value-record-p
   #:value-record-name
   #:value-record-instruction-set
   #:value-record-type
   #:value-record-primitive-type
   #:value-record-bits
   #:value-record-scs
   #:find-value-record
   #:scalar-record
   #:scalar-record-p
   #:scalar-record-name
   #:scalar-record-instruction-set
   #:scalar-record-type
   #:scalar-record-primitive-type
   #:scalar-record-bits
   #:scalar-record-scs
   #:simd-record
   #:simd-record-p
   #:simd-record-name
   #:simd-record-instruction-set
   #:simd-record-type
   #:simd-record-primitive-type
   #:simd-record-bits
   #:simd-record-scs
   #:simd-record-scalar-record
   #:simd-record-size
   #:instruction-record
   #:instruction-record-p
   #:instruction-record-name
   #:instruction-record-instruction-set
   #:instruction-record-vop
   #:instruction-record-mnemonic
   #:find-instruction-record
   #:filter-instruction-records
   #:filter-available-instruction-records
   #:primitive-record
   #:primitive-record-p
   #:primitive-record-name
   #:primitive-record-instruction-set
   #:primitive-record-vop
   #:primitive-record-mnemonic
   #:primitive-record-result-records
   #:primitive-record-argument-records
   #:primitive-record-cost
   #:primitive-record-pure
   #:primitive-record-commutative
   #:primitive-record-encoding
   #:primitive-record-prefix
   #:primitive-record-suffix
   #:vref-record
   #:vref-record-p
   #:vref-record-name
   #:vref-record-instruction-set
   #:vref-record-vop
   #:vref-record-mnemonic
   #:vref-record-value-record
   #:vref-record-vector-record
   #:vref-record-aref
   #:vref-record-row-major-aref
   #:load-record
   #:load-record-p
   #:load-record-name
   #:load-record-instruction-set
   #:load-record-vop
   #:load-record-mnemonic
   #:load-record-value-record
   #:load-record-vector-record
   #:load-record-aref
   #:load-record-row-major-aref
   #:store-record
   #:store-record-p
   #:store-record-name
   #:store-record-instruction-set
   #:store-record-vop
   #:store-record-mnemonic
   #:store-record-value-record
   #:store-record-vector-record
   #:store-record-aref
   #:store-record-row-major-aref
   #:define-instruction-set
   ;; Macros
   #:define-u64-packer
   #:define-u64-unpacker
   #:define-pseudo-vop
   #:define-unequal
   #:define-comparison
   #:define-commutative
   #:define-simd-cast
   #:define-simd-cast!
   #:define-reducer))

(progn
  (defpackage #:sb-simd-common
    (:use #:common-lisp #:sb-simd-internals)
    #0=
    (:export
     ;; Utilities
     #:define-inline
     ;; Constants
     #:+u8-true+  #:+u16-true+  #:+u32-true+  #:+u64-true+
     #:+u8-false+ #:+u16-false+ #:+u32-false+ #:+u64-false+
     #:+s8-true+  #:+s16-true+  #:+s32-true+  #:+s64-true+
     #:+s8-false+ #:+s16-false+ #:+s32-false+ #:+s64-false+
     #:+f32-true+  #:+f64-true+
     #:+f32-false+ #:+f64-false+
     ;; Scalars
     #:u8 #:u16 #:u32 #:u64
     #:s8 #:s16 #:s32 #:s64
     #:f32 #:f64
     ;; Scalar AREF
     #:u8-aref #:u16-aref #:u32-aref #:u64-aref
     #:s8-aref #:s16-aref #:s32-aref #:s64-aref
     #:f32-aref #:f64-aref
     ;; Scalar ROW-MAJOR-AREF
     #:u8-row-major-aref #:u16-row-major-aref #:u32-row-major-aref #:u64-row-major-aref
     #:s8-row-major-aref #:s16-row-major-aref #:s32-row-major-aref #:s64-row-major-aref
     #:f32-row-major-aref #:f64-row-major-aref
     ;; Vectors
     #:u8vec #:u16vec #:u32vec #:u64vec
     #:s8vec #:s16vec #:s32vec #:s64vec
     #:f32vec #:f64vec
     ;; Integer Packers
     #:u64-from-u8s #:u64-from-u16s #:u64-from-u32s
     #:u64-from-s8s #:u64-from-s16s #:u64-from-s32s #:u64-from-s64
     ;; Integer Unpackers
     #:u8s-from-u64 #:u16s-from-u64 #:u32s-from-u64
     #:s8s-from-u64 #:s16s-from-u64 #:s32s-from-u64 #:s64-from-u64))

  (defpackage #:sb-simd-x86-64
    (:use #:common-lisp #:sb-simd-internals #:sb-simd-common)
    #0#
    #1=
    (:export #:imm1 #:imm2 #:imm3 #:imm4 #:imm5 #:imm6 #:imm7 #:imm8))

  (defpackage #:sb-simd-sse
    (:use #:common-lisp #:sb-simd-internals #:sb-simd-x86-64)
    #0#
    #1#
    #2=
    (:export
     #:p128
     ;; f32.4
     #:make-f32.4
     #:f32.4
     #:f32.4!
     #:f32.4-values
     #:f32.4-broadcast
     #:f32.4!-from-f32
     #:f32.4-and
     #:f32.4-or
     #:f32.4-xor
     #:f32.4-max
     #:f32.4-min
     #:f32.4+
     #:f32.4-
     #:f32.4*
     #:f32.4/
     #:f32.4=
     #:f32.4/=
     #:f32.4<
     #:f32.4<=
     #:f32.4>
     #:f32.4>=
     #:f32.4-andnot
     #:f32.4-not
     #:f32.4-reciprocal
     #:f32.4-rsqrt
     #:f32.4-sqrt
     #:f32.4-incf
     #:f32.4-decf
     #:f32.4-aref #:f32.4-row-major-aref
     #:f32.4-non-temporal-aref #:f32.4-non-temporal-row-major-aref
     #:f32.4-vdot
     #:f32.4-vsum))

  (defpackage #:sb-simd-sse2
    (:use #:common-lisp #:sb-simd-internals #:sb-simd-sse)
    (:shadow
     #:f32.4=
     #:f32.4/=
     #:f32.4<
     #:f32.4<=
     #:f32.4>
     #:f32.4>=)
    #0#
    #1#
    #2#
    #3=
    (:export
     ;; f32.4
     ;; f64.2
     #:make-f64.2
     #:f64.2
     #:f64.2!
     #:f64.2-values
     #:f64.2-broadcast
     #:f64.2-and
     #:f64.2-or
     #:f64.2-xor
     #:f64.2-max
     #:f64.2-min
     #:f64.2+
     #:f64.2-
     #:f64.2*
     #:f64.2/
     #:f64.2=
     #:f64.2/=
     #:f64.2<
     #:f64.2<=
     #:f64.2>
     #:f64.2>=
     #:f64.2-andnot
     #:f64.2-not
     #:f64.2-sqrt
     #:f64.2-unpackhi
     #:f64.2-unpacklo
     #:f64.2-incf
     #:f64.2-decf
     #:f64.2-aref #:f64.2-row-major-aref
     #:f64.2-non-temporal-aref #:f64.2-non-temporal-row-major-aref
     ;; u8.16
     #:make-u8.16
     #:u8.16
     #:u8.16!
     #:u8.16-values
     #:u8.16-broadcast
     #:u8.16-and
     #:u8.16-or
     #:u8.16-xor
     #:u8.16-andnot
     #:u8.16-not
     #:u8.16+
     #:u8.16-
     #:u8.16-unpackhi
     #:u8.16-unpacklo
     #:u8.16-average
     #:u8.16-incf
     #:u8.16-decf
     #:u8.16-aref #:u8.16-row-major-aref
     #:u8.16-non-temporal-aref #:u8.16-non-temporal-row-major-aref
     ;; u16.8
     #:make-u16.8
     #:u16.8
     #:u16.8!
     #:u16.8-values
     #:u16.8-broadcast
     #:u16.8-and
     #:u16.8-or
     #:u16.8-xor
     #:u16.8-andnot
     #:u16.8-not
     #:u16.8+
     #:u16.8-
     #:u16.8-unpackhi
     #:u16.8-unpacklo
     #:u16.8-average
     #:u16.8-shiftl
     #:u16.8-shiftr
     #:u16.8-incf
     #:u16.8-decf
     #:u16.8-aref #:u16.8-row-major-aref
     #:u16.8-non-temporal-aref #:u16.8-non-temporal-row-major-aref
     ;; u32.4
     #:make-u32.4
     #:u32.4
     #:u32.4!
     #:u32.4-values
     #:u32.4-broadcast
     #:u32.4-from-f64.2
     #:u32.4-and
     #:u32.4-or
     #:u32.4-xor
     #:u32.4-andnot
     #:u32.4-not
     #:u32.4+
     #:u32.4-
     #:u32.4-unpackhi
     #:u32.4-unpacklo
     #:u32.4-shiftl
     #:u32.4-shiftr
     #:u32.4-incf
     #:u32.4-decf
     #:u32.4-aref #:u32.4-row-major-aref
     #:u32.4-non-temporal-aref #:u32.4-non-temporal-row-major-aref
     ;; u64.2
     #:make-u64.2
     #:u64.2
     #:u64.2!
     #:u64.2-values
     #:u64.2-broadcast
     #:u64.2-and
     #:u64.2-or
     #:u64.2-xor
     #:u64.2-andnot
     #:u64.2-not
     #:u64.2+
     #:u64.2-
     #:u64.2-unpackhi
     #:u64.2-unpacklo
     #:u64.2-shiftl
     #:u64.2-shiftr
     #:u64.2-incf
     #:u64.2-decf
     #:u64.2-aref #:u64.2-row-major-aref
     #:u64.2-non-temporal-aref #:u64.2-non-temporal-row-major-aref
     ;; s8.16
     #:make-s8.16
     #:s8.16
     #:s8.16!
     #:s8.16-values
     #:s8.16-broadcast
     #:s8.16-and
     #:s8.16-or
     #:s8.16-xor
     #:s8.16-andnot
     #:s8.16-not
     #:s8.16+
     #:s8.16-
     #:s8.16-unpackhi
     #:s8.16-unpacklo
     #:s8.16-aref #:s8.16-row-major-aref
     #:s8.16-non-temporal-aref #:s8.16-non-temporal-row-major-aref
     ;; s16.8
     #:make-s16.8
     #:s16.8
     #:s16.8!
     #:s16.8-values
     #:s16.8-broadcast
     #:s16.8-and
     #:s16.8-or
     #:s16.8-xor
     #:s16.8-andnot
     #:s16.8-not
     #:s16.8+
     #:s16.8-
     #:s16.8-unpackhi
     #:s16.8-unpacklo
     #:s16.8-shiftl
     #:s16.8-shiftr
     #:s16.8-mullo
     #:s16.8-aref #:s16.8-row-major-aref
     #:s16.8-non-temporal-aref #:s16.8-non-temporal-row-major-aref
     ;; s32.4
     #:make-s32.4
     #:s32.4
     #:s32.4!
     #:s32.4-values
     #:s32.4-broadcast
     #:s32.4-and
     #:s32.4-or
     #:s32.4-xor
     #:s32.4-andnot
     #:s32.4-not
     #:s32.4+
     #:s32.4-
     #:s32.4-unpackhi
     #:s32.4-unpacklo
     #:s32.4-shiftl
     #:s32.4-shiftr
     #:s32.4-aref #:s32.4-row-major-aref
     #:s32.4-non-temporal-aref #:s32.4-non-temporal-row-major-aref
     ;; s64.2
     #:make-s64.2
     #:s64.2
     #:s64.2!
     #:s64.2-values
     #:s64.2-broadcast
     #:s64.2-and
     #:s64.2-or
     #:s64.2-xor
     #:s64.2-andnot
     #:s64.2-not
     #:s64.2+
     #:s64.2-
     #:s64.2-unpackhi
     #:s64.2-unpacklo
     #:s64.2-shiftl
     #:s64.2-shiftr
     #:s64.2-aref #:s64.2-row-major-aref
     #:s64.2-non-temporal-aref #:s64.2-non-temporal-row-major-aref))

  (defpackage #:sb-simd-sse3
    (:use #:common-lisp #:sb-simd-internals #:sb-simd-sse2)
    #0#
    #1#
    #2#
    #3#
    #4=
    (:export
     #:f32.4-hdup
     #:f32.4-ldup
     #:f32.4-hsum
     #:f32.4-vdot
     #:f32.4-vsum
     #:f64.2-vdot
     #:f64.2-vsum))

  (defpackage #:sb-simd-ssse3
    (:use #:common-lisp #:sb-simd-internals #:sb-simd-sse3)
    #0#
    #1#
    #2#
    #3#
    #4#
    #5=
    (:export
     #:s16.8-mulhrs
     #:u16.8-hadd
     #:u32.4-hadd
     #:u16.8-hsub
     #:u32.4-hsub
     #:s8.16-shuffle
     #:s8.16-abs
     #:s8.16-sign
     #:s16.8-abs
     #:s16.8-maddubs
     #:s16.8-sign
     #:s16.8-hadd
     #:s16.8-hsub
     #:s32.4-abs
     #:s32.4-sign
     #:s32.4-hadd
     #:s32.4-hsub))

  (defpackage #:sb-simd-sse4.1
    (:use #:common-lisp #:sb-simd-internals #:sb-simd-ssse3)
    #0#
    #1#
    #2#
    #3#
    #4#
    #5#
    #6=
    (:export
     #:f32.4-blend
     #:f32.4-extract
     #:f32.4-insert
     #:f64.2-blend
     #:u8.16-extract
     #:u8.16-insert
     #:u16.8-max
     #:u16.8-min
     #:u16.8-blend
     #:u16.8-minpos
     #:u32.4-max
     #:u32.4-min
     #:u32.4-extract
     #:u32.4-insert
     #:u64.2=
     #:u64.2/=
     #:u64.2-extract
     #:s8.16-max
     #:s8.16-min
     #:s8.16-extract
     #:s8.16-insert
     #:s16.8-from-u8.16
     #:s16.8-from-s8.16
     #:s16.8-pack
     #:s16.8-blend
     #:s32.4-max
     #:s32.4-min
     #:s32.4-from-u8.16
     #:s32.4-from-s8.16
     #:s32.4-from-u16.8
     #:s32.4-from-s16.8
     #:s32.4-mullo
     #:s32.4-extract
     #:s32.4-insert
     #:s64.2-from-u8.16
     #:s64.2-from-s8.16
     #:s64.2-from-u16.8
     #:s64.2-from-s16.8
     #:s64.2-from-u32.4
     #:s64.2-from-s32.4
     #:s64.2=
     #:s64.2/=
     #:s64.2-extract
     #:f32.4-non-temporal-aref #:f32.4-non-temporal-row-major-aref
     #:f64.2-non-temporal-aref #:f64.2-non-temporal-row-major-aref
     #:u8.16-non-temporal-aref #:u8.16-non-temporal-row-major-aref
     #:u16.8-non-temporal-aref #:u16.8-non-temporal-row-major-aref
     #:u32.4-non-temporal-aref #:u32.4-non-temporal-row-major-aref
     #:u64.2-non-temporal-aref #:u64.2-non-temporal-row-major-aref
     #:s8.16-non-temporal-aref #:s8.16-non-temporal-row-major-aref
     #:s16.8-non-temporal-aref #:s16.8-non-temporal-row-major-aref
     #:s32.4-non-temporal-aref #:s32.4-non-temporal-row-major-aref
     #:s64.2-non-temporal-aref #:s64.2-non-temporal-row-major-aref))

  (defpackage #:sb-simd-sse4.2
    (:use #:common-lisp #:sb-simd-internals #:sb-simd-sse4.1)
    #0#
    #1#
    #2#
    #3#
    #4#
    #5#
    #6#
    #7=
    (:export
     #:u64.2>
     #:u64.2>=
     #:u64.2<
     #:u64.2<=))

  (defpackage #:sb-simd-avx
    (:use #:common-lisp #:sb-simd-internals #:sb-simd-x86-64)
    #0#
    #1#
    ;; SBCL's built-in floating point cast operations make use of SSE
    ;; instructions.  If we mix them with AVX code, we get a hefty SSE-AVX
    ;; transition penalty.  So we shadow these casts with our own,
    ;; VEC-encoded versions.
    (:shadow #:f32 #:f64)
    #8=
    (:export
     #:p128
     #:p256
     #:vzeroupper
     #:vzeroall
     ;; f32.4
     #:make-f32.4
     #:f32.4
     #:f32.4!
     #:f32.4-values
     #:f32.4-broadcast
     #:f32.4-from-f64.4
     #:f32.4-and
     #:f32.4-or
     #:f32.4-xor
     #:f32.4-andnot
     #:f32.4-not
     #:f32.4-max
     #:f32.4-min
     #:f32.4+
     #:f32.4-
     #:f32.4*
     #:f32.4/
     #:f32.4=
     #:f32.4/=
     #:f32.4<
     #:f32.4<=
     #:f32.4>
     #:f32.4>=
     #:f32.4-addsub
     #:f32.4-hadd
     #:f32.4-hsub
     #:f32.4-reciprocal
     #:f32.4-rsqrt
     #:f32.4-sqrt
     #:f32.4-unpackhi
     #:f32.4-unpacklo
     #:f32.4-broadcast
     #:f32.4-ceiling
     #:f32.4-blend
     #:f32.4-permute
     #:f32.4-shuffle
     #:f32.4-incf
     #:f32.4-decf
     #:f32.4-vdot
     #:f32.4-vsum
     #:f32.4-hsum
     #:f32.4-aref #:f32.4-row-major-aref
     #:f32.4-non-temporal-aref #:f32.4-non-temporal-row-major-aref
     ;; f64.2
     #:make-f64.2
     #:f64.2
     #:f64.2!
     #:f64.2-values
     #:f64.2-broadcast
     #:f64.2-and
     #:f64.2-or
     #:f64.2-xor
     #:f64.2-andnot
     #:f64.2-not
     #:f64.2-max
     #:f64.2-min
     #:f64.2+
     #:f64.2-
     #:f64.2*
     #:f64.2/
     #:f64.2=
     #:f64.2/=
     #:f64.2<
     #:f64.2<=
     #:f64.2>
     #:f64.2>=
     #:f64.2-addsub
     #:f64.2-hadd
     #:f64.2-hsub
     #:f64.2-sqrt
     #:f64.2-unpackhi
     #:f64.2-unpacklo
     #:f64.2-broadcast
     #:f64.2-ceiling
     #:f64.2-blend
     #:f64.2-permute
     #:f64.2-shuffle
     #:f64.2-vdot
     #:f64.2-incf
     #:f64.2-decf
     #:f64.2-vdot
     #:f64.2-vsum
     #:f64.2-hsum
     #:f64.2-aref #:f64.2-row-major-aref
     #:f64.2-non-temporal-aref #:f64.2-non-temporal-row-major-aref
     ;; f32.8
     #:make-f32.8
     #:f32.8
     #:f32.8!
     #:f32.8-values
     #:f32.8-broadcast
     #:f32.8-from-u32.8
     #:f32.8-and
     #:f32.8-or
     #:f32.8-xor
     #:f32.8-andnot
     #:f32.8-not
     #:f32.8-max
     #:f32.8-min
     #:f32.8+
     #:f32.8-
     #:f32.8*
     #:f32.8/
     #:f32.8=
     #:f32.8/=
     #:f32.8<
     #:f32.8<=
     #:f32.8>
     #:f32.8>=
     #:f32.8-dupeven
     #:f32.8-dupodd
     #:f32.8-hadd
     #:f32.8-hsub
     #:f32.8-reciprocal
     #:f32.8-rsqrt
     #:f32.8-sqrt
     #:f32.8-unpackhi
     #:f32.8-unpacklo
     #:f32.8-broadcast
     #:f32.8-ceiling
     #:f32.8-blend
     #:f32.8-permute
     #:f32.8-permute128
     #:f32.8-shuffle
     #:f32.8-extract128
     #:f32.8-insert128
     #:f32.8-round
     #:f32.8-incf
     #:f32.8-decf
     #:f32.8-vdot
     #:f32.8-vsum
     #:f32.8-hsum
     #:f32.8-aref #:f32.8-row-major-aref
     #:f32.8-non-temporal-aref #:f32.8-non-temporal-row-major-aref
     ;; f64.4
     #:make-f64.4
     #:f64.4
     #:f64.4!
     #:f64.4-values
     #:f64.4-broadcast
     #:f64.4-from-f32.4
     #:f64.4-from-u32.4
     #:f64.4-from-s32.4
     #:f64.4-and
     #:f64.4-or
     #:f64.4-xor
     #:f64.4-andnot
     #:f64.4-not
     #:f64.4-max
     #:f64.4-min
     #:f64.4+
     #:f64.4-
     #:f64.4*
     #:f64.4/
     #:f64.4=
     #:f64.4/=
     #:f64.4<
     #:f64.4<=
     #:f64.4>
     #:f64.4>=
     #:f64.4-dupeven
     #:f64.4-hadd
     #:f64.4-hsub
     #:f64.4-hsum
     #:f64.4-sqrt
     #:f64.4-unpackhi
     #:f64.4-unpacklo
     #:f64.4-broadcast
     #:f64.4-ceiling
     #:f64.4-blend
     #:f64.4-permute
     #:f64.4-permute128
     #:f64.4-shuffle
     #:f64.4-reverse
     #:f64.4-extract128
     #:f64.4-insert128
     #:f64.4-set128
     #:f64.4-round
     #:f64.4-incf
     #:f64.4-decf
     #:f64.4-rec-9
     #:f64.4-vdot
     #:f64.4-vsum
     #:f64.4-hsum
     #:f64.4-aref #:f64.4-row-major-aref
     #:f64.4-non-temporal-aref #:f64.4-non-temporal-row-major-aref
     ;; u8.16
     #:make-u8.16
     #:u8.16
     #:u8.16!
     #:u8.16-values
     #:u8.16-broadcast
     #:u8.16-and
     #:u8.16-or
     #:u8.16-xor
     #:u8.16-andnot
     #:u8.16-not
     #:u8.16+
     #:u8.16-
     #:u8.16=
     #:u8.16/=
     #:u8.16>
     #:u8.16<
     #:u8.16>=
     #:u8.16<=
     #:u8.16-unpackhi
     #:u8.16-unpacklo
     #:u8.16-aref #:u8.16-row-major-aref
     #:u8.16-non-temporal-aref #:u8.16-non-temporal-row-major-aref
     ;; u16.8
     #:make-u16.8
     #:u16.8
     #:u16.8!
     #:u16.8-values
     #:u16.8-broadcast
     #:u16.8-and
     #:u16.8-or
     #:u16.8-xor
     #:u16.8-andnot
     #:u16.8-not
     #:u16.8+
     #:u16.8-
     #:u16.8=
     #:u16.8/=
     #:u16.8>
     #:u16.8<
     #:u16.8>=
     #:u16.8<=
     #:u16.8-shiftl
     #:u16.8-shiftr
     #:u16.8-unpackhi
     #:u16.8-unpacklo
     #:u16.8-aref #:u16.8-row-major-aref
     #:u16.8-non-temporal-aref #:u16.8-non-temporal-row-major-aref
     ;; u32.4
     #:make-u32.4
     #:u32.4
     #:u32.4!
     #:u32.4-values
     #:u32.4-broadcast
     #:u32.4-and
     #:u32.4-or
     #:u32.4-xor
     #:u32.4-andnot
     #:u32.4-not
     #:u32.4+
     #:u32.4-
     #:u32.4=
     #:u32.4/=
     #:u32.4>
     #:u32.4<
     #:u32.4>=
     #:u32.4<=
     #:u32.4-unpackhi
     #:u32.4-unpacklo
     #:u32.4-blend
     #:u32.4-permute
     #:u32.4-aref #:u32.4-row-major-aref
     #:u32.4-non-temporal-aref #:u32.4-non-temporal-row-major-aref
     ;; u64.2
     #:make-u64.2
     #:u64.2
     #:u64.2!
     #:u64.2-values
     #:u64.2-broadcast
     #:u64.2-and
     #:u64.2-or
     #:u64.2-xor
     #:u64.2-andnot
     #:u64.2-not
     #:u64.2+
     #:u64.2-
     #:u64.2=
     #:u64.2/=
     #:u64.2>
     #:u64.2<
     #:u64.2>=
     #:u64.2<=
     #:u64.2-unpackhi
     #:u64.2-unpacklo
     #:u64.2-blend
     #:u64.2-permute
     #:u64.2-aref #:u64.2-row-major-aref
     #:u64.2-non-temporal-aref #:u64.2-non-temporal-row-major-aref
     ;; u8.32
     #:make-u8.32
     #:u8.32
     #:u8.32!
     #:u8.32-values
     #:u8.32-broadcast
     #:u8.32-extract128
     #:u8.32-insert128
     ;; u16.16
     #:make-u16.16
     #:u16.16
     #:u16.16!
     #:u16.16-values
     #:u16.16-broadcast
     #:u16.16-extract128
     #:u16.16-insert128
     ;; u32.8
     #:make-u32.8
     #:u32.8
     #:u32.8!
     #:u32.8-values
     #:u32.8-broadcast
     #:u32.8-from-f32.8
     #:u32.8-blend
     #:u32.8-permute
     #:u32.8-extract128
     #:u32.8-insert128
     ;; u64.4
     #:make-u64.4
     #:u64.4
     #:u64.4!
     #:u64.4-values
     #:u64.4-broadcast
     #:u64.4-blend
     #:u64.4-permute
     #:u64.4-extract128
     #:u64.4-insert128
     ;; s8.16
     #:make-s8.16
     #:s8.16
     #:s8.16!
     #:s8.16-values
     #:s8.16-broadcast
     #:s8.16-and
     #:s8.16-or
     #:s8.16-xor
     #:s8.16-andnot
     #:s8.16-not
     #:s8.16+
     #:s8.16-
     #:s8.16=
     #:s8.16/=
     #:s8.16>
     #:s8.16<
     #:s8.16>=
     #:s8.16<=
     #:s8.16-unpackhi
     #:s8.16-unpacklo
     #:s8.16-aref #:s8.16-row-major-aref
     #:s8.16-non-temporal-aref #:s8.16-non-temporal-row-major-aref
     ;; s16.8
     #:make-s16.8
     #:s16.8
     #:s16.8!
     #:s16.8-values
     #:s16.8-broadcast
     #:s16.8-and
     #:s16.8-or
     #:s16.8-xor
     #:s16.8-andnot
     #:s16.8-not
     #:s16.8+
     #:s16.8-
     #:s16.8-mullo
     #:s16.8=
     #:s16.8/=
     #:s16.8>
     #:s16.8<
     #:s16.8>=
     #:s16.8<=
     #:s16.8-mullo
     #:s16.8-shiftl
     #:s16.8-shiftr
     #:s16.8-unpackhi
     #:s16.8-unpacklo
     #:s16.8-aref #:s16.8-row-major-aref
     #:s16.8-non-temporal-aref #:s16.8-non-temporal-row-major-aref
     ;; s32.4
     #:make-s32.4
     #:s32.4
     #:s32.4!
     #:s32.4-values
     #:s32.4-broadcast
     #:s32.4-from-f64.4
     #:s32.4-and
     #:s32.4-or
     #:s32.4-xor
     #:s32.4-andnot
     #:s32.4-not
     #:s32.4+
     #:s32.4-
     #:s32.4-mullo
     #:s32.4=
     #:s32.4/=
     #:s32.4>
     #:s32.4<
     #:s32.4>=
     #:s32.4<=
     #:s32.4-mullo
     #:s32.4-unpackhi
     #:s32.4-unpacklo
     #:s32.4-blend
     #:s32.4-permute
     #:s32.4-aref #:s32.4-row-major-aref
     #:s32.4-non-temporal-aref #:s32.4-non-temporal-row-major-aref
     ;; s64.2
     #:s64.2
     #:make-s64.2
     #:s64.2
     #:s64.2!
     #:s64.2-values
     #:s64.2-broadcast
     #:s64.2-and
     #:s64.2-or
     #:s64.2-xor
     #:s64.2-andnot
     #:s64.2-not
     #:s64.2+
     #:s64.2-
     #:s64.2=
     #:s64.2/=
     #:s64.2>
     #:s64.2<
     #:s64.2>=
     #:s64.2<=
     #:s64.2-shiftl
     #:s64.2-shiftr
     #:s64.2-unpackhi
     #:s64.2-unpacklo
     #:s64.2-blend
     #:s64.2-permute
     #:s64.2-aref #:s64.2-row-major-aref
     #:s64.2-non-temporal-aref #:s64.2-non-temporal-row-major-aref
     ;; s8.32
     #:make-s8.32
     #:s8.32
     #:s8.32!
     #:s8.32-values
     #:s8.32-broadcast
     #:s8.32-extract128
     #:s8.32-insert128
     #:s8.32-permute128
     ;; s16.16
     #:make-s16.16
     #:s16.16
     #:s16.16!
     #:s16.16-values
     #:s16.16-broadcast
     #:s16.16-extract128
     #:s16.16-insert128
     #:s16.16-permute128
     ;; s32.8
     #:make-s32.8
     #:s32.8
     #:s32.8!
     #:s32.8-values
     #:s32.8-broadcast
     #:s32.8-extract128
     #:s32.8-insert128
     #:s32.8-permute128
     ;; s64.4
     #:make-s64.4
     #:s64.4
     #:s64.4!
     #:s64.4-values
     #:s64.4-broadcast
     #:s64.4-extract128
     #:s64.4-insert128
     #:s64.4-blend
     #:s64.4-permute
     #:s64.4-permute128))

  (defpackage #:sb-simd-avx2
    (:use #:common-lisp #:sb-simd-internals #:sb-simd-avx)
    (:shadow
     #:u8.16!-from-p256 #:u16.8!-from-p256 #:u32.4!-from-p256 #:u64.2!-from-p256
     #:s8.16!-from-p256 #:s16.8!-from-p256 #:s32.4!-from-p256 #:s64.2!-from-p256
     #:u8.16! #:u16.8! #:u32.4! #:u64.2!
     #:s8.16! #:s16.8! #:s32.4! #:s64.2!
     #:make-u8.32 #:make-u16.16 #:make-u32.8 #:make-u64.4
     #:make-s8.32 #:make-s16.16 #:make-s32.8 #:make-s64.4
     #:u8.32 #:u16.16 #:u32.8 #:u64.4
     #:s8.32 #:s16.16 #:s32.8 #:s64.4
     #:u8.32-values #:u16.16-values #:u32.8-values #:u64.4-values
     #:s8.32-values #:s16.16-values #:s32.8-values #:s64.4-values
     #:u8.32-extract128 #:u16.16-extract128 #:u32.8-extract128 #:u64.4-extract128
     #:s8.32-extract128 #:s16.16-extract128 #:s32.8-extract128 #:s64.4-extract128
     #:u8.32-insert128 #:u16.16-insert128 #:u32.8-insert128 #:u64.4-insert128
     #:s8.32-insert128 #:s16.16-insert128 #:s32.8-insert128 #:s64.4-insert128
     #:u8.16-broadcast #:u16.8-broadcast  #:u32.4-broadcast #:u64.2-broadcast
     #:s8.16-broadcast #:s16.8-broadcast  #:s32.4-broadcast #:s64.2-broadcast
     #:u8.32-broadcast #:u16.16-broadcast #:u32.8-broadcast #:u64.4-broadcast
     #:s8.32-broadcast #:s16.16-broadcast #:s32.8-broadcast #:s64.4-broadcast
     #:u32.4-blend #:s32.4-blend #:u32.8-blend #:s32.8-blend
     #:u8.32-permute128 #:s8.32-permute128
     #:u32.8-permute128 #:s32.8-permute128
     #:s16.16-permute128
     #:s64.4-permute128
     #:f64.4-reverse
     #:s64.2-shiftl
     #:s64.2-shiftr
     #:f32.8-vsum #:f64.4-vsum
     #:f32.8-vdot #:f64.4-vdot
     #:f32.8-vdot #:f64.4-vdot)
    #0#
    #1#
    #8#
    #9=
    (:export
     ;; f32.4
     #:f32.4-non-temporal-aref #:f32.4-non-temporal-row-major-aref
     ;; f64.2
     #:f64.2-non-temporal-aref #:f64.2-non-temporal-row-major-aref
     ;; f32.8
     #:F32.8-vdot
     #:F32.8-vsum
     #:f32.8-non-temporal-aref #:f32.8-non-temporal-row-major-aref
     ;; f64.4
     #:f64.4-reverse
     #:f64.4-vdot
     #:f64.4-vsum
     #:f64.4-non-temporal-aref #:f64.4-non-temporal-row-major-aref
     ;; u8.16
     ;; u16.8
     #:u16.8-blend
     ;; u32.4
     #:u32.4-shiftl
     #:u32.4-shiftr
     #:u32.4-blend
     ;; u64.2
     #:u64.2-shiftl
     #:u64.2-shiftr
     ;; s8.16
     ;; s16.8
     #:s16.8-blend
     ;; s32.4
     #:s32.4-blend
     #:s32.4-shiftl
     #:s32.4-shiftr
     #:s32.4-blend
     ;; s64.2
     #:s64.2-shiftl
     #:s64.2-shiftr
     ;; u8.32
     #:u8.32-and
     #:u8.32-or
     #:u8.32-xor
     #:u8.32-andnot
     #:u8.32-not
     #:u8.32-max
     #:u8.32-min
     #:u8.32+
     #:u8.32-
     #:u8.32=
     #:u8.32/=
     #:u8.32>
     #:u8.32<
     #:u8.32>=
     #:u8.32<=
     #:u8.32-avg
     #:u8.32-packus
     #:u8.32-permute128
     #:u8.32-extract128
     #:u8.32-insert128
     #:u8.32-non-temporal-aref #:u8.32-non-temporal-row-major-aref
     ;; u16.16
     #:u16.16-from-u8.16
     #:u16.16-and
     #:u16.16-or
     #:u16.16-xor
     #:u16.16-andnot
     #:u16.16-not
     #:u16.16-max
     #:u16.16-min
     #:u16.16+
     #:u16.16-
     #:s16.16-mulhi
     #:u16.16=
     #:u16.16/=
     #:u16.16>
     #:u16.16<
     #:u16.16>=
     #:u16.16<=
     #:u16.16-shiftl
     #:u16.16-shiftr
     #:u16.16-avg
     #:u16.16-packus
     #:u16.16-blend
     #:u16.16-extract128
     #:u16.16-insert128
     #:u16.16-permute128
     #:u16.16-non-temporal-aref #:u16.16-non-temporal-row-major-aref
     ;; u32.8
     #:u32.8-from-u16.8
     #:u32.8-from-u8.16
     #:u32.8-and
     #:u32.8-or
     #:u32.8-xor
     #:u32.8-andnot
     #:u32.8-not
     #:u32.8-max
     #:u32.8-min
     #:u32.8+
     #:u32.8-
     #:u32.8=
     #:u32.8/=
     #:u32.8>
     #:u32.8<
     #:u32.8>=
     #:u32.8<=
     #:u32.8-shiftl
     #:u32.8-shiftr
     #:u32.8-blend
     #:u32.8-extract128
     #:u32.8-insert128
     #:u32.8-permute128
     #:u32.8-incf
     #:u32.8-decf
     ;; u64.4
     #:u64.4-from-u16.8
     #:u64.4-from-u32.4
     #:u64.4-from-u8.16
     #:u64.4-and
     #:u64.4-or
     #:u64.4-xor
     #:u64.4-andnot
     #:u64.4-not
     #:u64.4+
     #:u64.4-
     #:u64.4-mul
     #:u64.4=
     #:u64.4/=
     #:u64.4>
     #:u64.4<
     #:u64.4>=
     #:u64.4<=
     #:u64.4-shiftl
     #:u64.4-shiftr
     #:u64.4-extract128
     #:u64.4-insert128
     #:u64.4-permute128
     #:u64.4-incf
     #:u64.4-decf
     ;; s8.32
     #:s8.32-and
     #:s8.32-or
     #:s8.32-xor
     #:s8.32-andnot
     #:s8.32-not
     #:s8.32-max
     #:s8.32-max
     #:s8.32+
     #:s8.32-
     #:s32.8-mullo
     #:s8.32=
     #:s8.32/=
     #:s8.32>
     #:s8.32<
     #:s8.32>=
     #:s8.32<=
     #:s8.32-abs
     #:s8.32-packs
     #:s8.32-shuffle
     #:s8.32-sign
     #:s8.32-extract128
     #:s8.32-insert128
     #:s8.32-permute128
     ;; s16.16
     #:s16.16-from-s8.16
     #:s16.16-from-u8.16
     #:s16.16-and
     #:s16.16-or
     #:s16.16-xor
     #:s16.16-andnot
     #:s16.16-not
     #:s16.16-max
     #:s16.16-min
     #:s16.16+
     #:s16.16-
     #:s16.16-mulhi
     #:s16.16-mullo
     #:s16.16-mulhrs
     #:s16.16=
     #:s16.16/=
     #:s16.16>
     #:s16.16<
     #:s16.16>=
     #:s16.16<=
     #:s16.16-abs
     #:s16.16-hadd
     #:s16.16-hadds
     #:s16.16-madd
     #:s16.16-maddubs
     #:s16.16-hsub
     #:s16.16-hsubs
     #:s16.16-packs
     #:s16.16-unpackhi
     #:s16.16-unpacklo
     #:s16.16-shiftl
     #:s16.16-shiftr
     #:s16.16-sign
     #:s16.16-blend
     #:s16.16-extract128
     #:s16.16-insert128
     #:s16.16-permute128
     ;; s32.8
     #:s32.8-from-s16.8
     #:s32.8-from-u16.8
     #:s32.8-from-s8.16
     #:s32.8-from-u8.16
     #:s32.8-and
     #:s32.8-or
     #:s32.8-xor
     #:s32.8-andnot
     #:s32.8-not
     #:s32.8-max
     #:s32.8-min
     #:s32.8+
     #:s32.8-
     #:s32.8-mullo
     #:s32.8=
     #:s32.8/=
     #:s32.8>
     #:s32.8<
     #:s32.8>=
     #:s32.8<=
     #:s32.8-abs
     #:s32.8-hadd
     #:s32.8-hsub
     #:s32.8-shiftl
     #:s32.8-shiftr
     #:s32.8-sign
     #:s32.8-blend
     #:s32.8-extract128
     #:s32.8-insert128
     #:s32.8-permute128
     #:s32.8-incf
     #:s32.8-decf
     ;; s64.4
     #:s64.4-from-s16.8
     #:s64.4-from-u16.8
     #:s64.4-from-s32.4
     #:s64.4-from-u32.4
     #:s64.4-from-s8.16
     #:s64.4-from-u8.16
     #:s64.4-and
     #:s64.4-or
     #:s64.4-xor
     #:s64.4-andnot
     #:s64.4-not
     #:s64.4+
     #:s64.4-
     #:s64.4=
     #:s64.4/=
     #:s64.4>
     #:s64.4<
     #:s64.4>=
     #:s64.4<=
     #:s64.4-shiftl
     #:s64.4-shiftr
     #:s64.4-unpackhi
     #:s64.4-unpacklo
     #:s64.4-extract128
     #:s64.4-insert128
     #:s64.4-permute128
     #:s64.4-incf
     #:s64.4-decf
     #:s64.4-non-temporal-aref #:s64.4-non-temporal-row-major-aref
     ;; s8.16
     #:s8.16-non-temporal-aref #:s8.16-non-temporal-row-major-aref
     ;; s16.8
     #:s16.8-non-temporal-aref #:s16.8-non-temporal-row-major-aref
     ;; s32.4
     #:s32.4-non-temporal-aref #:s32.4-non-temporal-row-major-aref
     ;; s64.2
     #:s8.32-non-temporal-aref #:s8.32-non-temporal-row-major-aref
     ;; s8.32
     #:s8.32-non-temporal-aref #:s8.32-non-temporal-row-major-aref
     ;; s16.16
     #:s16.16-non-temporal-aref #:s16.16-non-temporal-row-major-aref
     ;; s32.8
     #:s32.8-non-temporal-aref #:s32.8-non-temporal-row-major-aref
     ;; s64.4
     #:s64.4-non-temporal-aref #:s64.4-non-temporal-row-major-aref)))
