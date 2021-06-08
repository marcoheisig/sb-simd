(in-package #:cl-user)

(progn
  (defpackage #:sb-simd
    (:use #:common-lisp)
    #0=
    (:export
     ;; Scalars
     #:u8
     #:u16
     #:u32
     #:u64
     #:s8
     #:s16
     #:s32
     #:s64
     #:f32
     #:f64
     #:c64
     #:c128
     ;; Scalar AREF
     #:u8-aref
     #:u16-aref
     #:u32-aref
     #:u64-aref
     #:s8-aref
     #:s16-aref
     #:s32-aref
     #:s64-aref
     #:f32-aref
     #:f64-aref
     #:c64-aref
     #:c128-aref
     ;; Scalar ROW-MAJOR-AREF
     #:u8-row-major-aref
     #:u16-row-major-aref
     #:u32-row-major-aref
     #:u64-row-major-aref
     #:s8-row-major-aref
     #:s16-row-major-aref
     #:s32-row-major-aref
     #:s64-row-major-aref
     #:f32-row-major-aref
     #:f64-row-major-aref
     #:c64-row-major-aref
     #:c128-row-major-aref
     ;; Vectors
     #:u8vec
     #:u16vec
     #:u32vec
     #:u64vec
     #:s8vec
     #:s16vec
     #:s32vec
     #:s64vec
     #:f32vec
     #:f64vec
     #:c64vec
     #:c128vec
     ;; SIMD Packs
     #:u32.4
     #:u64.2
     #:f32.4
     #:f64.2
     #:u32.8
     #:u64.4
     #:f32.8
     #:f64.4
     #:make-u32.4
     #:make-u64.2
     #:make-f32.4
     #:make-f64.2
     #:make-u32.8
     #:make-u64.4
     #:make-f32.8
     #:make-f64.4
     #:u32.4-values
     #:u64.2-values
     #:f32.4-values
     #:f64.2-values
     #:u32.8-values
     #:u64.4-values
     #:f32.8-values
     #:f64.4-values))

  (defpackage #:sb-simd-sse
    (:use #:common-lisp #:sb-simd)
    #0#
    #1=
    (:export
     ;; f32.4
     #:f32.4-and
     #:f32.4-or
     #:f32.4-xor
     #:f32.4-andnot
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
     #:f32.4-reciprocal
     #:f32.4-rsqrt
     #:f32.4-sqrt
     #:f32-4-aref #:f32-4-row-major-aref
     #:f32-4-non-temporal-aref #:f32-4-non-temporal-row-major-aref))

  (defpackage #:sb-simd-sse2
    (:use #:common-lisp #:sb-simd-sse)
    #0#
    #1#
    #2=
    (:export
     ;; f64.2
     #:f64.2-and
     #:f64.2-or
     #:f64.2-xor
     #:f64.2-andnot
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
     #:f64.2-sqrt
     #:f64.2-aref #:f64.2-row-major-aref
     #:f64.2-non-temporal-aref #:f64.2-non-temporal-row-major-aref
     ;; u32.4
     #:u32.4-and
     #:u32.4-or
     #:u32.4-xor
     #:u32.4-andnot
     #:u32.4+
     #:u32.4-
     #:u32.4-shiftl
     #:u32.4-shiftr
     #:u32.4-not
     #:u32.4-aref #:u32.4-row-major-aref
     #:u32.4-non-temporal-aref #:u32.4-non-temporal-row-major-aref
     ;; u64.2
     #:u64.2-and
     #:u64.2-or
     #:u64.2-xor
     #:u64.2-andnot
     #:u64.2+
     #:u64.2-
     #:u64.2-shiftl
     #:u64.2-shiftr
     #:u64.2-not
     #:u64.2-aref #:u64.2-row-major-aref
     #:u64.2-non-temporal-aref #:u64.2-non-temporal-row-major-aref))

  (defpackage #:sb-simd-sse3
    (:use #:common-lisp #:sb-simd-sse2)
    #0#
    #1#
    #2#
    #3=
    (:export
     #:f32.4-hdup
     #:f32.4-ldup
     #:f64.2-broadcast))

  (defpackage #:sb-simd-ssse3
    (:use #:common-lisp #:sb-simd-sse3)
    #0#
    #1#
    #2#
    #3#
    #4=
    (:export
     #:u32.4-hadd
     #:u32.4-hsub))

  (defpackage #:sb-simd-sse4.1
    (:use #:common-lisp #:sb-simd-ssse3)
    #0#
    #1#
    #2#
    #3#
    #4#
    #5=
    (:export
     #:u64.2=
     #:f32.4-non-temporal-aref #:f32.4-non-temporal-row-major-aref
     #:f64.2-non-temporal-aref #:f64.2-non-temporal-row-major-aref
     #:u32.4-non-temporal-aref #:u32.4-non-temporal-row-major-aref
     #:u64.2-non-temporal-aref #:u64.2-non-temporal-row-major-aref))

  (defpackage #:sb-simd-sse4.2
    (:use #:common-lisp #:sb-simd-sse4.1)
    #0#
    #1#
    #2#
    #3#
    #4#
    #5#
    #6=
    (:export
     #:u64.2>))

  (defpackage #:sb-simd-avx
    (:use #:common-lisp #:sb-simd)
    #0#
    #7=
    (:export
     ;; f32.4
     #:f32.4-from-f64.4
     #:f32.4-and
     #:f32.4-or
     #:f32.4-xor
     #:f32.4-andnot
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
     #:f32.4-hadd
     #:f32.4-hsub
     #:f32.4-reciprocal
     #:f32.4-rsqrt
     #:f32.4-sqrt
     #:f32.4-unpackhi
     #:f32.4-unpacklo
     #:f32.4-aref #:f32.4-row-major-aref
     #:f32.4-non-temporal-aref #:f32.4-non-temporal-row-major-aref
     ;; f64.2
     #:f64.2-and
     #:f64.2-or
     #:f64.2-xor
     #:f64.2-andnot
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
     #:f64.2-hadd
     #:f64.2-hsub
     #:f64.2-sqrt
     #:f64.2-unpackhi
     #:f64.2-unpacklo
     #:f64.2-aref #:f64.2-row-major-aref
     #:f64.2-non-temporal-aref #:f64.2-non-temporal-row-major-aref
     ;; f32.8
     #:f32.8-from-u32.8
     #:f32.8-and
     #:f32.8-or
     #:f32.8-xor
     #:f32.8-andnot
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
     #:f32.8-hadd
     #:f32.8-hsub
     #:f32.8-reciprocal
     #:f32.8-rsqrt
     #:f32.8-sqrt
     #:f32.8-unpackhi
     #:f32.8-unpacklo
     #:f32.8-aref #:f32.8-row-major-aref
     #:f32.8-non-temporal-aref #:f32.8-non-temporal-row-major-aref
     ;; f64.4
     #:f64.4-from-f32.4
     #:f64.4-from-u32.4
     #:f64.4-and
     #:f64.4-or
     #:f64.4-xor
     #:f64.4-andnot
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
     #:f64.4-hadd
     #:f64.4-hsub
     #:f64.4-sqrt
     #:f64.4-unpackhi
     #:f64.4-unpacklo
     #:f64.4-aref #:f64.4-row-major-aref
     #:f64.4-non-temporal-aref #:f64.4-non-temporal-row-major-aref
     ;; u32.4
     #:u32.4-and
     #:u32.4-or
     #:u32.4-xor
     #:u32.4-andnot
     #:u32.4-not
     #:u32.4-aref #:u32.4-row-major-aref
     #:u32.4-non-temporal-aref #:u32.4-non-temporal-row-major-aref
     ;; u64.2
     #:u64.2-and
     #:u64.2-or
     #:u64.2-xor
     #:u64.2-andnot
     #:u64.2-not
     #:u64.2-aref #:u64.2-row-major-aref
     #:u64.2-non-temporal-aref #:u64.2-non-temporal-row-major-aref
     ;; u32.8
     #:u32.8-from-f32.8
     #:u32.8-and
     #:u32.8-or
     #:u32.8-xor
     #:u32.8-andnot
     #:u32.8-not
     #:u32.8-aref #:u32.8-row-major-aref
     #:u32.8-non-temporal-aref #:u32.8-non-temporal-row-major-aref
     ;; u64.4
     #:u64.4-and
     #:u64.4-or
     #:u64.4-xor
     #:u64.4-andnot
     #:u64.4-not
     #:u64.4-aref #:u64.4-row-major-aref
     #:u64.4-non-temporal-aref #:u64.4-non-temporal-row-major-aref))

  (defpackage #:sb-simd-avx2
    (:use #:common-lisp #:sb-simd-avx)
    #0#
    #7#
    #8=
    (:export
     ;; f32.4
     #:f32.4-broadcast
     #:f32.4-non-temporal-aref #:f32.4-non-temporal-row-major-aref
     ;; f64.2
     #:f64.2-broadcast
     #:f64.2-non-temporal-aref #:f64.2-non-temporal-row-major-aref
     ;; f32.8
     #:f32.8-broadcast
     #:f32.8-non-temporal-aref #:f32.8-non-temporal-row-major-aref
     ;; f64.4
     #:f64.4-broadcast
     #:f64.4-non-temporal-aref #:f64.4-non-temporal-row-major-aref
     ;; u32.4
     #:u32.4-max
     #:u32.4+
     #:u32.4-
     #:u32.4=
     #:u32.4>
     #:u32.4-shiftl
     #:u32.4-shiftr
     #:u32.4-unpackhi
     #:u32.4-unpacklo
     #:u32.4-broadcast
     #:u32.4-non-temporal-aref #:u32.4-non-temporal-row-major-aref
     ;; u64.2
     #:two-arg-u64.2+
     #:two-arg-u64.2-
     #:two-arg-u64.2=
     #:two-arg-u64.2>
     #:u64.2-shiftl
     #:u64.2-shiftr
     #:u64.2-unpackhi
     #:u64.2-unpacklo
     #:u64.2-broadcast
     #:u64.2-non-temporal-aref #:u64.2-non-temporal-row-major-aref
     ;; u32.8
     #:u32.8-max
     #:u32.8+
     #:u32.8-
     #:u32.8=
     #:u32.8>
     #:u32.8-shiftl
     #:u32.8-shiftr
     #:u32.8-unpackhi
     #:u32.8-unpacklo
     #:u32.8-broadcast
     #:u32.8-non-temporal-aref #:u32.8-non-temporal-row-major-aref
     ;; u64.4
     #:u64.4+
     #:u64.4-
     #:u64.4=
     #:u64.4>
     #:u64.4-shiftl
     #:u64.4-shiftr
     #:u64.4-unpackhi
     #:u64.4-unpacklo
     #:u64.4-broadcast
     #:u64.4-non-temporal-aref #:u64.4-non-temporal-row-major-aref)))
