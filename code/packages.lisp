(in-package #:cl-user)

(defpackage #:sb-simd
  (:use #:common-lisp)
  (:export
   #:U64.2+
   #:U32.4-
   #:F32.8
   #:U32.4*
   #:F32.8-
   #:F32.4
   #:F32.4-
   #:U2
   #:U32.8
   #:F32.8*
   #:U64.4*
   #:U32.8/
   #:U16
   #:F64.2-
   #:U1
   #:F64.2-VALUES
   #:S64
   #:MAKE-U32.8
   #:F64.4-
   #:MAKE-F32.8
   #:C128
   #:F32.8-VALUES
   #:U32.4/
   #:MAKE-F64.4
   #:U32.4+
   #:U64.2*
   #:U32.8*
   #:U4
   #:F32.8/
   #:U8
   #:F32.4-VALUES
   #:F64.2
   #:U64.
   #:MAKE-U32.4
   #:F64
   #:U64.2/
   #:S16
   #:F64.4/
   #:F32.4/
   #:U64.2-
   #:F32.4+
   #:F64.4+
   #:F64.4-VALUES
   #:U32.4
   #:U64.4+
   #:F64.4
   #:U32
   #:U32.4-VALUES
   #:MAKE-F64.2
   #:MAKE-F32.4
   #:F32.4*
   #:MAKE-U64.2
   #:U32.8-
   #:U64.4-
   #:C64
   #:U32.8+
   #:U32.8-VALUES
   #:U64.4/
   #:F32.8+
   #:F64.2/
   #:S32
   #:MAKE-U64.4
   #:F64.4*
   #:F32
   #:U64.4-VALUES
   #:U64.4
   #:S8
   #:F64.2*
   #:U64.2-VALUES
   #:U64
   #:F64.2+
   #:f64.4-incf
   #:f64.2-incf
   #:f32.4-incf
   #:f32.8-incf
   #:u64.4-incf
   #:u64.2-incf
   #:u32.4-incf
   #:u32.8-incf
   #:f64.4-decf
   #:f64.2-decf
   #:f32.4-decf
   #:f32.8-decf
   #:u64.4-decf
   #:u64.2-decf
   #:u32.4-decf
   #:u32.8-decf
   #:f64.4-ref
   #:f64.2-ref
   #:f32.4-ref
   #:f32.8-ref
   #:u64.4-ref
   #:u64.2-ref
   #:u32.4-ref
   #:u32.8-ref
   "(setf f64.4-ref)"
   "(setf f64.2-ref)"
   "(setf f32.8-ref)"
   "(setf f32.4-ref)"
   "(setf u64.4-ref)"
   "(setf u64.2-ref)"
   "(setf u32.8-ref)"
   "(setf u32.4-ref)"
   #:f64.4-hsum
   #:f32.8-hsum
   #:f64.2-hsum
   #:f32.4-hsum
   #:f64.4-vdot
   #:f64.2-vdot
   #:f32.8-vdot
   #:f32.4-vdot
   #:vzeroupper
    ;; . #.(when (find-package '#:sb-simd)
    ;;       (loop for it being the external-symbols of '#:sb-simd
    ;;             collect it))
   ))
