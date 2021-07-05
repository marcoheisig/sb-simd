(in-package #:sb-simd)

;;; Primitives with a :NONE encoding do not define a VOP.  Instead, we
;;; define a pseudo VOP - a regular function that has the name of the VOP
;;; we didn't emit.  Pseudo VOPs are useful for defining instructions that
;;; we would like to have in the instruction set, and that can be expressed
;;; easily in terms of the other VOPs.

(defmacro define-pseudo-vop (name lambda-list &body body)
  (with-accessors ((vop primitive-record-vop)
                   (result-records primitive-record-result-records)
                   (argument-records primitive-record-argument-records)
                   (instruction-set primitive-record-instruction-set))
      (find-instruction-record name)
    (assert (= (length lambda-list)
               (length argument-records)))
    (assert (null (intersection lambda-list lambda-list-keywords)))
    (when (instruction-set-available-p instruction-set)
      `(define-inline ,vop ,lambda-list
         (declare (optimize (safety 0) (debug 0)))
         (declare
          ,@(loop for argument-record in argument-records
                  for argument in lambda-list
                  collect `(type ,(value-record-name argument-record) ,argument)))
         (the (values ,@(mapcar #'value-record-name result-records) &optional)
              (progn ,@body))))))

;;; Integer Packers

(defmacro define-u64-packer (name scalar-record-name)
  (with-accessors ((type scalar-record-name)
                   (bits scalar-record-bits))
      (find-value-record scalar-record-name)
    (let ((args (argument-symbols (the integer (/ 64 bits)))))
      `(define-inline ,name ,args
         (declare (type ,type ,@args))
         (logior
          ,@(loop for arg in args
                  for position from 0 by bits
                  collect `(dpb ,arg (byte ,bits ,position) 0)))))))

(define-u64-packer u64-from-u8s u8)

(define-u64-packer u64-from-u16s u16)

(define-u64-packer u64-from-u32s u32)

(define-u64-packer u64-from-s8s s8)

(define-u64-packer u64-from-s16s s16)

(define-u64-packer u64-from-s32s s32)

(define-u64-packer u64-from-s64 s64)

;;; Integer Unpackers

(defmacro define-u64-unpacker (name scalar-record-name)
  (with-accessors ((type scalar-record-name)
                   (bits scalar-record-bits))
      (find-value-record scalar-record-name)
    (let ((args (argument-symbols (the integer (/ 64 bits))))
          (unsigned (subtypep type 'unsigned-byte)))
      `(define-inline ,name (x)
         (declare (u64 x))
         (values
          ,@ (loop for arg in args
                for position from 0 by bits
                collect
                (if unsigned
                    `(ldb (byte ,bits ,position) x)
                    `(if (logbitp ,(+ position bits -1) x)
                         (ash (dpb x (byte ,(1- bits) ,position) -1) ,(- position))
                         (ldb (byte ,(1- bits) ,position) x)))))))))

(define-u64-unpacker u8s-from-u64 u8)

(define-u64-unpacker u16s-from-u64 u16)

(define-u64-unpacker u32s-from-u64 u32)

(define-u64-unpacker s8s-from-u64 s8)

(define-u64-unpacker s16s-from-u64 s16)

(define-u64-unpacker s32s-from-u64 s32)

(define-u64-unpacker s64-from-u64 s64)

(in-package #:sb-simd-sse)

(sb-simd::define-pseudo-vop make-f32.4 (a b c d)
  (%f32.4-unpacklo
   (%f32.4-unpacklo
    (%f32.4!-from-f32 a)
    (%f32.4!-from-f32 c))
   (%f32.4-unpacklo
    (%f32.4!-from-f32 b)
    (%f32.4!-from-f32 d))))

(sb-simd::define-pseudo-vop f32.4-values (x)
  (values
   (%f32!-from-p128 x)
   (%f32!-from-p128 (%f32.4-shuffle x 1))
   (%f32!-from-p128 (%f32.4-shuffle x 2))
   (%f32!-from-p128 (%f32.4-shuffle x 3))))

(sb-simd::define-pseudo-vop f32.4-not (a)
  (%f32.4-andnot
   a
   (%make-f32.4 +f32-true+ +f32-true+ +f32-true+ +f32-true+)))

(in-package #:sb-simd-sse2)

(sb-simd::define-pseudo-vop make-f64.2 (a b)
  (%f64.2-unpacklo
   (%f64.2!-from-f64 a)
   (%f64.2!-from-f64 b)))

(sb-simd::define-pseudo-vop f64.2-values (x)
  (values
   (%f64!-from-p128 x)
   (%f64!-from-p128 (%f64.2-shuffle x 1))))

(sb-simd::define-pseudo-vop f64.2-not (a)
  (%f64.2-andnot
   a
   (%make-f64.2 +f64-true+ +f64-true+)))

(sb-simd::define-pseudo-vop make-u8.16 (a b c d e f g h i j k l m n o p)
  (%u8.16-unpacklo
   (%u8.16!-from-p128 (%u64.2!-from-u64 (u64-from-u8s a c e g i k m o)))
   (%u8.16!-from-p128 (%u64.2!-from-u64 (u64-from-u8s b d f h j l n p)))))

(sb-simd::define-pseudo-vop u8.16-values (x)
  (multiple-value-call #'values
    (u8s-from-u64 (%u64!-from-p128 x))
    (u8s-from-u64 (%u64!-from-p128 (%u32.4-shuffle (%u32.4!-from-p128 x) #b00001011)))))

(sb-simd::define-pseudo-vop u8.16-not (a)
  (%u8.16-andnot
   a
   (%make-u8.16 +u8-true+ +u8-true+ +u8-true+ +u8-true+
                +u8-true+ +u8-true+ +u8-true+ +u8-true+
                +u8-true+ +u8-true+ +u8-true+ +u8-true+
                +u8-true+ +u8-true+ +u8-true+ +u8-true+)))

(sb-simd::define-pseudo-vop make-u16.8 (a b c d e f g h)
  (%u16.8-unpacklo
   (%u16.8!-from-p128 (%u64.2!-from-u64 (u64-from-u16s a c e g)))
   (%u16.8!-from-p128 (%u64.2!-from-u64 (u64-from-u16s b d f h)))))

(sb-simd::define-pseudo-vop u16.8-values (x)
  (multiple-value-call #'values
    (u16s-from-u64 (%u64!-from-p128 x))
    (u16s-from-u64 (%u64!-from-p128 (%u32.4-shuffle (%u32.4!-from-p128 x) #b00001011)))))

(sb-simd::define-pseudo-vop u16.8-not (a)
  (%u16.8-andnot
   a
   (%make-u16.8 +u16-true+ +u16-true+ +u16-true+ +u16-true+
                +u16-true+ +u16-true+ +u16-true+ +u16-true+)))

(sb-simd::define-pseudo-vop make-u32.4 (a b c d)
  (%u32.4-unpacklo
   (%u32.4!-from-p128 (%u64.2!-from-u64 (u64-from-u32s a c)))
   (%u32.4!-from-p128 (%u64.2!-from-u64 (u64-from-u32s b d)))))

(sb-simd::define-pseudo-vop u32.4-values (x)
  (multiple-value-call #'values
    (u32s-from-u64 (%u64!-from-p128 x))
    (u32s-from-u64 (%u64!-from-p128 (%u32.4-shuffle (%u32.4!-from-p128 x) #b00001011)))))

(sb-simd::define-pseudo-vop u32.4-not (a)
  (%u32.4-andnot
   a
   (%make-u32.4 +u32-true+ +u32-true+ +u32-true+ +u32-true+)))

(sb-simd::define-pseudo-vop make-u64.2 (a b)
  (%u64.2-unpacklo
   (%u64.2!-from-u64 a)
   (%u64.2!-from-u64 b)))

(sb-simd::define-pseudo-vop u64.2-values (x)
  (values
   (%u64!-from-p128 x)
   (%u64!-from-p128 (%u32.4-shuffle (%u32.4!-from-p128 x) #b00001011))))

(sb-simd::define-pseudo-vop u64.2-not (a)
  (%u64.2-andnot
   a
   (%make-u64.2 +u64-true+ +u64-true+)))

(sb-simd::define-pseudo-vop make-s8.16 (a b c d e f g h i j k l m n o p)
  (%s8.16-unpacklo
   (%s8.16!-from-p128 (%u64.2!-from-u64 (u64-from-s8s a c e g i k m o)))
   (%s8.16!-from-p128 (%u64.2!-from-u64 (u64-from-s8s b d f h j l n p)))))

(sb-simd::define-pseudo-vop s8.16-not (a)
  (%s8.16-andnot
   a
   (%make-s8.16 +s8-true+ +s8-true+ +s8-true+ +s8-true+
                +s8-true+ +s8-true+ +s8-true+ +s8-true+
                +s8-true+ +s8-true+ +s8-true+ +s8-true+
                +s8-true+ +s8-true+ +s8-true+ +s8-true+)))

(sb-simd::define-pseudo-vop make-s16.8 (a b c d e f g h)
  (%s16.8-unpacklo
   (%s16.8!-from-p128 (%u64.2!-from-u64 (u64-from-s16s a c e g)))
   (%s16.8!-from-p128 (%u64.2!-from-u64 (u64-from-s16s b d f h)))))

(sb-simd::define-pseudo-vop s8.16-values (x)
  (multiple-value-call #'values
    (s8s-from-u64 (%u64!-from-p128 x))
    (s8s-from-u64 (%u64!-from-p128 (%u32.4-shuffle (%u32.4!-from-p128 x) #b00001011)))))

(sb-simd::define-pseudo-vop s16.8-not (a)
  (%s16.8-andnot
   a
   (%make-s16.8 +s16-true+ +s16-true+ +s16-true+ +s16-true+
                +s16-true+ +s16-true+ +s16-true+ +s16-true+)))

(sb-simd::define-pseudo-vop s16.8-values (x)
  (multiple-value-call #'values
    (s16s-from-u64 (%u64!-from-p128 x))
    (s16s-from-u64 (%u64!-from-p128 (%u32.4-shuffle (%u32.4!-from-p128 x) #b00001011)))))

(sb-simd::define-pseudo-vop make-s32.4 (a b c d)
  (%s32.4-unpacklo
   (%s32.4!-from-p128 (%u64.2!-from-u64 (u64-from-s32s a c)))
   (%s32.4!-from-p128 (%u64.2!-from-u64 (u64-from-s32s b d)))))

(sb-simd::define-pseudo-vop s32.4-values (x)
  (multiple-value-call #'values
    (s32s-from-u64 (%u64!-from-p128 x))
    (s32s-from-u64 (%u64!-from-p128 (%u32.4-shuffle (%u32.4!-from-p128 x) #b00001011)))))

(sb-simd::define-pseudo-vop s32.4-not (a)
  (%s32.4-andnot
   a
   (%make-s32.4 +s32-true+ +s32-true+ +s32-true+ +s32-true+)))

(sb-simd::define-pseudo-vop make-s64.2 (a b)
  (%s64.2-unpacklo
   (%s64.2!-from-p128 (%u64.2!-from-u64 (u64-from-s64 a)))
   (%s64.2!-from-p128 (%u64.2!-from-u64 (u64-from-s64 b)))))

(sb-simd::define-pseudo-vop s64.2-values (x)
  (values
   (s64-from-u64 (%u64!-from-p128 x))
   (s64-from-u64 (%u64!-from-p128 (%u32.4-shuffle (%u32.4!-from-p128 x) #b00001011)))))

(sb-simd::define-pseudo-vop s64.2-not (a)
  (%s64.2-andnot
   a
   (%make-s64.2 +s64-true+ +s64-true+)))

(in-package #:sb-simd-sse4.1)

(sb-simd::define-pseudo-vop two-arg-u64.2/= (a b)
  (sb-simd-sse2::%u64.2-not
   (%two-arg-u64.2= a b)))

(sb-simd::define-pseudo-vop two-arg-s64.2/= (a b)
  (sb-simd-sse2::%u64.2-not
   (%two-arg-s64.2= a b)))

(in-package #:sb-simd-sse4.2)

(sb-simd::define-pseudo-vop two-arg-u64.2< (a b)
  (%two-arg-u64.2> b a))

(sb-simd::define-pseudo-vop two-arg-u64.2>= (a b)
  (sb-simd-sse2::%u64.2-not
   (%two-arg-u64.2< a b)))

(sb-simd::define-pseudo-vop two-arg-u64.2<= (a b)
  (sb-simd-sse2::%u64.2-not
   (%two-arg-u64.2> a b)))

(in-package #:sb-simd-avx)

(sb-simd::define-pseudo-vop make-f32.4 (a b c d)
  (%f32.4-unpacklo
   (%f32.4-unpacklo
    (%f32.4!-from-f32 a)
    (%f32.4!-from-f32 c))
   (%f32.4-unpacklo
    (%f32.4!-from-f32 b)
    (%f32.4!-from-f32 d))))

(sb-simd::define-pseudo-vop f32.4-values (x)
  (values
   (%f32!-from-p128 x)
   (%f32!-from-p128 (%f32.4-permute x 1))
   (%f32!-from-p128 (%f32.4-permute x 2))
   (%f32!-from-p128 (%f32.4-permute x 3))))

(sb-simd::define-pseudo-vop f32.4-not (a)
  (%f32.4-andnot
   a
   (%make-f32.4 +f32-true+ +f32-true+ +f32-true+ +f32-true+)))

(sb-simd::define-pseudo-vop make-f64.2 (a b)
  (%f64.2-unpacklo
   (%f64.2!-from-f64 a)
   (%f64.2!-from-f64 b)))

(sb-simd::define-pseudo-vop f64.2-values (x)
  (values
   (%f64!-from-p128 x)
   (%f64!-from-p128 (%f64.2-permute x 1))))

(sb-simd::define-pseudo-vop f64.2-not (a)
  (%f64.2-andnot
   a
   (%make-f64.2 +f64-true+ +f64-true+)))

(sb-simd::define-pseudo-vop make-f32.8 (a b c d e f g h)
  (let ((lo (%make-f32.4 a b c d))
        (hi (%make-f32.4 e f g h)))
    (%f32.8-insert128 (%f32.8!-from-p128 lo) hi 1)))

(sb-simd::define-pseudo-vop f32.8-values (x)
  (multiple-value-call #'values
    (%f32.4-values (%f32.4!-from-p256 x))
    (%f32.4-values (%f32.8-extract128 x 1))))

(sb-simd::define-pseudo-vop f32.8-not (a)
  (%f32.8-andnot
   a
   (%make-f32.8 +f32-true+ +f32-true+ +f32-true+ +f32-true+
                +f32-true+ +f32-true+ +f32-true+ +f32-true+)))

(sb-simd::define-pseudo-vop make-f64.4 (a b c d)
  (let ((lo (%make-f64.2 a b))
        (hi (%make-f64.2 c d)))
    (%f64.4-insert128 (%f64.4!-from-p128 lo) hi 1)))

(sb-simd::define-pseudo-vop f64.4-values (x)
  (multiple-value-call #'values
    (%f64.2-values (%f64.2!-from-p256 x))
    (%f64.2-values (%f64.4-extract128 x 1))))

(sb-simd::define-pseudo-vop f64.4-not (a)
  (%f64.4-andnot
   a
   (%make-f64.4 +f64-true+ +f64-true+ +f64-true+ +f64-true+)))

(sb-simd::define-pseudo-vop f64.4-hsum (x)
  (multiple-value-call #'+
    (%f64.2-values
     (%two-arg-f64.2+
      (%f64.2!-from-p256 x)
      (%f64.4-extract128 x 1)))))

(sb-simd::define-pseudo-vop make-u8.16 (a b c d e f g h i j k l m n o p)
  (%u8.16-unpacklo
   (%u8.16!-from-p128 (%u64.2!-from-u64 (u64-from-u8s a c e g i k m o)))
   (%u8.16!-from-p128 (%u64.2!-from-u64 (u64-from-u8s b d f h j l n p)))))

(sb-simd::define-pseudo-vop u8.16-values (x)
  (multiple-value-call #'values
    (u8s-from-u64 (%u64!-from-p128 x))
    (u8s-from-u64 (%u64!-from-p128 (%u64.2-permute (%u64.2!-from-p128 x) 1)))))

(sb-simd::define-pseudo-vop u8.16-not (a)
  (%u8.16-andnot
   a
   (%make-u8.16 +u8-true+ +u8-true+ +u8-true+ +u8-true+
                +u8-true+ +u8-true+ +u8-true+ +u8-true+
                +u8-true+ +u8-true+ +u8-true+ +u8-true+
                +u8-true+ +u8-true+ +u8-true+ +u8-true+)))

(sb-simd::define-pseudo-vop two-arg-u8.16/= (a b)
  (%u8.16-not
   (%two-arg-u8.16= a b)))

(sb-simd::define-pseudo-vop two-arg-u8.16< (a b)
  (%two-arg-u8.16> b a))

(sb-simd::define-pseudo-vop two-arg-u8.16>= (a b)
  (%u8.16-not
   (%two-arg-u8.16< a b)))

(sb-simd::define-pseudo-vop two-arg-u8.16<= (a b)
  (%u8.16-not
   (%two-arg-u8.16> a b)))

(sb-simd::define-pseudo-vop make-u16.8 (a b c d e f g h)
  (%u16.8-unpacklo
   (%u16.8!-from-p128 (%u64.2!-from-u64 (u64-from-u16s a c e g)))
   (%u16.8!-from-p128 (%u64.2!-from-u64 (u64-from-u16s b d f h)))))

(sb-simd::define-pseudo-vop u16.8-values (x)
  (multiple-value-call #'values
    (u16s-from-u64 (%u64!-from-p128 x))
    (u16s-from-u64 (%u64!-from-p128 (%u64.2-permute (%u64.2!-from-p128 x) 1)))))

(sb-simd::define-pseudo-vop u16.8-not (a)
  (%u16.8-andnot
   a
   (%make-u16.8 +u16-true+ +u16-true+ +u16-true+ +u16-true+
                +u16-true+ +u16-true+ +u16-true+ +u16-true+)))

(sb-simd::define-pseudo-vop two-arg-u16.8/= (a b)
  (%u16.8-not
   (%two-arg-u16.8= a b)))

(sb-simd::define-pseudo-vop two-arg-u16.8< (a b)
  (%two-arg-u16.8> b a))

(sb-simd::define-pseudo-vop two-arg-u16.8>= (a b)
  (%u16.8-not
   (%two-arg-u16.8< a b)))

(sb-simd::define-pseudo-vop two-arg-u16.8<= (a b)
  (%u16.8-not
   (%two-arg-u16.8> a b)))

(sb-simd::define-pseudo-vop make-u32.4 (a b c d)
  (%u32.4-unpacklo
   (%u32.4!-from-p128 (%u64.2!-from-u64 (u64-from-u32s a c)))
   (%u32.4!-from-p128 (%u64.2!-from-u64 (u64-from-u32s b d)))))

(sb-simd::define-pseudo-vop u32.4-values (x)
  (multiple-value-call #'values
    (u32s-from-u64 (%u64!-from-p128 x))
    (u32s-from-u64 (%u64!-from-p128 (%u64.2-permute (%u64.2!-from-p128 x) 1)))))

(sb-simd::define-pseudo-vop u32.4-not (a)
  (%u32.4-andnot
   a
   (%make-u32.4 +u32-true+ +u32-true+ +u32-true+ +u32-true+)))

(sb-simd::define-pseudo-vop two-arg-u32.4/= (a b)
  (%u32.4-not
   (%two-arg-u32.4= a b)))

(sb-simd::define-pseudo-vop two-arg-u32.4< (a b)
  (%two-arg-u32.4> b a))

(sb-simd::define-pseudo-vop two-arg-u32.4>= (a b)
  (%u32.4-not
   (%two-arg-u32.4< a b)))

(sb-simd::define-pseudo-vop two-arg-u32.4<= (a b)
  (%u32.4-not
   (%two-arg-u32.4> a b)))

(sb-simd::define-pseudo-vop make-u64.2 (a b)
  (%u64.2-unpacklo
   (%u64.2!-from-u64 a)
   (%u64.2!-from-u64 b)))

(sb-simd::define-pseudo-vop u64.2-values (x)
  (multiple-value-call #'values
    (%u64!-from-p128 x)
    (%u64!-from-p128 (%u64.2-permute (%u64.2!-from-p128 x) 1))))

(sb-simd::define-pseudo-vop u64.2-not (a)
  (%u64.2-andnot
   a
   (%make-u64.2 +u64-true+ +u64-true+)))

(sb-simd::define-pseudo-vop two-arg-u64.2/= (a b)
  (%u64.2-not
   (%two-arg-u64.2= a b)))

(sb-simd::define-pseudo-vop two-arg-u64.2< (a b)
  (%two-arg-u64.2> b a))

(sb-simd::define-pseudo-vop two-arg-u64.2>= (a b)
  (sb-simd-avx::%u64.2-not
   (%two-arg-u64.2< a b)))

(sb-simd::define-pseudo-vop two-arg-u64.2<= (a b)
  (sb-simd-avx::%u64.2-not
   (%two-arg-u64.2> a b)))

(sb-simd::define-pseudo-vop make-u8.32
    (u01 u02 u03 u04 u05 u06 u07 u08 u09 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19 u20 u21 u22 u23 u24 u25 u26 u27 u28 u29 u30 u31 u32)
  (let ((lo (%make-u8.16 u01 u02 u03 u04 u05 u06 u07 u08 u09 u10 u11 u12 u13 u14 u15 u16))
        (hi (%make-u8.16 u17 u18 u19 u20 u21 u22 u23 u24 u25 u26 u27 u28 u29 u30 u31 u32)))
    (%u8.32-insert128 (%u8.32!-from-p128 lo) hi 1)))

(sb-simd::define-pseudo-vop u8.32-values (x)
  (multiple-value-call #'values
    (%u8.16-values (%u8.16!-from-p256 x))
    (%u8.16-values (%u8.32-extract128 x 1))))

(sb-simd::define-pseudo-vop make-u16.16 (a b c d e f g h i j k l m n o p)
  (let ((lo (%make-u16.8 a b c d e f g h))
        (hi (%make-u16.8 i j k l m n o p)))
    (%u16.16-insert128 (%u16.16!-from-p128 lo) hi 1)))

(sb-simd::define-pseudo-vop u16.16-values (x)
  (multiple-value-call #'values
    (%u16.8-values (%u16.8!-from-p256 x))
    (%u16.8-values (%u16.16-extract128 x 1))))

(sb-simd::define-pseudo-vop make-u32.8 (a b c d e f g h)
  (let ((lo (%make-u32.4 a b c d))
        (hi (%make-u32.4 e f g h)))
    (%u32.8-insert128 (%u32.8!-from-p128 lo) hi 1)))

(sb-simd::define-pseudo-vop u32.8-values (x)
  (multiple-value-call #'values
    (%u32.4-values (%u32.4!-from-p256 x))
    (%u32.4-values (%u32.8-extract128 x 1))))

(sb-simd::define-pseudo-vop make-u64.4 (a b c d)
  (let ((lo (%make-u64.2 a b))
        (hi (%make-u64.2 c d)))
    (%u64.4-insert128 (%u64.4!-from-p128 lo) hi 1)))

(sb-simd::define-pseudo-vop u64.4-values (x)
  (multiple-value-call #'values
    (%u64.2-values (%u64.2!-from-p256 x))
    (%u64.2-values (%u64.4-extract128 x 1))))

(sb-simd::define-pseudo-vop make-s8.16 (a b c d e f g h i j k l m n o p)
  (%s8.16-unpacklo
   (%s8.16!-from-p128 (%u64.2!-from-u64 (u64-from-s8s a c e g i k m o)))
   (%s8.16!-from-p128 (%u64.2!-from-u64 (u64-from-s8s b d f h j l n p)))))

(sb-simd::define-pseudo-vop s8.16-values (x)
  (multiple-value-call #'values
    (s8s-from-u64 (%u64!-from-p128 x))
    (s8s-from-u64 (%u64!-from-p128 (%u64.2-permute (%u64.2!-from-p128 x) 1)))))

(sb-simd::define-pseudo-vop s8.16-not (a)
  (%s8.16-andnot
   a
   (%make-s8.16 +s8-true+ +s8-true+ +s8-true+ +s8-true+
                +s8-true+ +s8-true+ +s8-true+ +s8-true+
                +s8-true+ +s8-true+ +s8-true+ +s8-true+
                +s8-true+ +s8-true+ +s8-true+ +s8-true+)))

(sb-simd::define-pseudo-vop two-arg-s8.16/= (a b)
  (%s8.16-not
   (%two-arg-s8.16= a b)))

(sb-simd::define-pseudo-vop two-arg-s8.16< (a b)
  (%two-arg-s8.16> b a))

(sb-simd::define-pseudo-vop two-arg-s8.16>= (a b)
  (%s8.16-not
   (%two-arg-s8.16< a b)))

(sb-simd::define-pseudo-vop two-arg-s8.16<= (a b)
  (%s8.16-not
   (%two-arg-s8.16> a b)))

(sb-simd::define-pseudo-vop make-s16.8 (a b c d e f g h)
  (%s16.8-unpacklo
   (%s16.8!-from-p128 (%u64.2!-from-u64 (u64-from-s16s a c e g)))
   (%s16.8!-from-p128 (%u64.2!-from-u64 (u64-from-s16s b d f h)))))

(sb-simd::define-pseudo-vop s16.8-values (x)
  (multiple-value-call #'values
    (s16s-from-u64 (%u64!-from-p128 x))
    (s16s-from-u64 (%u64!-from-p128 (%u64.2-permute (%u64.2!-from-p128 x) 1)))))

(sb-simd::define-pseudo-vop s16.8-not (a)
  (%s16.8-andnot
   a
   (%make-s16.8 +s16-true+ +s16-true+ +s16-true+ +s16-true+
                +s16-true+ +s16-true+ +s16-true+ +s16-true+)))

(sb-simd::define-pseudo-vop two-arg-s16.8/= (a b)
  (%s16.8-not
   (%two-arg-s16.8= a b)))

(sb-simd::define-pseudo-vop two-arg-s16.8< (a b)
  (%two-arg-s16.8> b a))

(sb-simd::define-pseudo-vop two-arg-s16.8>= (a b)
  (%s16.8-not
   (%two-arg-s16.8< a b)))

(sb-simd::define-pseudo-vop two-arg-s16.8<= (a b)
  (%s16.8-not
   (%two-arg-s16.8> a b)))

(sb-simd::define-pseudo-vop make-s32.4 (a b c d)
  (%s32.4-unpacklo
   (%s32.4!-from-p128 (%u64.2!-from-u64 (u64-from-s32s a c)))
   (%s32.4!-from-p128 (%u64.2!-from-u64 (u64-from-s32s b d)))))

(sb-simd::define-pseudo-vop s32.4-values (x)
  (multiple-value-call #'values
    (s32s-from-u64 (%u64!-from-p128 x))
    (s32s-from-u64 (%u64!-from-p128 (%u64.2-permute (%u64.2!-from-p128 x) 1)))))

(sb-simd::define-pseudo-vop s32.4-not (a)
  (%s32.4-andnot
   a
   (%make-s32.4 +s32-true+ +s32-true+ +s32-true+ +s32-true+)))

(sb-simd::define-pseudo-vop two-arg-s32.4/= (a b)
  (%s32.4-not
   (%two-arg-s32.4= a b)))

(sb-simd::define-pseudo-vop two-arg-s32.4< (a b)
  (%two-arg-s32.4> b a))

(sb-simd::define-pseudo-vop two-arg-s32.4>= (a b)
  (%s32.4-not
   (%two-arg-s32.4< a b)))

(sb-simd::define-pseudo-vop two-arg-s32.4<= (a b)
  (%s32.4-not
   (%two-arg-s32.4> a b)))

(sb-simd::define-pseudo-vop make-s64.2 (a b)
  (%s64.2-unpacklo
   (%s64.2!-from-p128 (%u64.2!-from-u64 (u64-from-s64 a)))
   (%s64.2!-from-p128 (%u64.2!-from-u64 (u64-from-s64 b)))))

(sb-simd::define-pseudo-vop s64.2-values (x)
  (multiple-value-call #'values
    (s64-from-u64 (%u64!-from-p128 x))
    (s64-from-u64 (%u64!-from-p128 (%u64.2-permute (%u64.2!-from-p128 x) 1)))))

(sb-simd::define-pseudo-vop s64.2-not (a)
  (%s64.2-andnot
   a
   (%make-s64.2 +s64-true+ +s64-true+)))

(sb-simd::define-pseudo-vop two-arg-s64.2/= (a b)
  (%s64.2-not
   (%two-arg-s64.2= a b)))

(sb-simd::define-pseudo-vop two-arg-s64.2< (a b)
  (%two-arg-s64.2> b a))

(sb-simd::define-pseudo-vop two-arg-s64.2>= (a b)
  (sb-simd-avx::%s64.2-not
   (%two-arg-s64.2< a b)))

(sb-simd::define-pseudo-vop two-arg-s64.2<= (a b)
  (sb-simd-avx::%s64.2-not
   (%two-arg-s64.2> a b)))

(sb-simd::define-pseudo-vop make-s8.32
    (s01 s02 s03 s04 s05 s06 s07 s08 s09 s10 s11 s12 s13 s14 s15 s16 s17 s18 s19 s20 s21 s22 s23 s24 s25 s26 s27 s28 s29 s30 s31 s32)
  (let ((lo (%make-s8.16 s01 s02 s03 s04 s05 s06 s07 s08 s09 s10 s11 s12 s13 s14 s15 s16))
        (hi (%make-s8.16 s17 s18 s19 s20 s21 s22 s23 s24 s25 s26 s27 s28 s29 s30 s31 s32)))
    (%s8.32-insert128 (%s8.32!-from-p128 lo) hi 1)))

(sb-simd::define-pseudo-vop s8.32-values (x)
  (multiple-value-call #'values
    (%s8.16-values (%s8.16!-from-p256 x))
    (%s8.16-values (%s8.32-extract128 x 1))))

(sb-simd::define-pseudo-vop make-s16.16 (a b c d e f g h i j k l m n o p)
  (let ((lo (%make-s16.8 a b c d e f g h))
        (hi (%make-s16.8 i j k l m n o p)))
    (%s16.16-insert128 (%s16.16!-from-p128 lo) hi 1)))

(sb-simd::define-pseudo-vop s16.16-values (x)
  (multiple-value-call #'values
    (%s16.8-values (%s16.8!-from-p256 x))
    (%s16.8-values (%s16.16-extract128 x 1))))

(sb-simd::define-pseudo-vop make-s32.8 (a b c d e f g h)
  (let ((lo (%make-s32.4 a b c d))
        (hi (%make-s32.4 e f g h)))
    (%s32.8-insert128 (%s32.8!-from-p128 lo) hi 1)))

(sb-simd::define-pseudo-vop s32.8-values (x)
  (multiple-value-call #'values
    (%s32.4-values (%s32.4!-from-p256 x))
    (%s32.4-values (%s32.8-extract128 x 1))))

(sb-simd::define-pseudo-vop make-s64.4 (a b c d)
  (let ((lo (%make-s64.2 a b))
        (hi (%make-s64.2 c d)))
    (%s64.4-insert128 (%s64.4!-from-p128 lo) hi 1)))

(sb-simd::define-pseudo-vop s64.4-values (x)
  (multiple-value-call #'values
    (%s64.2-values (%s64.2!-from-p256 x))
    (%s64.2-values (%s64.4-extract128 x 1))))

(in-package #:sb-simd-avx2)

(sb-simd::define-pseudo-vop make-u8.32
    (u01 u02 u03 u04 u05 u06 u07 u08 u09 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19 u20 u21 u22 u23 u24 u25 u26 u27 u28 u29 u30 u31 u32)
  (let ((lo (sb-simd-avx::%make-u8.16 u01 u02 u03 u04 u05 u06 u07 u08 u09 u10 u11 u12 u13 u14 u15 u16))
        (hi (sb-simd-avx::%make-u8.16 u17 u18 u19 u20 u21 u22 u23 u24 u25 u26 u27 u28 u29 u30 u31 u32)))
    (%u8.32-insert128 (sb-simd-avx::%u8.32!-from-p128 lo) hi 1)))

(sb-simd::define-pseudo-vop u8.32-values (x)
  (multiple-value-call #'values
    (sb-simd-avx::%u8.16-values (sb-simd-avx::%u8.16!-from-p256 x))
    (sb-simd-avx::%u8.16-values (%u8.32-extract128 x 1))))

(sb-simd::define-pseudo-vop u8.32-not (a)
  (%u8.32-andnot
   a
   (%make-u8.32 +u8-true+ +u8-true+ +u8-true+ +u8-true+
                +u8-true+ +u8-true+ +u8-true+ +u8-true+
                +u8-true+ +u8-true+ +u8-true+ +u8-true+
                +u8-true+ +u8-true+ +u8-true+ +u8-true+
                +u8-true+ +u8-true+ +u8-true+ +u8-true+
                +u8-true+ +u8-true+ +u8-true+ +u8-true+
                +u8-true+ +u8-true+ +u8-true+ +u8-true+
                +u8-true+ +u8-true+ +u8-true+ +u8-true+)))

(sb-simd::define-pseudo-vop two-arg-u8.32/= (a b)
  (%u8.32-not
   (%two-arg-u8.32= a b)))

(sb-simd::define-pseudo-vop two-arg-u8.32< (a b)
  (%two-arg-u8.32> b a))

(sb-simd::define-pseudo-vop two-arg-u8.32>= (a b)
  (%u8.32-not
   (%two-arg-u8.32< a b)))

(sb-simd::define-pseudo-vop two-arg-u8.32<= (a b)
  (%u8.32-not
   (%two-arg-u8.32> a b)))

(sb-simd::define-pseudo-vop make-u16.16 (a b c d e f g h i j k l m n o p)
  (let ((lo (sb-simd-avx::%make-u16.8 a b c d e f g h))
        (hi (sb-simd-avx::%make-u16.8 i j k l m n o p)))
    (%u16.16-insert128 (sb-simd-avx::%u16.16!-from-p128 lo) hi 1)))

(sb-simd::define-pseudo-vop u16.16-values (x)
  (multiple-value-call #'values
    (sb-simd-avx::%u16.8-values (sb-simd-avx::%u16.8!-from-p256 x))
    (sb-simd-avx::%u16.8-values (%u16.16-extract128 x 1))))

(sb-simd::define-pseudo-vop u16.16-not (a)
  (%u16.16-andnot
   a
   (%make-u16.16 +u16-true+ +u16-true+ +u16-true+ +u16-true+
                 +u16-true+ +u16-true+ +u16-true+ +u16-true+
                 +u16-true+ +u16-true+ +u16-true+ +u16-true+
                 +u16-true+ +u16-true+ +u16-true+ +u16-true+)))

(sb-simd::define-pseudo-vop two-arg-u16.16/= (a b)
  (%u16.16-not
   (%two-arg-u16.16= a b)))

(sb-simd::define-pseudo-vop two-arg-u16.16< (a b)
  (%two-arg-u16.16> b a))

(sb-simd::define-pseudo-vop two-arg-u16.16>= (a b)
  (%u16.16-not
   (%two-arg-u16.16< a b)))

(sb-simd::define-pseudo-vop two-arg-u16.16<= (a b)
  (%u16.16-not
   (%two-arg-u16.16> a b)))

(sb-simd::define-pseudo-vop make-u32.8 (a b c d e f g h)
  (let ((lo (sb-simd-avx::%make-u32.4 a b c d))
        (hi (sb-simd-avx::%make-u32.4 e f g h)))
    (%u32.8-insert128 (sb-simd-avx::%u32.8!-from-p128 lo) hi 1)))

(sb-simd::define-pseudo-vop u32.8-values (x)
  (multiple-value-call #'values
    (sb-simd-avx::%u32.4-values (sb-simd-avx::%u32.4!-from-p256 x))
    (sb-simd-avx::%u32.4-values (%u32.8-extract128 x 1))))

(sb-simd::define-pseudo-vop u32.8-not (a)
  (%u32.8-andnot
   a
   (%make-u32.8 +u32-true+ +u32-true+ +u32-true+ +u32-true+
                +u32-true+ +u32-true+ +u32-true+ +u32-true+)))

(sb-simd::define-pseudo-vop two-arg-u32.8/= (a b)
  (%u32.8-not
   (%two-arg-u32.8= a b)))

(sb-simd::define-pseudo-vop two-arg-u32.8< (a b)
  (%two-arg-u32.8> b a))

(sb-simd::define-pseudo-vop two-arg-u32.8>= (a b)
  (%u32.8-not
   (%two-arg-u32.8< a b)))

(sb-simd::define-pseudo-vop two-arg-u32.8<= (a b)
  (%u32.8-not
   (%two-arg-u32.8> a b)))

(sb-simd::define-pseudo-vop make-u64.4 (a b c d)
  (let ((lo (sb-simd-avx::%make-u64.2 a b))
        (hi (sb-simd-avx::%make-u64.2 c d)))
    (%u64.4-insert128 (sb-simd-avx::%u64.4!-from-p128 lo) hi 1)))

(sb-simd::define-pseudo-vop u64.4-values (x)
  (multiple-value-call #'values
    (sb-simd-avx::%u64.2-values (sb-simd-avx::%u64.2!-from-p256 x))
    (sb-simd-avx::%u64.2-values (%u64.4-extract128 x 1))))

(sb-simd::define-pseudo-vop u64.4-not (a)
  (%u64.4-andnot
   a
   (%make-u64.4 +u64-true+ +u64-true+ +u64-true+ +u64-true+)))

(sb-simd::define-pseudo-vop two-arg-u64.4/= (a b)
  (%u64.4-not
   (%two-arg-u64.4= a b)))

(sb-simd::define-pseudo-vop two-arg-u64.4< (a b)
  (%two-arg-u64.4> b a))

(sb-simd::define-pseudo-vop two-arg-u64.4>= (a b)
  (%u64.4-not
   (%two-arg-u64.4< a b)))

(sb-simd::define-pseudo-vop two-arg-u64.4<= (a b)
  (%u64.4-not
   (%two-arg-u64.4> a b)))

(sb-simd::define-pseudo-vop make-s8.32
    (s01 s02 s03 s04 s05 s06 s07 s08 s09 s10 s11 s12 s13 s14 s15 s16 s17 s18 s19 s20 s21 s22 s23 s24 s25 s26 s27 s28 s29 s30 s31 s32)
  (let ((lo (sb-simd-avx::%make-s8.16 s01 s02 s03 s04 s05 s06 s07 s08 s09 s10 s11 s12 s13 s14 s15 s16))
        (hi (sb-simd-avx::%make-s8.16 s17 s18 s19 s20 s21 s22 s23 s24 s25 s26 s27 s28 s29 s30 s31 s32)))
    (%s8.32-insert128 (sb-simd-avx::%s8.32!-from-p128 lo) hi 1)))

(sb-simd::define-pseudo-vop s8.32-values (x)
  (multiple-value-call #'values
    (sb-simd-avx::%s8.16-values (sb-simd-avx::%s8.16!-from-p256 x))
    (sb-simd-avx::%s8.16-values (%s8.32-extract128 x 1))))

(sb-simd::define-pseudo-vop s8.32-not (a)
  (%s8.32-andnot
   a
   (%make-s8.32 +s8-true+ +s8-true+ +s8-true+ +s8-true+
                +s8-true+ +s8-true+ +s8-true+ +s8-true+
                +s8-true+ +s8-true+ +s8-true+ +s8-true+
                +s8-true+ +s8-true+ +s8-true+ +s8-true+
                +s8-true+ +s8-true+ +s8-true+ +s8-true+
                +s8-true+ +s8-true+ +s8-true+ +s8-true+
                +s8-true+ +s8-true+ +s8-true+ +s8-true+
                +s8-true+ +s8-true+ +s8-true+ +s8-true+)))

(sb-simd::define-pseudo-vop two-arg-s8.32/= (a b)
  (%s8.32-not
   (%two-arg-s8.32= a b)))

(sb-simd::define-pseudo-vop two-arg-s8.32< (a b)
  (%two-arg-s8.32> b a))

(sb-simd::define-pseudo-vop two-arg-s8.32>= (a b)
  (%s8.32-not
   (%two-arg-s8.32< a b)))

(sb-simd::define-pseudo-vop two-arg-s8.32<= (a b)
  (%s8.32-not
   (%two-arg-s8.32> a b)))

(sb-simd::define-pseudo-vop make-s16.16 (a b c d e f g h i j k l m n o p)
  (let ((lo (sb-simd-avx::%make-s16.8 a b c d e f g h))
        (hi (sb-simd-avx::%make-s16.8 i j k l m n o p)))
    (%s16.16-insert128 (sb-simd-avx::%s16.16!-from-p128 lo) hi 1)))

(sb-simd::define-pseudo-vop s16.16-values (x)
  (multiple-value-call #'values
    (sb-simd-avx::%s16.8-values (sb-simd-avx::%s16.8!-from-p256 x))
    (sb-simd-avx::%s16.8-values (sb-simd-avx::%s16.16-extract128 x 1))))

(sb-simd::define-pseudo-vop s16.16-not (a)
  (%s16.16-andnot
   a
   (%make-s16.16 +s16-true+ +s16-true+ +s16-true+ +s16-true+
                 +s16-true+ +s16-true+ +s16-true+ +s16-true+
                 +s16-true+ +s16-true+ +s16-true+ +s16-true+
                 +s16-true+ +s16-true+ +s16-true+ +s16-true+)))

(sb-simd::define-pseudo-vop two-arg-s16.16/= (a b)
  (%s16.16-not
   (%two-arg-s16.16= a b)))

(sb-simd::define-pseudo-vop two-arg-s16.16< (a b)
  (%two-arg-s16.16> b a))

(sb-simd::define-pseudo-vop two-arg-s16.16>= (a b)
  (%s16.16-not
   (%two-arg-s16.16< a b)))

(sb-simd::define-pseudo-vop two-arg-s16.16<= (a b)
  (%s16.16-not
   (%two-arg-s16.16> a b)))

(sb-simd::define-pseudo-vop make-s32.8 (a b c d e f g h)
  (let ((lo (sb-simd-avx::%make-s32.4 a b c d))
        (hi (sb-simd-avx::%make-s32.4 e f g h)))
    (%s32.8-insert128 (sb-simd-avx::%s32.8!-from-p128 lo) hi 1)))

(sb-simd::define-pseudo-vop s32.8-values (x)
  (multiple-value-call #'values
    (sb-simd-avx::%s32.4-values (sb-simd-avx::%s32.4!-from-p256 x))
    (sb-simd-avx::%s32.4-values (sb-simd-avx::%s32.8-extract128 x 1))))

(sb-simd::define-pseudo-vop s32.8-not (a)
  (%s32.8-andnot
   a
   (%make-s32.8 +s32-true+ +s32-true+ +s32-true+ +s32-true+
                +s32-true+ +s32-true+ +s32-true+ +s32-true+)))

(sb-simd::define-pseudo-vop two-arg-s32.8/= (a b)
  (%s32.8-not
   (%two-arg-s32.8= a b)))

(sb-simd::define-pseudo-vop two-arg-s32.8< (a b)
  (%two-arg-s32.8> b a))

(sb-simd::define-pseudo-vop two-arg-s32.8>= (a b)
  (%s32.8-not
   (%two-arg-s32.8< a b)))

(sb-simd::define-pseudo-vop two-arg-s32.8<= (a b)
  (%s32.8-not
   (%two-arg-s32.8> a b)))

(sb-simd::define-pseudo-vop make-s64.4 (a b c d)
  (let ((lo (sb-simd-avx::%make-s64.2 a b))
        (hi (sb-simd-avx::%make-s64.2 c d)))
    (%s64.4-insert128 (sb-simd-avx::%s64.4!-from-p128 lo) hi 1)))

(sb-simd::define-pseudo-vop s64.4-values (x)
  (multiple-value-call #'values
    (sb-simd-avx::%s64.2-values (sb-simd-avx::%s64.2!-from-p256 x))
    (sb-simd-avx::%s64.2-values (%s64.4-extract128 x 1))))

(sb-simd::define-pseudo-vop s64.4-not (a)
  (%s64.4-andnot
   a
   (%make-s64.4 +s64-true+ +s64-true+ +s64-true+ +s64-true+)))

(sb-simd::define-pseudo-vop two-arg-s64.4/= (a b)
  (%s64.4-not
   (%two-arg-s64.4= a b)))

(sb-simd::define-pseudo-vop two-arg-s64.4< (a b)
  (%two-arg-s64.4> b a))

(sb-simd::define-pseudo-vop two-arg-s64.4>= (a b)
  (%s64.4-not
   (%two-arg-s64.4< a b)))

(sb-simd::define-pseudo-vop two-arg-s64.4<= (a b)
  (%s64.4-not
   (%two-arg-s64.4> a b)))

(sb-simd::define-pseudo-vop f64.4-reverse (a)
  (%f64.4-permute4x64 a #b00011011))
