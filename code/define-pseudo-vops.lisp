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
    (destructuring-bind (result-record) result-records
      (when (instruction-set-available-p instruction-set)
        `(define-inline ,vop ,lambda-list
           (declare
            ,@(loop for argument-record in argument-records
                    for argument in lambda-list
                    collect `(type ,(value-record-name argument-record) ,argument)))
           (the ,(value-record-name result-record)
                (progn ,@body)))))))

(in-package #:sb-simd-sse)

(sb-simd::define-pseudo-vop f32.4-not (a)
  (%f32.4-andnot
   a
   (f32.4 +f32-true+)))

(sb-simd::define-pseudo-vop f32.4-zeros ()
  (f32.4-or))

(in-package #:sb-simd-sse2)

(sb-simd::define-pseudo-vop f64.2-not (a)
  (%f64.2-andnot
   a
   (f64.2 +f64-true+)))

(sb-simd::define-pseudo-vop f64.2-zeros ()
  (f64.2-or))

(sb-simd::define-pseudo-vop u8.16-not (a)
  (%u8.16-andnot
   a
   (u8.16 +u8-true+)))

(sb-simd::define-pseudo-vop u8.16-zeros ()
  (u8.16-or))

(sb-simd::define-pseudo-vop u16.8-not (a)
  (%u16.8-andnot
   a
   (u16.8 +u16-true+)))

(sb-simd::define-pseudo-vop u16.8-zeros ()
  (u16.8-or))

(sb-simd::define-pseudo-vop u32.4-not (a)
  (%u32.4-andnot
   a
   (u32.4 +u32-true+)))

(sb-simd::define-pseudo-vop u32.4-zeros ()
  (u32.4-or))

(sb-simd::define-pseudo-vop u64.2-not (a)
  (%u64.2-andnot
   a
   (u64.2 +u64-true+)))

(sb-simd::define-pseudo-vop u64.2-zeros ()
  (u64.2-or))

(sb-simd::define-pseudo-vop s8.16-not (a)
  (%s8.16-andnot
   a
   (s8.16 +s8-true+)))

(sb-simd::define-pseudo-vop s8.16-zeros ()
  (s8.16-or))

(sb-simd::define-pseudo-vop s16.8-not (a)
  (%s16.8-andnot
   a
   (s16.8 +s16-true+)))

(sb-simd::define-pseudo-vop s16.8-zeros ()
  (s16.8-or))

(sb-simd::define-pseudo-vop s32.4-not (a)
  (%s32.4-andnot
   a
   (s32.4 +s32-true+)))

(sb-simd::define-pseudo-vop s32.4-zeros ()
  (s32.4-or))

(sb-simd::define-pseudo-vop s64.2-not (a)
  (%s64.2-andnot
   a
   (s64.2 +s64-true+)))

(sb-simd::define-pseudo-vop s64.2-zeros ()
  (s64.2-or))

(in-package #:sb-simd-sse4.1)

(sb-simd::define-pseudo-vop two-arg-u64.2/= (a b)
  (sb-simd-sse2::%u64.2-not
   (%two-arg-u64.2= a b)))

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

(sb-simd::define-pseudo-vop f32.4-not (a)
  (%f32.4-andnot
   a
   (f32.4 +f32-true+)))

(sb-simd::define-pseudo-vop f32.4-zeros ()
  (f32.4-or))

(sb-simd::define-pseudo-vop f64.2-not (a)
  (%f64.2-andnot
   a
   (f64.2 +f64-true+)))

(sb-simd::define-pseudo-vop f64.2-zeros ()
  (f64.2-or))

(sb-simd::define-pseudo-vop f32.8-not (a)
  (%f32.8-andnot
   a
   (f32.8 +f32-true+)))

(sb-simd::define-pseudo-vop f32.8-zeros ()
  (f32.8-or))

(sb-simd::define-pseudo-vop f64.4-not (a)
  (%f64.4-andnot
   a
   (f64.4 +f64-true+)))

(sb-simd::define-pseudo-vop f64.4-zeros ()
  (f64.4-or))

(sb-simd::define-pseudo-vop u8.16-not (a)
  (%u8.16-andnot
   a
   (u8.16 +u8-true+)))

(sb-simd::define-pseudo-vop u8.16-zeros ()
  (u8.16-or))

(sb-simd::define-pseudo-vop u16.8-not (a)
  (%u16.8-andnot
   a
   (u16.8 +u16-true+)))

(sb-simd::define-pseudo-vop u16.8-zeros ()
  (u16.8-or))

(sb-simd::define-pseudo-vop u32.4-not (a)
  (%u32.4-andnot
   a
   (u32.4 +u32-true+)))

(sb-simd::define-pseudo-vop u32.4-zeros ()
  (u32.4-or))

(sb-simd::define-pseudo-vop u64.2-not (a)
  (%u64.2-andnot
   a
   (u64.2 +u64-true+)))

(sb-simd::define-pseudo-vop u64.2-zeros ()
  (u64.2-or))

(sb-simd::define-pseudo-vop s8.16-not (a)
  (%s8.16-andnot
   a
   (s8.16 +s8-true+)))

(sb-simd::define-pseudo-vop u8.16-zeros ()
  (u8.16-or))

(sb-simd::define-pseudo-vop s16.8-not (a)
  (%s16.8-andnot
   a
   (s16.8 +s16-true+)))

(sb-simd::define-pseudo-vop s16.8-zeros ()
  (s16.8-or))

(sb-simd::define-pseudo-vop s32.4-not (a)
  (%s32.4-andnot
   a
   (s32.4 +s32-true+)))

(sb-simd::define-pseudo-vop s32.4-zeros ()
  (s32.4-or))

(sb-simd::define-pseudo-vop s64.2-not (a)
  (%s64.2-andnot
   a
   (s64.2 +s64-true+)))

(sb-simd::define-pseudo-vop s64.2-zeros ()
  (s64.2-or))

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

(sb-simd::define-pseudo-vop f64.4-reverse (a)
  (f64.4-permute (f64.4-permute2f128 a a 1) 5))

(sb-simd::define-pseudo-vop f64.4-hsum (a)
  (multiple-value-bind (r0 r1)
      (f64.2-values (f64.2+ (f64.4-extractf128 a 0)
                            (f64.4-extractf128 a 1)))
    (+ r0 r1)))

(in-package #:sb-simd-avx2)

(sb-simd::define-pseudo-vop u8.32-not (a)
  (%u8.32-andnot
   a
   (u8.32 +u8-true+)))

(sb-simd::define-pseudo-vop u8.32-zeros ()
  (u8.32-or))

(sb-simd::define-pseudo-vop u16.16-not (a)
  (%u16.16-andnot
   a
   (u16.16 +u16-true+)))

(sb-simd::define-pseudo-vop u16.16-zeros ()
  (u16.16-or))

(sb-simd::define-pseudo-vop u32.8-not (a)
  (%u32.8-andnot
   a
   (u32.8 +u32-true+)))

(sb-simd::define-pseudo-vop u32.8-zeros ()
  (u32.8-or))

(sb-simd::define-pseudo-vop u64.4-not (a)
  (%u64.4-andnot
   a
   (u64.4 +u64-true+)))

(sb-simd::define-pseudo-vop u64.4-zeros ()
  (u64.4-or))

(sb-simd::define-pseudo-vop s8.32-not (a)
  (%s8.32-andnot
   a
   (s8.32 +s8-true+)))

(sb-simd::define-pseudo-vop s8.32-zeros ()
  (s8.32-or))

(sb-simd::define-pseudo-vop s16.16-not (a)
  (%s16.16-andnot
   a
   (s16.16 +s16-true+)))

(sb-simd::define-pseudo-vop s16.16-zeros ()
  (s16.16-or))

(sb-simd::define-pseudo-vop s32.8-not (a)
  (%s32.8-andnot
   a
   (s32.8 +s32-true+)))

(sb-simd::define-pseudo-vop s32.8-zeros ()
  (s32.8-or))

(sb-simd::define-pseudo-vop s64.4-not (a)
  (%s64.4-andnot
   a
   (s64.4 +s64-true+)))

(sb-simd::define-pseudo-vop s64.4-zeros ()
  (s64.4-or))

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

;; (sb-simd::define-pseudo-vop f64.4-reverse (a)
;;   (f64.4-permute4x64 a #b00011011))

