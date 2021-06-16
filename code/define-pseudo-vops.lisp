(in-package #:sb-simd)

;;; Primitives with a :NONE encoding do not define a VOP.  Instead, we
;;; define a pseudo VOP - a regular function that has the name of the VOP
;;; we didn't emit.  Pseudo VOPs are useful for defining instructions that
;;; we would like to have in the instruction set, and that can be expressed
;;; easily in terms of the other VOPs.

(defmacro define-pseudo-vop (name lambda-list &body body)
  (with-accessors ((vop primitive-record-vop)
                   (result-records primitive-record-result-records)
                   (argument-records primitive-record-argument-records))
      (find-instruction-record name)
    (assert (= (length lambda-list)
               (length argument-records)))
    (destructuring-bind (result-record) result-records
      `(define-inline ,vop ,lambda-list
         (declare
          ,@(loop for argument-record in argument-records
                  for argument in lambda-list
                  collect `(type ,(value-record-name argument-record) ,argument)))
         (the ,(value-record-name result-record)
              (progn ,@body))))))

(in-package #:sb-simd-sse)

(sb-simd::define-pseudo-vop f32.4-not (a)
  (%f32.4-andnot
   a
   (f32.4 +f32-true+)))

(in-package #:sb-simd-sse2)

(sb-simd::define-pseudo-vop f64.2-not (a)
  (%f64.2-andnot
   a
   (f64.2 +f64-true+)))

(sb-simd::define-pseudo-vop u32.4-not (a)
  (%u32.4-andnot
   a
   (u32.4 +u32-true+)))

(sb-simd::define-pseudo-vop u64.2-not (a)
  (%u64.2-andnot
   a
   (u64.2 +u64-true+)))

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

(sb-simd::define-pseudo-vop f64.2-not (a)
  (%f64.2-andnot
   a
   (f64.2 +f64-true+)))

(sb-simd::define-pseudo-vop f32.8-not (a)
  (%f32.8-andnot
   a
   (f32.8 +f32-true+)))

(sb-simd::define-pseudo-vop f64.4-not (a)
  (%f64.4-andnot
   a
   (f64.4 +f64-true+)))

(sb-simd::define-pseudo-vop u32.4-not (a)
  (%u32.4-andnot
   a
   (u32.4 +u32-true+)))

(sb-simd::define-pseudo-vop u64.2-not (a)
  (%u64.2-andnot
   a
   (u64.2 +u64-true+)))

(sb-simd::define-pseudo-vop u32.8-not (a)
  (%u32.8-andnot
   a
   (u32.8 +u32-true+)))

(sb-simd::define-pseudo-vop u64.4-not (a)
  (%u64.4-andnot
   a
   (u64.4 +u64-true+)))

;; (sb-simd::define-pseudo-vop f64.4-reverse (a)
;;   (f64.4-permute (f64.4-permute2f128 a a 1) 5))

;; (sb-simd::define-pseudo-vop f64.4-hsum (a)
;;   (multiple-value-bind (r0 r1)
;;       (f64.2-values (f64.2+ (f64.4-extractf128 a 0)
;;                             (f64.4-extractf128 a 1)))
;;     (+ r0 r1)))

;; (sb-simd::define-pseudo-vop f64.4-hsum (a)
;;   (let* ((x0 (f64.4-extractf128 a 0))
;;          (x1 (f64.4-extractf128 a 1))
;;          (x0 (f64.2+ x0 x1))
;;          (x1 (f64.2-unpackhi x0 x0)))
;;     (sb-ext::%simd-pack-low x1)))

;; (declaim (ftype (function (f64.4) f64.4))
;;          (inline f64.4-rec-9))
;; (defun f64.4-rec-9 (%x)
;;   (declare (optimize speed (safety 0) (debug 0))
;;            (type f64.4 %x))
;;   (let* ((x (f64.4-from-f32.4 (f32.4-reciprocal (f32.4-from-f64.4 %x))))
;;          (w (f64.4* x %x))
;;          (three (make-f64.4 3 3 3 3))
;;          (z (f64.4* w x))
;;          (w (f64.4- w three))
;;          (x (f64.4* x three))
;;          (z (f64.4* z w)))
;;     (f64.4+ z x)))

(in-package #:sb-simd-avx2)

(sb-simd::define-pseudo-vop two-arg-u32.4/= (a b)
  (sb-simd-avx::%u32.4-not
   (%two-arg-u32.4= a b)))

(sb-simd::define-pseudo-vop two-arg-u32.4< (a b)
  (%two-arg-u32.4> b a))

(sb-simd::define-pseudo-vop two-arg-u32.4>= (a b)
  (sb-simd-avx::%u32.4-not
   (%two-arg-u32.4< a b)))

(sb-simd::define-pseudo-vop two-arg-u32.4<= (a b)
  (sb-simd-avx::%u32.4-not
   (%two-arg-u32.4> a b)))

(sb-simd::define-pseudo-vop two-arg-u32.8/= (a b)
  (sb-simd-avx::%u32.8-not
   (%two-arg-u32.8= a b)))

(sb-simd::define-pseudo-vop two-arg-u32.8< (a b)
  (%two-arg-u32.8> b a))

(sb-simd::define-pseudo-vop two-arg-u32.8>= (a b)
  (sb-simd-avx::%u32.8-not
   (%two-arg-u32.8< a b)))

(sb-simd::define-pseudo-vop two-arg-u32.8<= (a b)
  (sb-simd-avx::%u32.8-not
   (%two-arg-u32.8> a b)))

(sb-simd::define-pseudo-vop two-arg-u64.2/= (a b)
  (sb-simd-avx::%u64.2-not
   (%two-arg-u64.2= a b)))

(sb-simd::define-pseudo-vop two-arg-u64.2< (a b)
  (%two-arg-u64.2> b a))

(sb-simd::define-pseudo-vop two-arg-u64.2>= (a b)
  (sb-simd-avx::%u64.2-not
   (%two-arg-u64.2< a b)))

(sb-simd::define-pseudo-vop two-arg-u64.2<= (a b)
  (sb-simd-avx::%u64.2-not
   (%two-arg-u64.2> a b)))

(sb-simd::define-pseudo-vop two-arg-u64.4/= (a b)
  (sb-simd-avx::%u64.4-not
   (%two-arg-u64.4= a b)))

(sb-simd::define-pseudo-vop two-arg-u64.4< (a b)
  (%two-arg-u64.4> b a))

(sb-simd::define-pseudo-vop two-arg-u64.4>= (a b)
  (sb-simd-avx::%u64.4-not
   (%two-arg-u64.4< a b)))

(sb-simd::define-pseudo-vop two-arg-u64.4<= (a b)
  (sb-simd-avx::%u64.4-not
   (%two-arg-u64.4> a b)))

;; (sb-simd::define-pseudo-vop f64.4-reverse (a)
;;   (f64.4-permute4x64 a #b00011011))

