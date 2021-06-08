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

(in-package #:sb-simd-sse2)

(sb-simd::define-pseudo-vop u32.4-not (a)
  (%u32.4-andnot
   a
   (u32.4 (1- (expt 2 32)))))

(sb-simd::define-pseudo-vop u64.2-not (a)
  (%u64.2-andnot
   a
   (u64.2 (1- (expt 2 64)))))

(in-package #:sb-simd-avx)

(sb-simd::define-pseudo-vop u32.4-not (a)
  (%u32.4-andnot
   a
   (u32.4 (1- (expt 2 32)))))

(sb-simd::define-pseudo-vop u64.2-not (a)
  (%u64.2-andnot
   a
   (u64.2 (1- (expt 2 64)))))

(sb-simd::define-pseudo-vop u32.8-not (a)
  (%u32.8-andnot
   a
   (u32.8 (1- (expt 2 32)))))

(sb-simd::define-pseudo-vop u64.4-not (a)
  (%u64.4-andnot
   a
   (u64.4 (1- (expt 2 64)))))
