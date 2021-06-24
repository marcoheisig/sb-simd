(in-package #:sb-simd)

;;; Define VOPs for all the instructions, loads, and stores of each
;;; instruction set.

(defmacro define-primitive-vop (primitive-record-name)
  (with-accessors ((name primitive-record-name)
                   (vop primitive-record-vop)
                   (mnemonic primitive-record-mnemonic)
                   (argument-records primitive-record-argument-records)
                   (result-records primitive-record-result-records)
                   (cost primitive-record-cost)
                   (pure primitive-record-pure)
                   (commutative primitive-record-commutative)
                   (prefix primitive-record-prefix)
                   (encoding primitive-record-encoding))
      (find-instruction-record primitive-record-name)
    (let* ((asyms (argument-symbols (length argument-records)))
           (rsyms (result-symbols (length result-records)))
           (defknown
               `(sb-c:defknown ,vop
                    (,@(mapcar #'value-record-name argument-records))
                    (values ,@(mapcar #'value-record-name result-records) &optional)
                    (,@(unless (eq encoding :none) '(sb-c:always-translatable))
                     ,@(when pure '(sb-c:foldable sb-c:flushable sb-c:movable)))
                  :overwrite-fndb-silently t))
           (args
             (loop for asym in asyms
                   for argument-record in argument-records
                   when (symbolp (value-record-primitive-type argument-record))
                     collect `(,asym :scs (,(value-record-register argument-record)))))
           (info
             (loop for asym in asyms
                   for argument-record in argument-records
                   unless (symbolp (value-record-primitive-type argument-record))
                     collect asym))
           (results
             (loop for rsym in rsyms
                   for result-record in result-records
                   collect `(,rsym :scs (,(value-record-register result-record))))))
      (ecase encoding
        (:none
         `(progn ,defknown))
        (:standard
         `(progn
            ,defknown
            (sb-c:define-vop (,vop)
              (:translate ,vop)
              (:policy :fast-safe)
              (:args ,@args)
              (:info ,@info)
              (:results ,@results)
              (:arg-types ,@(mapcar #'value-record-primitive-type argument-records))
              (:result-types ,@(mapcar #'value-record-primitive-type result-records))
              (:generator
               ,cost
               (sb-assem:inst ,mnemonic ,@(when prefix `(,prefix)) ,@rsyms ,@asyms)))))
        (:sse
         `(progn
            ,defknown
            (sb-c:define-vop (,vop)
              (:translate ,vop)
              (:policy :fast-safe)
              (:args (,@(first args) :target ,(first rsyms)) ,@(rest args))
              (:info ,@info)
              (:results ,@results)
              (:arg-types ,@(mapcar #'value-record-primitive-type argument-records))
              (:result-types ,@(mapcar #'value-record-primitive-type result-records))
              (:generator
               ,cost
               (sb-c:move ,(first rsyms) ,(first asyms))
               (sb-assem:inst ,mnemonic ,@(when prefix `(,prefix)) ,@rsyms ,@(rest asyms))))))))))

;;; Load- and store VOPs are augmented with an auxiliary last argument that
;;; is a constant addend for the address calculation.  This addend is zero
;;; by default, but we can sometimes transform the code for the index
;;; calculation such that we have a nonzero addend.  We also generate two
;;; variants of the VOP - one for the general case, and one for the case
;;; where the index is a compile-time constant.

(defmacro define-load-vop (load-record-name)
  (with-accessors ((name load-record-name)
                   (vop load-record-vop)
                   (mnemonic load-record-mnemonic)
                   (value-record load-record-value-record)
                   (vector-record load-record-vector-record))
      (find-instruction-record load-record-name)
    (let* ((scalar-record
             (etypecase value-record
               (scalar-record value-record)
               (simd-record (simd-record-scalar-record value-record))))
           (element-size (ceiling (scalar-record-bits scalar-record) 8))
           (scale (ash element-size (- sb-vm:n-fixnum-tag-bits)))
           (displacement
             (multiple-value-bind (lo hi)
                 (sb-vm::displacement-bounds sb-vm:other-pointer-lowtag element-size sb-vm:vector-data-offset)
               `(integer ,lo ,hi))))
      `(progn
         (sb-c:defknown ,vop
             (,(value-record-name vector-record) index ,displacement)
             (values ,(value-record-name value-record) &optional)
             (sb-c:always-translatable)
           :overwrite-fndb-silently t)
         (sb-vm::define-vop (,vop)
           (:translate ,vop)
           (:policy :fast-safe)
           (:args (vector :scs (sb-vm::descriptor-reg))
                  (index :scs (sb-vm::any-reg)))
           (:info addend)
           (:arg-types ,(scalar-record-primitive-type vector-record)
                       sb-vm::positive-fixnum
                       (:constant ,displacement))
           (:results (result :scs (,(value-record-register value-record))))
           (:result-types ,(value-record-primitive-type value-record))
           (:generator 2
                       (sb-assem:inst
                        ,mnemonic
                        result
                        (sb-vm::float-ref-ea vector index addend ,element-size :scale ,scale))))
         (sb-vm::define-vop (,(mksym (symbol-package vop) vop "-C"))
           (:translate ,vop)
           (:policy :fast-safe)
           (:args (vector :scs (sb-vm::descriptor-reg)))
           (:info index addend)
           (:arg-types ,(scalar-record-primitive-type vector-record)
                       (:constant sb-vm::low-index)
                       (:constant ,displacement))
           (:results (result :scs (,(value-record-register value-record))))
           (:result-types ,(value-record-primitive-type value-record))
           (:generator 1
                       (sb-assem:inst
                        ,mnemonic
                        result
                        (sb-vm::float-ref-ea vector index addend ,element-size))))))))

(defmacro define-store-vop (store-record-name)
  (with-accessors ((name store-record-name)
                   (vop store-record-vop)
                   (mnemonic store-record-mnemonic)
                   (value-record store-record-value-record)
                   (vector-record store-record-vector-record))
      (find-instruction-record store-record-name)
    (let* ((scalar-record
             (etypecase value-record
               (scalar-record value-record)
               (simd-record (simd-record-scalar-record value-record))))
           (element-size (ceiling (scalar-record-bits scalar-record) 8))
           (scale (ash element-size (- sb-vm:n-fixnum-tag-bits)))
           (displacement
             (multiple-value-bind (lo hi)
                 (sb-vm::displacement-bounds sb-vm:other-pointer-lowtag element-size sb-vm:vector-data-offset)
               `(integer ,lo ,hi))))
      `(progn
         (sb-c:defknown ,vop
             (,(value-record-name value-record)
              ,(value-record-name vector-record)
              index ,displacement)
             (values ,(value-record-name value-record) &optional)
             (sb-c:always-translatable)
           :overwrite-fndb-silently t)
         (sb-vm::define-vop (,vop)
           (:translate ,vop)
           (:policy :fast-safe)
           (:args (value :scs (,(value-record-register value-record)))
                  (vector :scs (sb-vm::descriptor-reg))
                  (index :scs (sb-vm::any-reg)))
           (:info addend)
           (:arg-types ,(value-record-primitive-type value-record)
                       ,(scalar-record-primitive-type vector-record)
                       sb-vm::positive-fixnum
                       (:constant ,displacement))
           (:results (result :scs (,(value-record-register value-record))))
           (:result-types ,(value-record-primitive-type value-record))
           (:generator 2
             (sb-assem:inst
              ,mnemonic
              (sb-vm::float-ref-ea vector index addend ,element-size :scale ,scale)
              value)
             (sb-c:move result value)))
         (sb-vm::define-vop (,(mksym (symbol-package vop) vop "-C"))
           (:translate ,vop)
           (:policy :fast-safe)
           (:args (value :scs (,(value-record-register value-record)))
                  (vector :scs (sb-vm::descriptor-reg)))
           (:info index addend)
           (:arg-types ,(value-record-primitive-type value-record)
                       ,(scalar-record-primitive-type vector-record)
                       (:constant sb-vm::low-index)
                       (:constant ,displacement))
           (:results (result :scs (,(value-record-register value-record))))
           (:result-types ,(value-record-primitive-type value-record))
           (:generator 1
             (sb-assem:inst
              ,mnemonic
              (sb-vm::float-ref-ea vector index addend ,element-size)
              value)
             (sb-c:move result value)))))))

(defmacro define-vops ()
  `(progn
     ,@(loop for primitive-record in (filter-available-instruction-records #'primitive-record-p)
             collect `(define-primitive-vop ,(primitive-record-name primitive-record)))
     ,@(loop for load-record in (filter-available-instruction-records #'load-record-p)
             collect `(define-load-vop ,(load-record-name load-record)))
     ,@(loop for store-record in (filter-available-instruction-records #'store-record-p)
             collect `(define-store-vop ,(store-record-name store-record)))))

(define-vops)
