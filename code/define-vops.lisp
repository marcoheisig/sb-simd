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
    (let* ((arguments (argument-symbols (length argument-records)))
           (results (result-symbols (length result-records)))
           (defknown
               `(sb-c:defknown ,vop
                    (,@(mapcar #'value-record-name argument-records))
                    (values ,@(mapcar #'value-record-name result-records) &optional)
                    (,@(unless (eq encoding :none) '(sb-c:always-translatable))
                     ,@(when pure '(sb-c:foldable sb-c:flushable sb-c:movable)))
                  :overwrite-fndb-silently t)))
      (ecase encoding
        (:none
         `(progn ,defknown))
        (:standard
         `(progn
            ,defknown
            (sb-c:define-vop (,vop)
              (:translate ,vop)
              (:policy :fast-safe)
              (:args
               ,@(loop for argument in arguments
                       for argument-record in argument-records
                       collect
                       `(,argument :scs (,(value-record-register argument-record)))))
              (:results
               ,@(loop for result in results
                       for result-record in result-records
                       collect
                       `(,result :scs (,(value-record-register result-record)))))
              (:arg-types ,@(mapcar #'value-record-primitive-type argument-records))
              (:result-types ,@(mapcar #'value-record-primitive-type result-records))
              (:generator
               ,cost
               (sb-assem:inst ,mnemonic ,@(when prefix `(,prefix)) ,@results ,@arguments)))))
        (:sse
         `(progn
            ,defknown
            (sb-c:define-vop (,vop)
              (:translate ,vop)
              (:policy :fast-safe)
              (:args
               (,(first arguments)
                :scs (,(value-record-register (first argument-records)))
                :target ,(first results))
               ,@(loop for argument in (rest arguments)
                       for argument-record in (rest argument-records)
                       collect
                       `(,argument :scs (,(value-record-register argument-record)))))
              (:results
               ,@(loop for result in results
                       for result-record in result-records
                       collect
                       `(,result :scs (,(value-record-register result-record)))))
              (:arg-types ,@(mapcar #'value-record-primitive-type argument-records))
              (:result-types ,@(mapcar #'value-record-primitive-type result-records))
              (:generator
               ,cost
               (sb-c:move ,(first results) ,(first arguments))
               (sb-assem:inst ,mnemonic ,@(when prefix `(,prefix)) ,@results ,@(rest arguments))))))))))

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
