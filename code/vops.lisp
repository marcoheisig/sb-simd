(in-package #:sb-simd)

(defmacro define-instruction-vops ()
  `(progn
     ,@(loop for instruction-record being the hash-values of *instruction-records*
             for name = (instruction-record-name instruction-record)
             when (instruction-record-supported-p instruction-record)
               collect
               (ecase (instruction-record-encoding instruction-record)
                 (:standard `(define-standard-vop ,name))
                 (:sse `(define-sse-vop ,name))
                 (:store `(define-store-vop ,name))
                 (:load `(define-load-vop ,name))))))

(defmacro define-standard-vop (name)
  (with-accessors ((name instruction-record-name)
                   (mnemonic instruction-record-mnemonic)
                   (argument-records instruction-record-argument-records)
                   (result-records instruction-record-result-records)
                   (cost instruction-record-cost)
                   (pure instruction-record-pure)
                   (commutative instruction-record-commutative)
                   (prefix instruction-record-prefix))
      (find-instruction-record name)
    (let ((arguments (subseq *arguments* 0 (length argument-records)))
          (results (subseq *results* 0 (length result-records)))
          (vop-name (vop-name name)))
      `(progn
         (sb-c:defknown ,vop-name ,(mapcar #'value-record-type argument-records)
             (values ,@(mapcar #'value-record-type result-records) &optional)
             (sb-c:always-translatable ,@(when pure '(sb-c:movable sb-c:flushable sb-c:foldable)))
           :overwrite-fndb-silently t)
         (sb-c:define-vop (,vop-name)
           (:translate ,vop-name)
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
            (sb-assem:inst ,mnemonic ,@(when prefix `(,prefix)) ,@results ,@arguments)))))))

;;; SSE VOPs are very similar to standard VOPs, except that the first
;;; operand of the mnemonic is used both to receive the first argument and
;;; to store the result.
(defmacro define-sse-vop (name)
  (with-accessors ((name instruction-record-name)
                   (mnemonic instruction-record-mnemonic)
                   (argument-records instruction-record-argument-records)
                   (result-records instruction-record-result-records)
                   (cost instruction-record-cost)
                   (pure instruction-record-pure)
                   (commutative instruction-record-commutative)
                   (prefix instruction-record-prefix))
      (find-instruction-record name)
    (assert (= 1 (length result-records)))
    (let ((arguments (subseq *arguments* 0 (length argument-records)))
          (results (subseq *results* 0 (length result-records)))
          (vop-name (vop-name name)))
      `(progn
         (sb-c:defknown ,vop-name ,(mapcar #'value-record-type argument-records)
             (values ,@(mapcar #'value-record-type result-records) &optional)
             (sb-c:always-translatable ,@(when pure '(sb-c:movable sb-c:flushable sb-c:foldable)))
           :overwrite-fndb-silently t)
         (sb-c:define-vop (,vop-name)
           (:translate ,vop-name)
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
            (sb-assem:inst ,mnemonic ,@(when prefix `(,prefix)) ,@results ,@(rest arguments))))))))

;;; Load VOPs are augmented with an auxiliary third argument that is a
;;; constant addend for the address calculation.  This addend is zero by
;;; default, but we can sometimes transform the code for the index
;;; calculation such that we have a nonzero addend.  We also generate two
;;; variants of the VOP - one for the general case, and one for the case
;;; where the index is a compile-time constant.
(defmacro define-load-vop (name)
  (with-accessors ((name instruction-record-name)
                   (mnemonic instruction-record-mnemonic)
                   (argument-records instruction-record-argument-records)
                   (result-records instruction-record-result-records)
                   (cost instruction-record-cost)
                   (pure instruction-record-pure)
                   (commutative instruction-record-commutative)
                   (prefix instruction-record-prefix))
      (find-instruction-record name)
    (let* ((vop-name (vop-name name))
           (vop-name-c (vop-name name "-C"))
           (value-record (first result-records))
           (scalar-record
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
         (sb-c:defknown ,vop-name (,@(mapcar #'value-record-type argument-records) ,displacement)
             (values ,@(mapcar #'value-record-type result-records) &optional)
             (sb-c:always-translatable)
           :overwrite-fndb-silently t)
         (sb-vm::define-vop (,vop-name)
           (:translate ,vop-name)
           (:policy :fast-safe)
           (:args (vector :scs (sb-vm::descriptor-reg))
                  (index :scs (sb-vm::any-reg)))
           (:info addend)
           (:arg-types ,(scalar-record-primitive-type (first argument-records))
                       ,(scalar-record-primitive-type (second argument-records))
                       (:constant ,displacement))
           (:results (result :scs (,(value-record-register (first result-records)))))
           (:result-types ,(value-record-primitive-type value-record))
           (:generator
            ,cost
            (sb-assem:inst
             ,mnemonic
             result
             (sb-vm::float-ref-ea vector index addend ,element-size :scale ,scale))))
         (sb-vm::define-vop (,vop-name-c)
           (:translate ,vop-name)
           (:policy :fast-safe)
           (:args (vector :scs (sb-vm::descriptor-reg)))
           (:info index addend)
           (:arg-types ,(scalar-record-primitive-type (first argument-records))
                       (:constant sb-vm::low-index)
                       (:constant ,displacement))
           (:results (result :scs (,(value-record-register value-record))))
           (:result-types ,(value-record-primitive-type value-record))
           (:generator
            ,(1- cost)
            (sb-assem:inst
             ,mnemonic
             result
             (sb-vm::float-ref-ea vector index addend ,element-size))))))))

(defmacro define-store-vop (name)
  (with-accessors ((name instruction-record-name)
                   (mnemonic instruction-record-mnemonic)
                   (argument-records instruction-record-argument-records)
                   (result-records instruction-record-result-records)
                   (cost instruction-record-cost)
                   (commutative instruction-record-commutative)
                   (prefix instruction-record-prefix))
      (find-instruction-record name)
    (let* ((vop-name (vop-name name))
           (vop-name-c (vop-name name "-C"))
           (value-record (first result-records))
           (scalar-record
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
         (sb-c:defknown ,vop-name (,@(mapcar #'value-record-type argument-records) ,displacement)
             (values ,@(mapcar #'value-record-type result-records) &optional)
             (sb-c:always-translatable)
           :overwrite-fndb-silently t)
         (sb-vm::define-vop (,vop-name)
           (:translate ,vop-name)
           (:policy :fast-safe)
           (:args (value :scs (,(value-record-register value-record)))
                  (vector :scs (sb-vm::descriptor-reg))
                  (index :scs (sb-vm::any-reg)))
           (:info addend)
           (:arg-types ,(value-record-primitive-type (first argument-records))
                       ,(scalar-record-primitive-type (second argument-records))
                       ,(scalar-record-primitive-type (third argument-records))
                       (:constant ,displacement))
           (:results (result :scs (,(value-record-register value-record))))
           (:result-types ,(value-record-primitive-type value-record))
           (:generator
            7
            (sb-assem:inst
             ,mnemonic
             (sb-vm::float-ref-ea vector index addend ,element-size :scale ,scale)
             value)
            (sb-c:move result value)))
         (sb-vm::define-vop (,vop-name-c)
           (:translate ,vop-name)
           (:policy :fast-safe)
           (:args (value :scs (,(value-record-register value-record)))
                  (vector :scs (sb-vm::descriptor-reg)))
           (:info index addend)
           (:arg-types ,(value-record-primitive-type (first argument-records))
                       ,(scalar-record-primitive-type (second argument-records))
                       (:constant sb-vm::low-index)
                       (:constant ,displacement))
           (:results (result :scs (,(value-record-register value-record))))
           (:result-types ,(value-record-primitive-type value-record))
           (:generator
            6
            (sb-assem:inst
             ,mnemonic
             (sb-vm::float-ref-ea vector index addend ,element-size)
             value)
            (sb-c:move result value)))))))

(define-instruction-vops)
