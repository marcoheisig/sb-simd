(in-package #:sb-simd)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction VOPs

(defmacro define-instruction-vop (instruction-record-name)
  (with-accessors ((name instruction-record-name)
                   (mnemonic instruction-record-mnemonic)
                   (argument-records instruction-record-argument-records)
                   (result-records instruction-record-result-records)
                   (emitter instruction-record-emitter)
                   (cost instruction-record-cost)
                   (movable instruction-record-movable)
                   (flushable instruction-record-flushable)
                   (unsafely-flushable instruction-record-unsafely-flushable)
                   (foldable instruction-record-foldable)
                   (commutative instruction-record-commutative)
                   (first-arg-stores-result instruction-record-first-arg-stores-result))
      (find-instruction-record instruction-record-name)
    (let ((arguments (subseq *arguments* 0 (length argument-records)))
          (results (subseq *results* 0 (length result-records)))
          (vop-name (internal-name name)))
      `(progn
         ;; Make the instruction known to the compiler.
         (sb-c:defknown ,vop-name ,(mapcar #'value-record-type argument-records)
             (values ,@(mapcar #'value-record-type result-records) &optional)
             (sb-c:always-translatable
              ,@(when movable '(sb-c:movable))
              ,@(when flushable '(sb-c:flushable))
              ,@(when unsafely-flushable '(sb-c:unsafely-flushable))
              ,@(when foldable '(sb-c:foldable)))
           :overwrite-fndb-silently t)
         ;; Create a suitable VOP for this instruction.
         ,(cond
            ;; Instructions where the first argument register will
            ;; also be used to store the result.
            (first-arg-stores-result
             (destructuring-bind (result-record) result-records
               `(sb-c:define-vop (,vop-name)
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
                   (,(first results) :scs (,(value-record-register result-record))))
                  (:arg-types ,@(mapcar #'value-record-primitive-type argument-records))
                  (:result-types ,@(mapcar #'value-record-primitive-type result-records))
                  (:generator
                   ,cost
                   (sb-c:move ,(first results) ,(first arguments))
                   ,(apply emitter mnemonic (append results (rest arguments)))))))
            ;; Generate the default VOP.
            (t
             `(sb-c:define-vop (,vop-name)
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
                (:generator ,cost ,(apply emitter mnemonic (append results arguments))))))))))

(defmacro define-instruction-vops ()
  `(progn
     ,@(loop for instruction-record being the hash-values of *instruction-records*
             when (instruction-record-supported-p instruction-record)
               collect `(define-instruction-vop ,(instruction-record-name instruction-record)))))

(define-instruction-vops)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reffer VOPs

(defmacro define-reffer-vop (reffer-record-name)
  (let* ((reffer-record (find-reffer-record reffer-record-name))
         (value-record (reffer-record-value-record reffer-record))
         (name (reffer-record-name reffer-record))
         (load (reffer-record-load reffer-record))
         (store (reffer-record-store reffer-record))
         (non-temporal-load (reffer-record-non-temporal-load reffer-record))
         (non-temporal-store (reffer-record-non-temporal-store reffer-record))
         (load-c (intern (concatenate 'string (string load) "-C")))
         (store-c (intern (concatenate 'string (string store) "-C")))
         (non-temporal-load-c (intern (concatenate 'string (string non-temporal-load) "-C")))
         (non-temporal-store-c (intern (concatenate 'string (string non-temporal-store) "-C")))
         (scalar-record
           (etypecase value-record
             (scalar-record value-record)
             (simd-record (simd-record-scalar-record value-record))))
         (element-type (scalar-record-name scalar-record))
         (element-size (ceiling (scalar-record-bits scalar-record) 8))
         (scale (ash element-size (- sb-vm:n-fixnum-tag-bits)))
         (primitive-array-type (scalar-record-primitive-array-type scalar-record))
         (displacement
           (multiple-value-bind (lo hi)
               (sb-vm::displacement-bounds sb-vm:other-pointer-lowtag element-size sb-vm:vector-data-offset)
             `(integer ,lo ,hi))))
    `(progn
       ;; Make the VOPs known to the compiler.
       (sb-c:defknown ,load ((simple-array ,element-type (*)) index ,displacement) ,name
           (sb-c:movable sb-c:foldable sb-c:flushable sb-c:always-translatable)
         :overwrite-fndb-silently t)
       (sb-c:defknown ,store ((simple-array ,element-type (*)) index ,displacement ,name) ,name
           (sb-c:always-translatable)
         :overwrite-fndb-silently t)
       (sb-c:defknown ,non-temporal-load ((simple-array ,element-type (*)) index ,displacement) ,name
           (sb-c:movable sb-c:foldable sb-c:flushable sb-c:always-translatable)
         :overwrite-fndb-silently t)
       (sb-c:defknown ,non-temporal-store ((simple-array ,element-type (*)) index ,displacement ,name) ,name
           (sb-c:always-translatable)
         :overwrite-fndb-silently t)
       ;; Load
       (sb-vm::define-vop (,load)
         (:translate ,load)
         (:policy :fast-safe)
         (:args (vector :scs (sb-vm::descriptor-reg))
                (index :scs (sb-vm::any-reg)))
         (:info addend)
         (:arg-types ,primitive-array-type sb-vm::tagged-num (:constant ,displacement))
         (:results (result :scs (,(value-record-register value-record))))
         (:result-types ,(value-record-primitive-type value-record))
         (:generator
          7
          (sb-assem:inst
           ,(reffer-record-mnemonic reffer-record)
           result
           (sb-vm::float-ref-ea vector index addend ,element-size :scale ,scale))))
       (sb-vm::define-vop (,load-c)
         (:translate ,load)
         (:policy :fast-safe)
         (:args (vector :scs (sb-vm::descriptor-reg)))
         (:info index addend)
         (:arg-types ,primitive-array-type (:constant sb-vm::low-index) (:constant ,displacement))
         (:results (result :scs (,(value-record-register value-record))))
         (:result-types ,(value-record-primitive-type value-record))
         (:generator
          6
          (sb-assem:inst
           ,(reffer-record-mnemonic reffer-record)
           result
           (sb-vm::float-ref-ea vector index addend ,element-size))))
       ;; Store
       (sb-vm::define-vop (,store)
         (:translate ,store)
         (:policy :fast-safe)
         (:args (vector :scs (sb-vm::descriptor-reg))
                (index :scs (sb-vm::any-reg))
                (value :scs (,(value-record-register value-record))))
         (:info addend)
         (:arg-types ,primitive-array-type sb-vm::tagged-num (:constant ,displacement)
                     ,(value-record-primitive-type value-record))
         (:results (result :scs (,(value-record-register value-record))))
         (:result-types ,(value-record-primitive-type value-record))
         (:generator
          7
          (sb-assem:inst
           ,(reffer-record-mnemonic reffer-record)
           (sb-vm::float-ref-ea vector index addend ,element-size :scale ,scale)
           value)
          (sb-c:move result value)))
       (sb-vm::define-vop (,store-c)
         (:translate ,store)
         (:policy :fast-safe)
         (:args (vector :scs (sb-vm::descriptor-reg))
                (value :scs (,(value-record-register value-record))))
         (:info index addend)
         (:arg-types ,primitive-array-type (:constant sb-vm::low-index) (:constant ,displacement)
                     ,(value-record-primitive-type value-record))
         (:results (result :scs (,(value-record-register value-record))))
         (:result-types ,(value-record-primitive-type value-record))
         (:generator
          6
          (sb-assem:inst
           ,(reffer-record-mnemonic reffer-record)
           (sb-vm::float-ref-ea vector index addend ,element-size)
           value)
          (sb-c:move result value)))
       ;; Non-temporal Load
       ;; TODO
       ;; Non-temporal Store
       ;; TODO
       )))

(defmacro define-reffer-vops ()
  `(progn
     ,@(loop for reffer-record being the hash-values of *reffer-records*
             when (reffer-record-supported-p reffer-record)
               collect `(define-reffer-vop ,(reffer-record-name reffer-record)))))

(define-reffer-vops)
