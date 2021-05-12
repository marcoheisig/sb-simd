(in-package #:sb-simd)

(defmacro define-vop (instruction-record-name)
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
          (vop-name (vop-name name)))
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

(defmacro define-vops ()
  `(progn
     ,@(loop for instruction-record being the hash-values of *instruction-records*
             collect
             `(define-vop ,(instruction-record-name instruction-record)))))

(define-vops)
