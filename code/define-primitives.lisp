(in-package #:sb-simd-internals)

(defmacro define-primitive (name)
  (with-accessors ((name primitive-record-name)
                   (vop primitive-record-vop)
                   (argument-records primitive-record-argument-records)
                   (encoding primitive-record-encoding)
                   (instruction-set primitive-record-instruction-set))
      (find-instruction-record name)
    (let* ((arguments (argument-symbols (length argument-records)))
           (constant-arguments
             (loop for argument in arguments
                   for argument-record in argument-records
                   for primitive-type = (value-record-primitive-type argument-record)
                   when (consp primitive-type)
                     collect `(,argument 0 ,(expt 2 (second (second primitive-type)))))))
      (if (not (instruction-set-available-p instruction-set))
          `(define-missing-instruction ,name
             :required-arguments ,arguments)
          ;; Define the actual primitive as a wrapper around the VOP
          ;; that attempts to cast all arguments to the correct types.
          `(define-inline ,name ,arguments
             (declare (sb-vm::instruction-sets ,@(included-instruction-sets instruction-set)))
             (let ,(loop for argument in arguments
                         for type in (mapcar #'value-record-name argument-records)
                         collect `(,argument (,type ,argument)))
               (with-constant-arguments ,constant-arguments
                 (,vop ,@arguments))))))))

(defmacro define-primitives ()
  `(progn
     ,@(loop for primitive-record in (filter-instruction-records #'primitive-record-p)
             for name = (primitive-record-name primitive-record)
             collect `(define-primitive ,name))))

(define-primitives)
