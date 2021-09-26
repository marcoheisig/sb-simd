(in-package #:sb-simd-internals)

(defmacro define-primitive (name)
  (with-accessors ((name primitive-record-name)
                   (vop primitive-record-vop)
                   (argument-records primitive-record-argument-records)
                   (encoding primitive-record-encoding)
                   (instruction-set primitive-record-instruction-set))
      (find-instruction-record name)
    (let ((argument-record-names (mapcar #'record-name argument-records))
          (argument-symbols (prefixed-symbols "ARGUMENT-" (length argument-records))))
      (if (not (instruction-set-available-p instruction-set))
          `(define-missing-instruction ,name
             :required-arguments ,argument-symbols)
          ;; Define the actual primitive as a wrapper around the VOP
          ;; that attempts to cast all arguments to the correct types.
          `(define-inline ,name ,argument-symbols
             (declare (sb-vm::instruction-sets ,@(included-instruction-sets instruction-set)))
             (let ,(loop for argument-symbol in argument-symbols
                         for type in (mapcar #'value-record-name argument-records)
                         collect `(,argument-symbol (,type ,argument-symbol)))
               (with-primitive-arguments
                   ,(mapcar #'list argument-symbols argument-record-names)
                 (,vop ,@argument-symbols))))))))

(defmacro define-primitives ()
  `(progn
     ,@(loop for primitive-record in (filter-instruction-records #'primitive-record-p)
             for name = (primitive-record-name primitive-record)
             collect `(define-primitive ,name))))

(define-primitives)
