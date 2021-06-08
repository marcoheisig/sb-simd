(in-package #:sb-simd)

(defmacro define-primitive (name)
  (with-accessors ((name primitive-record-name)
                   (vop primitive-record-vop)
                   (argument-records primitive-record-argument-records)
                   (encoding primitive-record-encoding))
      (find-instruction-record name)
    (let ((arguments (argument-symbols (length argument-records))))
      (if (not (instruction-available-p name))
          `(define-missing-instruction ,name
             :required-arguments ',arguments)
          `(progn
             ;; Define a function with the same name as the VOP that SBCL
             ;; can use for constant folding.
             ,@(unless (eq encoding :none)
                 `((defun ,vop (,@arguments)
                (declare
                 ,@(loop for argument in arguments
                         for argument-record in argument-records
                         collect `(type ,(value-record-name argument-record) ,argument)))
                (,vop ,@arguments))))
             ;; Define the actual primitive as a wrapper around the VOP
             ;; that attempts to cast all arguments to the correct types.
             (define-inline ,name ,arguments
               (,vop
                ,@(loop for argument in arguments
                        for type in (mapcar #'value-record-name argument-records)
                        collect `(,type ,argument)))))))))

(defmacro define-primitives ()
  `(progn
     ,@(loop for primitive-record in (filter-instruction-records #'primitive-record-p)
             for name = (primitive-record-name primitive-record)
             collect `(define-primitive ,name))))

(define-primitives)
