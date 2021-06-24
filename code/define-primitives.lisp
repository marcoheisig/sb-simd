(in-package #:sb-simd)

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
          `(progn
             ;; Define a function with the same name as the VOP that SBCL
             ;; can use for constant folding.
             ,@(unless (eq encoding :none)
                 `((defun ,vop (,@arguments)
                     (declare
                      ,@(loop for argument in arguments
                              for argument-record in argument-records
                              collect `(type ,(value-record-name argument-record) ,argument)))
                     (with-constant-arguments ,constant-arguments
                       (,vop ,@arguments)))))
             ;; Define the actual primitive as a wrapper around the VOP
             ;; that attempts to cast all arguments to the correct types.
             (define-inline ,name ,arguments
               (with-constant-arguments ,constant-arguments
                 (,vop
                  ,@(loop for argument in arguments
                          for type in (mapcar #'value-record-name argument-records)
                          collect `(,type ,argument))))))))))

(defmacro with-constant-arguments (arguments &body body)
  (if (null arguments)
      `(progn ,@body)
      `(with-constant-argument ,(first arguments)
         (with-constant-arguments ,(rest arguments) ,@body))))

(defmacro with-constant-argument ((argument low high) &body body)
  (check-type argument symbol)
  `(ecase ,argument
     ,@(loop for value from low below high
             collect `(,value (symbol-macrolet ((,argument ,value)) ,@body)))))

(defmacro define-primitives ()
  `(progn
     ,@(loop for primitive-record in (filter-instruction-records #'primitive-record-p)
             for name = (primitive-record-name primitive-record)
             collect `(define-primitive ,name))))

(define-primitives)
