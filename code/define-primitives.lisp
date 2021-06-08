(in-package #:sb-simd)

(defmacro define-primitive (name)
  (with-accessors ((name primitive-record-name)
                   (argument-records primitive-record-argument-records)
                   (result-records primitive-record-result-records))
      (find-instruction-record name)
    (let ((vop-name (mksym (symbol-package name) "%" name))
          (arguments (argument-symbols (length argument-records))))
      (if (not (instruction-available-p name))
          `(define-missing-instruction ,name
             :required-arguments ',arguments)
          `(progn
             ;; Define a function of the same name as the VOP.
             (defun ,vop-name ,arguments
               ,@(loop for argument in arguments
                       for type in (mapcar #'value-record-name argument-records)
                       collect `(declare (type ,type ,argument)))
               (,vop-name ,@arguments))
             ;; Define a high-level wrapper function that attempts to cast all
             ;; arguments to the correct types.
             (define-inline ,name ,arguments
               (,vop-name
                ,@(loop for argument in arguments
                        for type in (mapcar #'value-record-name argument-records)
                        if (fboundp type)
                          collect `(,type ,argument)
                        else
                          collect `(coerce ,argument ',type)))))))))

(defmacro define-primitives ()
  `(progn
     ,@(loop for primitive-record in (filter-instruction-records #'primitive-record-p)
             for name = (primitive-record-name primitive-record)
             collect `(define-primitive ,name))))

(define-primitives)
