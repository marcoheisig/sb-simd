(in-package #:sb-simd)

(defmacro define-instruction (instruction-record-name)
  (with-accessors ((name instruction-record-name)
                   (argument-records instruction-record-argument-records)
                   (result-records instruction-record-result-records))
      (find-instruction-record instruction-record-name)
    (let ((arguments (subseq *arguments* 0 (length argument-records)))
          (vop-name (internal-name name)))
      `(progn
         (export ',name)
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
                      collect `(coerce ,argument ',type))))))))

(defmacro define-instructions ()
  `(progn
     ,@(loop for instruction-record being the hash-values of *instruction-records*
             when (instruction-record-supported-p instruction-record)
             collect
             `(define-instruction ,(instruction-record-name instruction-record)))))

(define-instructions)
