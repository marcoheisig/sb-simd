(in-package #:sb-simd)

(defmacro define-stub (instruction-record-name)
  (with-accessors ((name instruction-record-name)
                   (argument-records instruction-record-argument-records)
                   (result-records instruction-record-result-records))
      (find-instruction-record-by-name instruction-record-name)
    (export name)
    (let ((arguments (subseq *arguments* 0 (length argument-records)))
          (vop-name (vop-name name)))
      `(progn
         (defun ,vop-name ,arguments
           ,@(loop for argument in arguments
                   for type in (mapcar #'value-record-name argument-records)
                   collect `(declare (type ,type ,argument)))
           (,vop-name ,@arguments))
         (define-inline ,name ,arguments
           (,vop-name
            ,@(loop for argument in arguments
                    for type in (mapcar #'value-record-name argument-records)
                    if (fboundp type)
                      collect `(,type ,argument)
                    else
                      collect `(coerce ,argument ',type))))))))

(defmacro define-stubs ()
  `(progn
     ,@(loop for instruction-record in *instruction-records*
             collect
             `(define-stub ,(instruction-record-name instruction-record)))))

(define-stubs)
