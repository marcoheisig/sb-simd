(in-package #:sb-simd)

(defmacro define-stub (instruction-record-name)
  (with-accessors ((name instruction-record-name)
                   (argument-records instruction-record-argument-records)
                   (result-records instruction-record-result-records))
      (find-instruction-record-by-name instruction-record-name)
    (let ((arguments (subseq *arguments* 0 (length argument-records))))
      `(defun ,name ,arguments
         ,@(loop for argument in arguments
                 for type in (mapcar #'value-record-type argument-records)
                 collect `(declare (type ,type ,argument)))
         (,name ,@arguments)))))

(defmacro define-stubs ()
  `(progn
     ,@(loop for instruction-record in *instruction-records*
             collect
             `(define-stub ,(instruction-record-name instruction-record)))))

(define-stubs)
