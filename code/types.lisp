(in-package #:sb-simd)

(defmacro define-type (value-record-name)
  (with-accessors ((name value-record-name)
                   (type value-record-type))
      (find-value-record-by-name value-record-name)
    `(progn
       (deftype ,name () ',type)
       (define-inline ,name (x) (coerce x ',type))
       (export ',name))))

(defmacro define-types ()
  `(progn
     ,@(loop for value-record in *value-records*
             collect
             `(define-type ,(value-record-name value-record)))))

(define-types)
