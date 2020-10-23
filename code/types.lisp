(in-package #:sb-simd)

(defmacro define-type (value-record-name)
  (export value-record-name)
  (with-accessors ((name value-record-name)
                   (type value-record-type))
      (find-value-record-by-name value-record-name)
    `(deftype ,name () ',type)))

(defmacro define-types ()
  `(progn
     ,@(loop for value-record in *value-records*
             collect
             `(define-type ,(value-record-name value-record)))))

(define-types)
