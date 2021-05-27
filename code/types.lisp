(in-package #:sb-simd)

(defmacro define-type (value-record-name)
  (with-accessors ((name value-record-name)
                   (type value-record-type))
      (find-value-record value-record-name)
    ;; Only define a type if the value record name isn't already a type.
    (if (eq name type)
        `(export ',name)
        `(progn
           (export ',name)
           (deftype ,name () ',type)))))

(defmacro define-types ()
  `(progn
     ,@(loop for value-record being the hash-values of *value-records*
             when (value-record-supported-p value-record)
             collect `(define-type ,(value-record-name value-record)))))

(define-types)
