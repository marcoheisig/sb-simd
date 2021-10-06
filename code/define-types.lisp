(in-package #:sb-simd-internals)

(macrolet
    ((define-type (value-record-name)
       (with-accessors ((name value-record-name)
                        (type value-record-type))
           (find-value-record value-record-name)
         `(deftype ,name () ',type)))
     (define-types ()
       `(progn
          ,@(loop for value-record being the hash-values of *value-records*
                  collect
                  `(define-type ,(value-record-name value-record))))))
  (define-types))
