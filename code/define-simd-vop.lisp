(in-package #:sb-simd)

(defun primitive-type (object)
  (etypecase object
    (symbol
     (cond ((ignore-errors (find-simd-record :name object))
            (primitive-type
             (find-simd-record :name object)))
           ((ignore-errors (find-scalar-record :type object))
            (primitive-type
             (find-scalar-record :type object)))
           (t object)))
    (scalar-record (scalar-record-type object))
    (simd-record (simd-record-primitive-type object))
    (t object)))

(defmacro define-simd-vop (name ((result result-type) &rest arguments)
                             &body options)
  (let ((args (mapcar #'first arguments))
        (types (mapcar #'second arguments))
        (lambda-list (mapcar #'caar arguments)))
    `(progn
       (sb-c:defknown ,name ,types ,result-type
           (sb-c:movable sb-c:flushable sb-c:always-translatable)
         :overwrite-fndb-silently t)
       (sb-c:define-vop (,name)
         (:translate ,name)
         (:policy :fast-safe)
         (:args ,@args)
         (:results ,result)
         (:arg-types ,@(mapcar #'primitive-type types))
         (:result-types ,(primitive-type result-type))
         ,@options)
       (defun ,name ,lambda-list
         ,@(loop for var in lambda-list
                 for type in types
                 when (valid-type-p type)
                 collect `(declare (type ,type ,var)))
         (,name ,@lambda-list)))))
