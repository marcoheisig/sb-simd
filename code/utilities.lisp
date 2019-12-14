(in-package #:sb-simd)

(defmacro define-inline (name lambda-list &body body)
  `(progn
     (declaim (inline ,name))
     (defun ,name ,lambda-list ,@body)))

(defun valid-type-p (type-specifier)
  (ignore-errors (typep 42 type-specifier) t))
