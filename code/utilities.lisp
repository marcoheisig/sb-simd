(in-package #:sb-simd)

(defmacro define-inline (name lambda-list &body body)
  `(progn
     (declaim (inline ,name))
     (defun ,name ,lambda-list ,@body)))

(defmacro macro-when (condition &body body)
  (when condition `(progn ,@body)))

(deftype type-specifier ()
  '(or symbol cons))

(deftype name ()
  '(and symbol (not null)))

(deftype function-name ()
  '(or name (cons (eql setf) name)))

(defun vop-name (name)
  (intern (concatenate 'string "%" (symbol-name name))
          (symbol-package name)))

;;; A list of symbols that we use to pick function and VOP argument names.
(defparameter *arguments* '(a b c d e f g h i j k l m n o p q r s t u v w x y z))

;; A list of symbols that we use to pick VOP result names.
(defparameter *results* '(r0 r1 r2 r3 r4 r5 r6 r7 r8 r9))
