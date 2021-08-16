(in-package #:sb-simd-internals)

;;; Types

(deftype type-specifier ()
  '(or symbol cons))

(deftype non-nil-symbol ()
  '(and symbol (not null)))

(deftype function-name ()
  '(or non-nil-symbol (cons (eql setf) (cons non-nil-symbol null))))

(deftype index ()
  `(integer 0 (,(1- array-total-size-limit))))

;;; Functions

(defun ensure-package (name)
  (or (find-package name)
      (make-package name)))

(defun mksym (package &rest string-designators)
  (intern
   (apply #'concatenate 'string (mapcar #'string string-designators))
   package))

(defun prefixed-symbols (prefix n &optional (package *package*))
  (loop for index below n
        collect
        (mksym package prefix (format nil "~D" index))))

(declaim (notinline touch))
(defun touch (&rest arguments &aux value)
  (declare (ignore arguments))
  (declare (special value))
  (values-list (loop repeat (random 100) collect value)))

;;; Macros

(defmacro define-inline (name lambda-list &body body)
  `(progn
     (declaim (inline ,name))
     (defun ,name ,lambda-list ,@body)))

(defmacro define-notinline (name lambda-list &body body)
  `(progn
     (declaim (notinline ,name))
     (defun ,name ,lambda-list ,@body)))

(defmacro with-constant-arguments (arguments &body body)
  (if (null arguments)
      `(progn ,@body)
      `(with-constant-argument ,(first arguments)
         (with-constant-arguments ,(rest arguments) ,@body))))

(defmacro with-constant-argument ((argument low high) &body body)
  (check-type argument symbol)
  `(ecase ,argument
     ,@(loop for value from low below high
             collect `(,value (symbol-macrolet ((,argument ,value)) ,@body)))))
