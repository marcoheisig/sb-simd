(in-package #:sb-simd)

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

(defun argument-symbols (n &optional (package *package*))
  (loop for index below n
        collect
        (mksym package "ARG-" (format nil "~D" index))))

(defun result-symbols (n &optional (package *package*))
  (loop for index below n
        collect
        (mksym package "RESULT-" (format nil "~D" index))))

;;; Macros

(defmacro define-inline (name lambda-list &body body)
  `(progn
     (declaim (inline ,name))
     (defun ,name ,lambda-list ,@body)))

