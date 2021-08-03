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

(defun value-symbols (n &optional (package *package*))
  (loop for index below n
        collect
        (mksym package "V" (format nil "~D" index))))

(defun argument-symbols (n &optional (package *package*))
  (loop for index below n
        collect
        (mksym package "ARG-" (format nil "~D" index))))

(defun result-symbols (n &optional (package *package*))
  (loop for index below n
        collect
        (mksym package "RESULT-" (format nil "~D" index))))

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

;;; Constants

(defconstant  +u8-true+ (1- (expt 2 8)))
(defconstant +u16-true+ (1- (expt 2 16)))
(defconstant +u32-true+ (1- (expt 2 32)))
(defconstant +u64-true+ (1- (expt 2 64)))

(defconstant  +u8-false+ 0)
(defconstant +u16-false+ 0)
(defconstant +u32-false+ 0)
(defconstant +u64-false+ 0)

(defconstant  +s8-true+ -1)
(defconstant +s16-true+ -1)
(defconstant +s32-true+ -1)
(defconstant +s64-true+ -1)

(defconstant  +s8-false+ 0)
(defconstant +s16-false+ 0)
(defconstant +s32-false+ 0)
(defconstant +s64-false+ 0)

(defconstant +f32-true+ (sb-kernel:make-single-float +s32-true+))
(defconstant +f64-true+ (sb-kernel:make-double-float +s32-true+ +u32-true+))

(defconstant +f32-false+ (sb-kernel:make-single-float +s32-false+))
(defconstant +f64-false+ (sb-kernel:make-double-float +s32-false+ +u32-false+))
