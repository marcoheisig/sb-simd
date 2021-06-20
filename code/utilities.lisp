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

;;; Constants

(defconstant  +u8-true+ 1) ;(1- (expt 2 8))
(defconstant +u16-true+ 1) ;(1- (expt 2 16))
(defconstant +u32-true+ 1) ;(1- (expt 2 32))
(defconstant +u64-true+ 1) ;(1- (expt 2 64))

(defconstant  +u8-false+ 0)
(defconstant +u16-false+ 0)
(defconstant +u32-false+ 0)
(defconstant +u64-false+ 0)

(defconstant  +s8-true+ 1)
(defconstant +s16-true+ 1)
(defconstant +s32-true+ 1)
(defconstant +s64-true+ 1)

(defconstant  +s8-false+ 0)
(defconstant +s16-false+ 0)
(defconstant +s32-false+ 0)
(defconstant +s64-false+ 0)

(defconstant +f32-true+ 1f0) ;(sb-kernel:make-single-float +s32-true+)
(defconstant +f64-true+ 1d0) ;(sb-kernel:make-double-float +s32-true+ +u32-true+)

(defconstant +f32-false+ 0f0) ;(sb-kernel:make-single-float +s32-false+)
(defconstant +f64-false+ 0d0) ;(sb-kernel:make-double-float +s32-false+ +u32-false+)
