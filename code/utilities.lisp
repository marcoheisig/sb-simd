(in-package #:sb-simd)

(defun type= (type1 type2)
  (multiple-value-bind (a1 a2) (subtypep type1 type2)
    (multiple-value-bind (b1 b2) (subtypep type2 type1)
      (values
       (and a1 b1)
       (and a2 b2)))))

(defun specialized-array-element-type-p (type)
  (type= (upgraded-array-element-type type) type))

(defun required-argument (name)
  (error "Missing required argument ~S."
         name))

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
  '(or name (cons (eql setf) (cons name null))))

(deftype index ()
  `(integer 0 (,(1- array-total-size-limit))))

(defun vop-name (name &optional (suffix ""))
  (etypecase name
    (name
     (intern (concatenate 'string "%" (symbol-name name) suffix)
             (symbol-package name)))
    ((cons (eql setf) (cons function-name null))
     (let ((name (second name)))
       (intern (concatenate 'string "%SET-" (symbol-name name) suffix)
               (symbol-package name))))))

;;; A list of symbols that we use to pick function and VOP argument names.
(defparameter *arguments* '(p0 p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15
                            p16 p17 p18 p19 p20 p21 p22 p23 p24 p25 p26 p27 p28
                            p29 p30 p31))

;; A list of symbols that we use to pick VOP result names.
(defparameter *results* '(r0 r1 r2 r3 r4 r5 r6 r7 r8 r9))
