(in-package #:sb-simd)

;;; For each value record we define a function of the same name that will
;;; either suitably convert its argument to that value record's type, or
;;; signal an error.

(defmacro define-scalar-cast (scalar-record-name)
  (with-accessors ((name scalar-record-name))
      (find-value-record scalar-record-name)
    (let ((err (mksym (symbol-package name) "CANNOT-CONVERT-TO-" name)))
      `(progn
         (define-notinline ,err (x)
           (error "Cannot convert ~S to ~S." x ',name))
         (define-inline ,name (x)
           (typecase x
             ,@(cond ((subtypep name 'integer)
                      `((,name x)))
                     ((subtypep name 'float)
                      (let ((prototype (coerce 0 name)))
                        `((real (float x ,prototype)))))
                     ((subtypep name 'complex)
                      (let ((prototype (realpart (coerce 0 name))))
                        `((real (complex (float x ,prototype)))
                          (complex
                           (complex (float (realpart x) ,prototype)
                                    (float (imagpart x) ,prototype))))))
                     (t
                      `((,name x))))
             (otherwise (,err x))))))))

(defmacro define-scalar-casts ()
  `(progn
     ,@(loop for value-record being the hash-values of *value-records*
             when (scalar-record-p value-record)
               collect `(define-scalar-cast ,(value-record-name value-record)))))

(define-scalar-casts)
