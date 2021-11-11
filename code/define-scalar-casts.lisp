(in-package #:sb-simd-internals)

;;; For each value record we define a function of the same name that will
;;; either suitably convert its argument to that value record's type, or
;;; signal an error.

(macrolet
    ((define-scalar-cast (scalar-cast-record-name)
       (with-accessors ((name scalar-cast-record-name))
           (find-function-record scalar-cast-record-name)
         (let ((err (mksym (symbol-package name) "CANNOT-CONVERT-TO-" name)))
           `(progn
              (define-notinline ,err (x)
                (error "Cannot convert ~S to ~S." x ',name))
              (define-inline ,name (x)
                ,(instruction-set-declaration (find-instruction-set (symbol-package name)))
                (typecase x
                  ,@(cond ((subtypep name 'single-float)
                           `((,name x)
                             (double-float x (sb-kernel:%single-float x))
                             ((signed-byte 64) (sb-kernel:%single-float x))
                             (integer (sb-kernel:%single-float x)))
                           (let ((prototype (coerce 0 name)))
                             `((real (float x ,prototype)))))
                          ((subtypep name 'double-float)
                           `((,name x)
                             (single-float x (sb-kernel:%double-float x))
                             ((signed-byte 64) (sb-kernel:%double-float x))
                             (integer (sb-kernel:%double-float x))))
                          (t
                           `((,name x))))
                  (otherwise (,err x))))))))
     (define-scalar-casts ()
       `(progn
          ,@(loop for scalar-cast-record in (filter-function-records #'scalar-cast-record-p)
                  collect `(define-scalar-cast ,(function-record-name scalar-cast-record))))))
  (define-scalar-casts))
