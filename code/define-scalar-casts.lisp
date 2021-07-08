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
           ,@(unless (eq (symbol-package name) (find-package "SB-SIMD"))
               `((declare
                  (sb-vm::instruction-sets
                   ,@(available-instruction-sets
                      (find-instruction-set (symbol-package name)))))))
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

(defmacro define-scalar-casts ()
  `(progn
     ,@(loop for value-record being the hash-values of *value-records*
             when (scalar-record-p value-record)
               collect `(define-scalar-cast ,(value-record-name value-record)))))

(define-scalar-casts)
