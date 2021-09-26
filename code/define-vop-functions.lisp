(in-package #:sb-simd-internals)

;;; We need to introduce functions of the same name as the VOP for SBCL to
;;; use during constant folding.

(defmacro with-primitive-arguments (alist &body body)
  ;; Each entry in ALIST is of the form (VARIABLE VALUE-RECORD-NAME).
  (if (null alist)
      `(progn ,@body)
      `(with-primitive-argument ,(first alist)
         (with-primitive-arguments ,(rest alist)
           ,@body))))

(defmacro with-primitive-argument ((symbol value-record) &body body)
  (with-accessors ((primitive-type value-record-primitive-type)
                   (simd-p simd-record-p))
      (find-value-record value-record)
    (etypecase primitive-type
      ;; Case 1: A symbol denoting a primitive type.
      (symbol
       (let ((alias (find primitive-type sb-c::*backend-primitive-type-aliases* :key #'car)))
         ;; ALIAS is either null or a list of the form (:OR PRIMITIVE-TYPE*)
         (if (or (not simd-p) (not alias))
             `(progn ,@body)
             `(etypecase ,symbol
                ,@(loop for pt in (rest (rest alias))
                        for type = (sb-c::primitive-type-specifier (sb-c:primitive-type-or-lose pt))
                        collect `(,type ,@body))))))
      ;; Case 2: A list of the form (:CONSTANT TYPE), where TYPE is either
      ;; of the form (SIGNED-BYTE N) or (UNSIGNED-BYTE N) for some positive
      ;; integer N.
      ((cons (eql :constant) (cons type-specifier null))
       (multiple-value-bind (low high)
           (integer-type-specifier-inclusive-bounds (second primitive-type))
         `(ecase ,symbol
            ,@(loop for value from low to high
                    collect `(,value (symbol-macrolet ((,symbol ,value)) ,@body)))))))))

(defmacro define-vop-function (name)
  (with-accessors ((name primitive-record-name)
                   (vop primitive-record-vop)
                   (argument-records primitive-record-argument-records)
                   (encoding primitive-record-encoding)
                   (instruction-set primitive-record-instruction-set))
      (find-instruction-record name)
    (let* ((argument-record-names (mapcar #'record-name argument-records))
           (argument-symbols (prefixed-symbols "ARGUMENT-" (length argument-records))))
      (unless (or (eq encoding :none)
                  (not (instruction-set-available-p instruction-set)))
        `(defun ,vop (,@argument-symbols)
           (declare (sb-vm::instruction-sets ,@(included-instruction-sets instruction-set)))
           (declare
            ,@(loop for argument-symbol in argument-symbols
                    for argument-record in argument-records
                    collect `(type ,(value-record-name argument-record) ,argument-symbol)))
           (with-primitive-arguments ,(mapcar #'list argument-symbols argument-record-names)
             (,vop ,@argument-symbols)))))))

(defmacro define-vop-functions ()
  `(progn
     ,@(loop for primitive-record in (filter-instruction-records #'primitive-record-p)
             for name = (primitive-record-name primitive-record)
             collect `(define-vop-function ,name))))

(define-vop-functions)
