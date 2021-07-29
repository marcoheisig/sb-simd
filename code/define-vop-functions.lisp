(in-package #:sb-simd-internals)

;;; Even though we mark each VOP as 'always-translatable', meaning each
;;; reference to that VOP in the source code can be open coded, we still
;;; need to introduce functions of the same name as the VOP for SBCL to use
;;; during constant folding.

(defmacro define-vop-function (name)
  (with-accessors ((name primitive-record-name)
                   (vop primitive-record-vop)
                   (argument-records primitive-record-argument-records)
                   (encoding primitive-record-encoding)
                   (instruction-set primitive-record-instruction-set))
      (find-instruction-record name)
    (let* ((arguments (argument-symbols (length argument-records)))
           (constant-arguments
             (loop for argument in arguments
                   for argument-record in argument-records
                   for primitive-type = (value-record-primitive-type argument-record)
                   when (consp primitive-type)
                     collect `(,argument 0 ,(expt 2 (second (second primitive-type)))))))
      (unless (or (eq encoding :none)
                  (not (instruction-set-available-p instruction-set)))
        `(defun ,vop (,@arguments)
           (declare (sb-vm::instruction-sets ,@(included-instruction-sets instruction-set)))
           (declare
            ,@(loop for argument in arguments
                    for argument-record in argument-records
                    collect `(type ,(value-record-name argument-record) ,argument)))
           (with-constant-arguments ,constant-arguments
             (,vop ,@arguments)))))))

(defmacro define-vop-functions ()
  `(progn
     ,@(loop for primitive-record in (filter-instruction-records #'primitive-record-p)
             for name = (primitive-record-name primitive-record)
             collect `(define-vop-function ,name))))

(define-vop-functions)
