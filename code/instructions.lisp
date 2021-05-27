(in-package #:sb-simd)

(defmacro define-instructions ()
  `(progn
     ,@(loop for instruction-record being the hash-values of *instruction-records*
             for name = (instruction-record-name instruction-record)
             when (instruction-record-supported-p instruction-record)
               collect
               (ecase (instruction-record-encoding instruction-record)
                 ((:standard :sse) `(define-standard-instruction ,name))
                 (:load `(define-load-instruction ,name))
                 (:store `(define-store-instruction ,name))))))

(defmacro define-standard-instruction (name)
  (with-accessors ((name instruction-record-name)
                   (argument-records instruction-record-argument-records)
                   (result-records instruction-record-result-records))
      (find-instruction-record name)
    (let ((arguments (subseq *arguments* 0 (length argument-records)))
          (vop-name (vop-name name)))
      `(progn
         (export ',name)
         ;; Define a function of the same name as the VOP.
         (defun ,vop-name ,arguments
           ,@(loop for argument in arguments
                   for type in (mapcar #'value-record-name argument-records)
                   collect `(declare (type ,type ,argument)))
           (,vop-name ,@arguments))
         ;; Define a high-level wrapper function that attempts to cast all
         ;; arguments to the correct types.
         (define-inline ,name ,arguments
           (,vop-name
            ,@(loop for argument in arguments
                    for type in (mapcar #'value-record-name argument-records)
                    if (fboundp type)
                      collect `(,type ,argument)
                    else
                      collect `(coerce ,argument ',type))))))))

(defmacro define-load-instruction (name)
  (with-accessors ((name instruction-record-name)
                   (argument-records instruction-record-argument-records)
                   (result-records instruction-record-result-records))
      (find-instruction-record name)
    (let* ((vop-name (vop-name name))
           (value-record (first result-records))
           (scalar-record
             (etypecase value-record
               (scalar-record value-record)
               (simd-record (simd-record-scalar-record value-record))))
           (simd-width
             (etypecase value-record
               (scalar-record 1)
               (simd-record (simd-record-size value-record))))
           (element-type
             (scalar-record-name scalar-record)))
      `(progn
         (export ',name)
         (define-inline ,name (array index)
           (declare (type (array ,element-type) array)
                    (index index))
           (multiple-value-bind (vector index)
               (sb-kernel:%data-vector-and-index
                array
                (sb-kernel:check-bound array (- (array-total-size array) ,(1- simd-width)) index))
             (declare (type (simple-array ,element-type (*)) vector))
             (,vop-name vector index 0)))))))

(defmacro define-store-instruction (name)
  (with-accessors ((name instruction-record-name)
                   (argument-records instruction-record-argument-records)
                   (result-records instruction-record-result-records))
      (find-instruction-record name)
    (let* ((vop-name (vop-name name))
           (value-record (first result-records))
           (scalar-record
             (etypecase value-record
               (scalar-record value-record)
               (simd-record (simd-record-scalar-record value-record))))
           (simd-width
             (etypecase value-record
               (scalar-record 1)
               (simd-record (simd-record-size value-record))))
           (element-type
             (scalar-record-name scalar-record)))
      `(progn
         (export ',name)
         (define-inline ,name (value array index)
           (declare (type (array ,element-type) array)
                    (index index))
           (multiple-value-bind (vector index)
               (sb-kernel:%data-vector-and-index
                array
                (sb-kernel:check-bound array (- (array-total-size array) ,(1- simd-width)) index))
             (declare (type (simple-array ,element-type (*)) vector))
             (,vop-name (,(value-record-name value-record) value) vector index 0)))))))

(define-instructions)
