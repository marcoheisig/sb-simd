(in-package #:sb-simd-internals)

(defmacro define-vector-ref (name kind)
  (with-accessors ((name vref-record-name)
                   (instruction-set vref-record-instruction-set)
                   (value-record vref-record-value-record))
      (find-instruction-record name)
    (let* ((vop-name (mksym (symbol-package name) "%" name))
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
      (ecase kind
        (:load
         `(define-inline ,name (array index)
            (declare (sb-vm::instruction-sets ,@(included-instruction-sets instruction-set)))
            (declare (type (array ,element-type) array)
                     (index index))
            (multiple-value-bind (vector index)
                (sb-kernel:%data-vector-and-index
                 array
                 (sb-kernel:check-bound array (- (array-total-size array) ,(1- simd-width)) index))
              (declare (type (simple-array ,element-type (*)) vector))
              (,vop-name vector index 0))))
        (:store
         `(define-inline ,name (value array index)
            (declare (sb-vm::instruction-sets ,@(included-instruction-sets instruction-set)))
            (declare (type (array ,element-type) array)
                     (index index))
            (multiple-value-bind (vector index)
                (sb-kernel:%data-vector-and-index
                 array
                 (sb-kernel:check-bound array (- (array-total-size array) ,(1- simd-width)) index))
              (declare (type (simple-array ,element-type (*)) vector))
              (,vop-name (,(value-record-name value-record) value) vector index 0))))))))

(defmacro define-loads-and-stores ()
  `(progn
     ,@(loop for load-record in (filter-instruction-records #'load-record-p)
             for name = (load-record-name load-record)
             collect `(define-vector-ref ,name :load))
     ,@(loop for store-record in (filter-instruction-records #'store-record-p)
             for name = (store-record-name store-record)
             collect `(define-vector-ref ,name :store))))

(define-loads-and-stores)
