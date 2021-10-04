(in-package #:sb-simd-internals)

(defmacro define-vref (name kind)
  (with-accessors ((name vref-record-name)
                   (instruction-set vref-record-instruction-set)
                   (value-record vref-record-value-record)
                   (vector-record vref-record-vector-record)
                   (vop vref-record-vop))
      (find-instruction-record name)
    (let* ((simd-width
             (etypecase value-record
               (scalar-record 1)
               (simd-record (simd-record-size value-record))))
           (element-type
             (second
              (value-record-type vector-record))))
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
              (,vop vector index 0))))
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
              (,vop (,(value-record-name value-record) value) vector index 0))))))))

(defmacro define-loads-and-stores ()
  `(progn
     ,@(loop for load-record in (filter-instruction-records #'load-record-p)
             for name = (load-record-name load-record)
             collect `(define-vref ,name :load))
     ,@(loop for store-record in (filter-instruction-records #'store-record-p)
             for name = (store-record-name store-record)
             collect `(define-vref ,name :store))))

(define-loads-and-stores)
