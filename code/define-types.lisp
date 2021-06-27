(in-package #:sb-simd)

(defmacro define-scalar-type (scalar-record-name)
  (with-accessors ((name scalar-record-name)
                   (type scalar-record-type))
      (find-value-record scalar-record-name)
    `(progn
       ;; Define the name as an alias for the type.
       (deftype ,name () ',type)
       ;; Define a trivial constructor.
       (define-inline ,scalar-record-name (x)
         ,(cond ((subtypep type 'float)
                 `(float x ,(coerce 0 type)))
                ((subtypep type 'complex)
                 `(coerce x ',type))
                (t `(the ,type x)))))))

(defmacro define-simd-type (simd-record-name)
  (with-accessors ((name simd-record-name)
                   (size simd-record-size)
                   (type simd-record-type)
                   (packer simd-record-packer)
                   (unpacker simd-record-unpacker)
                   (primitive-packer simd-record-primitive-packer)
                   (primitive-unpacker simd-record-primitive-unpacker)
                   (scalar-record simd-record-scalar-record))
      (find-value-record simd-record-name)
    (let ((scalar-type (scalar-record-name scalar-record))
          (arguments (argument-symbols size)))
      `(progn
         ;; Define the name as an alias for the type.
         (deftype ,name () ',type)
         ;; Define a packer.
         (define-inline ,packer (,@arguments)
           (,primitive-packer
            ,@(loop for argument in arguments collect `(,scalar-type ,argument))))
         ;; Define a cast function.
         (sb-c:defknown ,simd-record-name (t) (values ,type &optional)
             (sb-c:foldable sb-c:flushable sb-c:movable)
           :overwrite-fndb-silently t)
         (defun ,simd-record-name (x)
           (etypecase x
             (real
              (let ((.scalar. (,scalar-type x)))
                (,packer ,@(loop repeat size collect '.scalar.))))
             (,simd-record-name x)))
         ;; Optimize the case where the value is already of the correct type.
         (sb-c:deftransform ,simd-record-name ((x) (,type))
           'x)
         ;; Optimize the case where the value is not of the correct type.
         (sb-c:deftransform ,simd-record-name ((x) ((not ,type)))
           `(let ((.value. (,',scalar-type x)))
              (,',packer ,@',(loop repeat size collect '.value.))))
         ;; Define an unpacker.
         (define-inline ,unpacker (x)
           (,primitive-unpacker (,simd-record-name x)))))))

(defmacro define-types ()
  `(progn
     ,@(loop for value-record being the hash-values of *value-records*
             collect
             (etypecase value-record
               (scalar-record `(define-scalar-type ,(value-record-name value-record)))
               (simd-record `(define-simd-type ,(value-record-name value-record)))))))

(define-types)
