(in-package #:sb-simd)

(defmacro define-scalar-cast (scalar-record-name)
  `(progn
     (export ',scalar-record-name)
     (define-inline ,scalar-record-name (x)
       (coerce x ',scalar-record-name))))

(defmacro define-simd-cast (simd-record-name)
  (with-accessors ((size simd-record-size)
                   (packer simd-record-packer)
                   (unpacker simd-record-unpacker)
                   (primitive-packer simd-record-primitive-packer)
                   (primitive-unpacker simd-record-primitive-unpacker)
                   (scalar-record simd-record-scalar-record))
      (find-value-record simd-record-name)
    (let ((scalar-type (scalar-record-name scalar-record)))
      `(progn
         (export ',simd-record-name)
         (export ',packer)
         (export ',unpacker)
         ;; Define a packer.
         (define-inline ,packer ,(subseq *arguments* 0 size)
           (,primitive-packer
            ,@(loop for argument in (subseq *arguments* 0 size)
                    collect `(,scalar-type ,argument))))
         ;; Define a cast function.
         (define-inline ,simd-record-name (x)
           (typecase x
             (,simd-record-name x)
             (otherwise
              (let ((,scalar-type (,scalar-type x)))
                (,packer ,@(loop repeat size collect scalar-type))))))
         ;; Define an unpacker.
         (define-inline ,unpacker (x)
           (,primitive-unpacker (,simd-record-name x)))))))

(defmacro define-cast (value-record-name)
  (etypecase (find-value-record value-record-name)
    (scalar-record `(define-scalar-cast ,value-record-name))
    (simd-record `(define-simd-cast ,value-record-name))))

(defmacro define-casts ()
  `(progn
     ,@(loop for value-record being the hash-values of *value-records*
             when (and (value-record-supported-p value-record)
                       ;; Don't generate casts for array types.
                       (not (subtypep (value-record-type value-record) 'array)))
               collect `(define-cast ,(value-record-name value-record)))))

(define-casts)
