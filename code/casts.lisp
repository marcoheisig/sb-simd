(in-package #:sb-simd)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Scalar Casts

(defmacro define-scalar-cast (scalar-record-name)
  `(define-inline ,scalar-record-name (x)
     (coerce x ',scalar-record-name)))

(defmacro define-scalar-casts ()
  `(progn
     ,@(loop for record being the hash-values of *value-records*
             when (scalar-record-p record)
               collect
             `(define-scalar-cast ,(scalar-record-name record)))))

(define-scalar-casts)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SIMD Casts

(defmacro define-simd-cast (simd-record-name)
  (with-accessors ((size simd-record-size)
                   (packer simd-record-packer)
                   (unpacker simd-record-unpacker)
                   (primitive-packer simd-record-primitive-packer)
                   (primitive-unpacker simd-record-primitive-unpacker)
                   (scalar-record simd-record-scalar-record))
      (find-value-record-by-name simd-record-name)
    (export simd-record-name)
    (export packer)
    (export unpacker)
    (let ((scalar-type (scalar-record-name scalar-record)))
      `(progn
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

(defmacro define-simd-casts ()
  `(progn
     ,@(loop for record being the hash-values of *value-records*
             when (simd-record-p record)
               collect
             `(define-simd-cast ,(simd-record-name record)))))

(define-simd-casts)
