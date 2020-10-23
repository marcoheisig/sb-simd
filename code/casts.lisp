(in-package #:sb-simd)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Scalar Casts

(defmacro define-scalar-cast (scalar-record-name)
  `(define-inline ,scalar-record-name (x)
     (coerce x ',scalar-record-name)))

(defmacro define-scalar-casts ()
  `(progn
     ,@(loop for scalar-record in *scalar-records*
             collect
             `(define-scalar-cast ,(scalar-record-name scalar-record)))))

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
    (let ((scalar-type (scalar-record-name scalar-record)))
      `(progn
         ;; Define a packer.
         (define-inline ,packer ,(subseq *arguments* 0 size)
           (,primitive-packer
            ,@(loop for argument in (subseq *arguments* 0 size)
                    collect `(,scalar-type ,argument))))
         (export ',packer)
         ;; Define a cast function.
         (define-inline ,simd-record-name (x)
           (typecase x
             (,simd-record-name x)
             (otherwise
              (let ((,scalar-type (,scalar-type x)))
                (,packer ,@(loop repeat size collect scalar-type))))))
         ;; Define an unpacker.
         (define-inline ,unpacker (x)
           (,primitive-unpacker (,simd-record-name x)))
         (export ',unpacker)))))

(defmacro define-simd-casts ()
  `(progn
     ,@(loop for simd-record in *simd-records*
             collect
             `(define-simd-cast ,(simd-record-name simd-record)))))

(define-simd-casts)
