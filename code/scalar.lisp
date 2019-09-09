(in-package #:sb-simd)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Types

(defmacro define-scalar-type (scalar-record)
  (let ((name (scalar-record-name scalar-record))
        (type (scalar-record-type scalar-record)))
    `(deftype ,name () ',type)))

(defmacro define-scalar-types ()
  `(progn
     ,@(loop for scalar-record in *scalar-records*
             collect
             `(define-scalar-type ,scalar-record))))

(define-scalar-types)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Constructors

(defmacro define-scalar-constructor (scalar-record)
  (let ((name (constructor-name scalar-record)))
    `(define-inline ,name (x)
         (coerce x ',(scalar-record-type scalar-record)))))

(defmacro define-scalar-constructors ()
  `(progn
     ,@(loop for scalar-record in *scalar-records*
             collect
             `(define-scalar-constructor ,scalar-record))))

(define-scalar-constructors)
