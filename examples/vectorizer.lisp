(defpackage #:vectorizer-examples
  (:use #:common-lisp #:sb-simd))

(in-package #:vectorizer-examples)

(defun f64-copy (dst src)
  (declare (type (simple-array f64) dst src))
  (do-vectorized (index 0 (array-total-size src))
    (:unroll 4)
    (setf (f64-row-major-aref dst index)
          (f64-row-major-aref src index))))

(defun f64-fill (array constant)
  (declare (type (simple-array f64) array)
           (f64 constant))
  (do-vectorized (i 0 (array-total-size array))
    (setf (f64-row-major-aref array i) constant))
  array)

(defun jacobi (dst src)
  (declare (type (simple-array f64 2) dst src))
  (loop for i from 1 below (1- (array-dimension dst 0)) do
    (do-vectorized (j 1 (1- (array-dimension dst 1)))
      (:unroll 2)
      (setf (f64-aref dst i j)
            (f64* 0.25d0
                  (f64+
                   (f64-aref src i (1+ j))
                   (f64-aref src i (1- j))
                   (f64-aref src (1+ i) j)
                   (f64-aref src (1- i) j)))))))

(defun jacobi2 (dst src)
  (declare (type (simple-array f64 2) dst src))
  (loop for j from 1 below (1- (array-dimension dst 1)) do
    (do-vectorized (i 1 (1- (array-dimension dst 0)))
      (setf (f64-aref dst i j)
            (f64* 0.25d0
                  (f64+
                   (f64-aref src i (1+ j))
                   (f64-aref src i (1- j))
                   (f64-aref src (1+ i) j)
                   (f64-aref src (1- i) j)))))))

(defun run-jacobi-benchmark ()
  (let ((dst (make-array '(4 400) :element-type 'double-float
                                  :initial-element 0d0))
        (src (make-array '(4 400) :element-type 'double-float
                                  :initial-element 0d0)))
    (time
     (loop repeat 500000 do
       (jacobi dst src)
       (jacobi src dst)))))
