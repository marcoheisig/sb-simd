(defpackage #:vectorizer-examples
  (:use #:common-lisp #:sb-simd))

(in-package #:vectorizer-examples)

(defun copy (dst src)
  (do-vectorized (index 0 (array-total-size src))
    (:unroll 4)
    (setf (f64-row-major-aref dst index)
          (f64-row-major-aref src index))))

(defun f64-fill (array constant)
  (let ((f64 (f64 constant)))
    (loop for i below (array-dimension array 0) do
      (do-vectorized (j 0 (array-dimension array 1))
        (setf (f64-aref array i j) f64)))))

(defun jacobi (dst src)
  (loop for i from 1 below (1- (array-dimension dst 0)) do
    (do-vectorized (j 1 (1- (array-dimension dst 1)))
      (setf (f64-aref dst i j)
            (f64* 0.25d0
                  (f64+
                   (f64-aref src i (1+ j))
                   (f64-aref src i (1- j))
                   (f64-aref src (1+ i) j)
                   (f64-aref src (1- i) j)))))))
