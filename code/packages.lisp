(in-package #:cl-user)

(defpackage #:sb-simd
  (:use #:common-lisp)
  (:export
   . #.(when (find-package '#:sb-simd)
         (loop for it being the external-symbols of '#:sb-simd
               collect it))))
