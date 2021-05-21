(in-package #:cl-user)

(defpackage #:sb-simd
  (:use #:common-lisp)
  (:export
   #:+sse+
   #:+sse2+
   #:+sse3+
   #:+ssse3+
   #:+sse4.1+
   #:+sse4.2+
   #:+avx+
   #:+avx2+
   #:+fma+
   .
   #.(when (find-package '#:sb-simd)
       (loop for it being the external-symbols of '#:sb-simd
             collect it))))
