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
   ;; Most functions in this library are generated automatically, and
   ;; depend on the available instruction sets.  The following lines ensure
   ;; that symbols that are exported once remain so, even when the
   ;; defpackage form is re-evaluated.
   . #.(when (find-package '#:sb-simd)
         (loop for it being the external-symbols of '#:sb-simd
               collect it))))
