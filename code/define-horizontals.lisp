(in-package #:sb-simd-internals)

;;; For each associative SIMD function X.Y-FOO, define a function
;;; X.Y-HORIZONTAL-FOO that combines all elements of a SIMD pack of type
;;; X.Y with that associative function in a hierarchical fashion.

(in-package #:sb-simd-sse2)

#|

(define-fake-vop f64.2-hsum (x)
  (multiple-value-call #'%two-arg-f64+
    (%f64.2-values x)))

(define-fake-vop f64.2-hprod (x)
  (multiple-value-call #'%two-arg-f64*
    (%f64.2-values x)))

(define-fake-vop f32.8-hsum (x)
  (%f32.4-hsum
   (%two-arg-f32.4+
    (%f32.4-from-f32.8 x 0)
    (%f32.4-from-f32.8 x 1))))

(define-fake-vop f32.8-hprod (x)
  (%f32.4-hprod
   (%two-arg-f32.4*
    (%f32.4-from-f32.8 x 0)
    (%f32.4-from-f32.8 x 1))))

(define-fake-vop f64.4-hsum (x)
  (%f64.2-hsum
   (%two-arg-f64.2+
    (%f64.2-from-f64.4 x 0)
    (%f64.2-from-f64.4 x 1))))

(define-fake-vop f64.4-hprod (x)
  (%f64.2-hprod
   (%two-arg-f64.2*
    (%f64.2-from-f64.4 x 0)
    (%f64.2-from-f64.4 x 1))))

(define-fake-vop f32.4-hsum (x)
  (multiple-value-bind (a b c d) (%f32.4-values x)
    (%two-arg-f32+
     (%two-arg-f32+ a b)
     (%two-arg-f32+ c d))))

(define-fake-vop f32.4-hprod (x)
  (multiple-value-bind (a b c d) (%f32.4-values x)
    (%two-arg-f32*
     (%two-arg-f32* a b)
     (%two-arg-f32* c d))))

(define-fake-vop f64.2-hsum (x)
  (multiple-value-call #'%two-arg-f64+
    (%f64.2-values x)))

(define-fake-vop f64.2-hprod (x)
  (multiple-value-call #'%two-arg-f64*
    (%f64.2-values x)))
|#
