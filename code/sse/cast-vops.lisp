(in-package #:sb-simd)

#+(or)
(defun f64.2-from-u64.2 (x)
  (declare (type u64.2 x))
  (multiple-value-bind (a b) (u64.2-values x)
    (make-f64.2 (coerce a 'f64) (coerce b 'f64))))
