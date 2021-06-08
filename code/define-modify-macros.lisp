(in-package #:sb-simd-sse)

(define-modify-macro f32.4-incf (&optional (num 1f0)) two-arg-f32.4+)
(define-modify-macro f32.4-decf (&optional (num 1f0)) two-arg-f32.4-)

(in-package #:sb-simd-sse2)

(define-modify-macro f64.2-incf (&optional (num 1d0)) two-arg-f64.2+)
(define-modify-macro f64.2-decf (&optional (num 1d0)) two-arg-f64.2-)

(define-modify-macro u64.2-incf (&optional (num 1)) two-arg-u64.2+)
(define-modify-macro u64.2-decf (&optional (num 1)) two-arg-u64.2-)

(define-modify-macro u64.4-incf (&optional (num 1)) two-arg-u64.4+)
(define-modify-macro u64.4-decf (&optional (num 1)) two-arg-u64.4-)


(in-package #:sb-simd-avx)

(define-modify-macro f32.4-incf (&optional (num 1f0)) two-arg-f32.4+)
(define-modify-macro f32.4-decf (&optional (num 1f0)) two-arg-f32.4-)

(define-modify-macro f64.2-incf (&optional (num 1d0)) two-arg-f64.2+)
(define-modify-macro f64.2-decf (&optional (num 1d0)) two-arg-f64.2-)

(define-modify-macro f32.8-incf (&optional (num 1f0)) two-arg-f32.8+)
(define-modify-macro f32.8-decf (&optional (num 1f0)) two-arg-f32.8-)

(define-modify-macro f64.4-incf (&optional (num 1d0)) two-arg-f64.4+)
(define-modify-macro f64.4-decf (&optional (num 1d0)) two-arg-f64.4-)

(in-package #:sb-simd-avx2)

(define-modify-macro u64.2-incf (&optional (num 1)) two-arg-u64.2+)
(define-modify-macro u64.2-decf (&optional (num 1)) two-arg-u64.2-)

(define-modify-macro u64.4-incf (&optional (num 1)) two-arg-u64.4+)
(define-modify-macro u64.4-decf (&optional (num 1)) two-arg-u64.4-)

(define-modify-macro u32.4-incf (&optional (num 1)) two-arg-u32.4+)
(define-modify-macro u32.4-decf (&optional (num 1)) two-arg-u32.4-)

(define-modify-macro u32.8-incf (&optional (num 1)) two-arg-u32.8+)
(define-modify-macro u32.8-decf (&optional (num 1)) two-arg-u32.8-)

