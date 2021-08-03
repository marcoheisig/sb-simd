(in-package #:sb-simd-test-suite)

(define-test packages
  (check-package '#:sb-simd-internals)
  #+(or)
  (check-package '#:sb-simd-common)
  (check-package '#:sb-simd-x86-64)
  (check-package '#:sb-simd-sse)
  (check-package '#:sb-simd-sse2)
  (check-package '#:sb-simd-sse3)
  (check-package '#:sb-simd-ssse3)
  (check-package '#:sb-simd-sse4.1 :skip '(#:f32.4-extract))
  (check-package '#:sb-simd-sse4.2 :skip '(#:f32.4-extract))
  (check-package '#:sb-simd-avx
                 :skip '(#:u32.8-from-f32.8
                         #:f32.4-vsum
                         #:f32.4-hsum
                         #:f32.4-vdot
                         #:f64.2-vsum
                         #:s8.32-insert
                         ))
  (check-package '#:sb-simd-avx2
                 :skip '(#:u32.8-from-f32.8
                         #:f64.2-vsum
                         #:f32.4-vsum
                         #:f32.4-hsum
                         #:f32.4-vdot
                         #:f32.8-vsum
                         #:f64.4-vsum
                         #:s16.16-mulhrs
                         #:s64.4-mul
                         )))
