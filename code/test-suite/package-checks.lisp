(in-package #:sb-simd-test-suite)

(define-test packages)

#+(or)
(define-test packages
  (check-package '#:sb-simd-internals)
  (check-package '#:sb-simd)
  (check-package '#:sb-simd-x86-64)
  (check-package '#:sb-simd-sse)
  (check-package '#:sb-simd-sse2)
  (check-package '#:sb-simd-sse3)
  (check-package '#:sb-simd-ssse3)
  (check-package '#:sb-simd-sse4.1 :skip '(#:f32.4-extract))
  (check-package '#:sb-simd-sse4.2 :skip '(#:f32.4-extract))
  (check-package '#:sb-simd-avx
                 :skip '(#:u32.8-from-f32.8 #:s8.32-insert))
  (check-package '#:sb-simd-avx2
                 :skip '(#:u32.8-from-f32.8 #:s16.16-mulhrs #:s64.4-mul)))
