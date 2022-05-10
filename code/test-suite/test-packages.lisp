(in-package #:sb-simd-test-suite)

(define-test packages)

(define-test packages
  ;; We export some symbols for which we have no attached definition right
  ;; now, but where we plan to export a definition in the future.  Those
  ;; symbols need to be :SKIPped explicitly to avoid test failure.
  (check-package '#:sb-simd-internals)
  (check-package '#:sb-simd)
  (check-package '#:sb-simd-x86-64)
  (check-package '#:sb-simd-sse)
  (check-package '#:sb-simd-sse2)
  (check-package '#:sb-simd-sse3)
  (check-package '#:sb-simd-ssse3)
  (check-package '#:sb-simd-sse4.1)
  (check-package '#:sb-simd-sse4.2)
  (check-package '#:sb-simd-avx)
  (check-package '#:sb-simd-avx2))
