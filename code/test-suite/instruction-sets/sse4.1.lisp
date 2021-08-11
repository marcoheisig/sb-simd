(in-package #:sb-simd-sse4.1)

(sb-simd-test-suite:define-simple-simd-test u64.2= (u64.2) (u64.2 &rest u64.2) u64=)
(sb-simd-test-suite:define-simple-simd-test u64.2/= (u64.2) (u64.2 &rest u64.2) u64/=)

(sb-simd-test-suite:define-simple-simd-test s64.2= (u64.2) (s64.2 &rest s64.2) s64=)
(sb-simd-test-suite:define-simple-simd-test s64.2/= (u64.2) (s64.2 &rest s64.2) s64/=)
