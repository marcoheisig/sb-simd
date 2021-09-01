(in-package #:sb-simd-sse4.1)

(sb-simd-test-suite:define-simple-simd-test f32.4-if (f32.4) (u32.4 f32.4 f32.4) f32-if)
(sb-simd-test-suite:define-simple-simd-test f64.2-if (f64.2) (u64.2 f64.2 f64.2) f64-if)
(sb-simd-test-suite:define-simple-simd-test u8.16-if (u8.16) (u8.16 u8.16 u8.16) u8-if)
(sb-simd-test-suite:define-simple-simd-test u16.8-if (u16.8) (u16.8 u16.8 u16.8) u16-if)
(sb-simd-test-suite:define-simple-simd-test u32.4-if (u32.4) (u32.4 u32.4 u32.4) u32-if)
(sb-simd-test-suite:define-simple-simd-test u64.2-if (u64.2) (u64.2 u64.2 u64.2) u64-if)
(sb-simd-test-suite:define-simple-simd-test s8.16-if (s8.16) (u8.16 s8.16 s8.16) s8-if)
(sb-simd-test-suite:define-simple-simd-test s16.8-if (s16.8) (u16.8 s16.8 s16.8) s16-if)
(sb-simd-test-suite:define-simple-simd-test s32.4-if (s32.4) (u32.4 s32.4 s32.4) s32-if)
(sb-simd-test-suite:define-simple-simd-test s64.2-if (s64.2) (u64.2 s64.2 s64.2) s64-if)

(sb-simd-test-suite:define-simple-simd-test u64.2= (u64.2) (u64.2 &rest u64.2) u64=)
(sb-simd-test-suite:define-simple-simd-test u64.2/= (u64.2) (u64.2 &rest u64.2) u64/=)

(sb-simd-test-suite:define-simple-simd-test s64.2= (u64.2) (s64.2 &rest s64.2) s64=)
(sb-simd-test-suite:define-simple-simd-test s64.2/= (u64.2) (s64.2 &rest s64.2) s64/=)
