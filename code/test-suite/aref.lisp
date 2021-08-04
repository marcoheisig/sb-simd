(in-package #:sb-simd-test-suite)

(defmacro define-aref-test (aref element-type simd-width &optional (unpacker 'identity))
  (let ((value-symbols (prefixed-symbols "V" simd-width))
        (zero (coerce 0 element-type))
        (one (coerce 1 element-type)))
    `(define-test ,aref
       ;; Create an array of zeros and successively replace zeros with
       ;; ones.  After each replacement, check whether a load still
       ;; produces the expected result.
       (let ((array (make-array '(,simd-width)
                                 :element-type ',element-type
                                 :initial-element ,zero)))
         (multiple-value-bind ,value-symbols (,unpacker (,aref array 0))
           ,@(loop for value-symbol in value-symbols
                   collect `(is (= ,value-symbol ,zero))))
         (loop for index below ,simd-width do
           (setf (aref array index) ,one)
           (loop for number in (multiple-value-list (,unpacker (,aref array 0)))
                 for position from 0
                 do (if (<= position index)
                        (is (= number ,one))
                        (is (= number ,zero))))))
       ;; Create an array with twice as many elements as the width of the
       ;; SIMD data type, and whose lower half consists of all zeros and
       ;; whose upper half consists of all ones.  Check that all valid
       ;; loads from this array have the expected state.
       (let ((array (make-array '(,(* 2 simd-width))
                                :element-type ',element-type
                                :initial-contents
                                (append (make-list ,simd-width :initial-element ,zero)
                                        (make-list ,simd-width :initial-element ,one)))))
         (loop for index below ,simd-width do
           (multiple-value-bind ,value-symbols
               (,unpacker (,aref array index))
             ,@(loop for value-symbol in value-symbols
                     for position from 0
                     collect `(if (< (+ ,position index) ,simd-width)
                                  (is (= ,value-symbol ,zero))
                                  (is (= ,value-symbol ,one))))))))))

(define-aref-test sb-simd-common:u8-aref  (unsigned-byte 8) 1)
(define-aref-test sb-simd-common:u16-aref (unsigned-byte 16) 1)
(define-aref-test sb-simd-common:u32-aref (unsigned-byte 32) 1)
(define-aref-test sb-simd-common:u64-aref (unsigned-byte 64) 1)
(define-aref-test sb-simd-common:s8-aref  (signed-byte 8) 1)
(define-aref-test sb-simd-common:s16-aref (signed-byte 16) 1)
(define-aref-test sb-simd-common:s32-aref (signed-byte 32) 1)
(define-aref-test sb-simd-common:s64-aref (signed-byte 64) 1)
(define-aref-test sb-simd-common:f32-aref single-float 1)
(define-aref-test sb-simd-common:f64-aref double-float 1)

(define-aref-test sb-simd-sse:f32.4-aref single-float 4 sb-simd-sse:f32.4-values)

(define-aref-test sb-simd-sse2:f64.2-aref double-float 2 sb-simd-sse2:f64.2-values)
(define-aref-test sb-simd-sse2:u8.16-aref (unsigned-byte 8) 16 sb-simd-sse2:u8.16-values)
(define-aref-test sb-simd-sse2:u16.8-aref (unsigned-byte 16) 8 sb-simd-sse2:u16.8-values)
(define-aref-test sb-simd-sse2:u32.4-aref (unsigned-byte 32) 4 sb-simd-sse2:u32.4-values)
(define-aref-test sb-simd-sse2:u64.2-aref (unsigned-byte 64) 2 sb-simd-sse2:u64.2-values)
(define-aref-test sb-simd-sse2:s8.16-aref (signed-byte 8) 16 sb-simd-sse2:s8.16-values)
(define-aref-test sb-simd-sse2:s16.8-aref (signed-byte 16) 8 sb-simd-sse2:s16.8-values)
(define-aref-test sb-simd-sse2:s32.4-aref (signed-byte 32) 4 sb-simd-sse2:s32.4-values)
(define-aref-test sb-simd-sse2:s64.2-aref (signed-byte 64) 2 sb-simd-sse2:s64.2-values)

(define-aref-test sb-simd-avx:f32.4-aref single-float 4 sb-simd-avx:f32.4-values)
(define-aref-test sb-simd-avx:f32.8-aref single-float 8 sb-simd-avx:f32.8-values)
(define-aref-test sb-simd-avx:f64.2-aref double-float 2 sb-simd-avx:f64.2-values)
(define-aref-test sb-simd-avx:f64.4-aref double-float 4 sb-simd-avx:f64.4-values)
(define-aref-test sb-simd-avx:u8.16-aref (unsigned-byte 8) 16 sb-simd-avx:u8.16-values)
(define-aref-test sb-simd-avx:u16.8-aref (unsigned-byte 16) 8 sb-simd-avx:u16.8-values)
(define-aref-test sb-simd-avx:u32.4-aref (unsigned-byte 32) 4 sb-simd-avx:u32.4-values)
(define-aref-test sb-simd-avx:u64.2-aref (unsigned-byte 64) 2 sb-simd-avx:u64.2-values)
(define-aref-test sb-simd-avx:s8.16-aref (signed-byte 8) 16 sb-simd-avx:s8.16-values)
(define-aref-test sb-simd-avx:s16.8-aref (signed-byte 16) 8 sb-simd-avx:s16.8-values)
(define-aref-test sb-simd-avx:s32.4-aref (signed-byte 32) 4 sb-simd-avx:s32.4-values)
(define-aref-test sb-simd-avx:s64.2-aref (signed-byte 64) 2 sb-simd-avx:s64.2-values)
(define-aref-test sb-simd-avx:u8.32-aref (unsigned-byte 8) 32 sb-simd-avx:u8.32-values)
(define-aref-test sb-simd-avx:u16.16-aref (unsigned-byte 16) 16 sb-simd-avx:u16.16-values)
(define-aref-test sb-simd-avx:u32.8-aref (unsigned-byte 32) 8 sb-simd-avx:u32.8-values)
(define-aref-test sb-simd-avx:u64.4-aref (unsigned-byte 64) 4 sb-simd-avx:u64.4-values)
(define-aref-test sb-simd-avx:s8.32-aref (signed-byte 8) 32 sb-simd-avx:s8.32-values)
(define-aref-test sb-simd-avx:s16.16-aref (signed-byte 16) 16 sb-simd-avx:s16.16-values)
(define-aref-test sb-simd-avx:s32.8-aref (signed-byte 32) 8 sb-simd-avx:s32.8-values)
(define-aref-test sb-simd-avx:s64.4-aref (signed-byte 64) 4 sb-simd-avx:s64.4-values)
