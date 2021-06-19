(in-package #:sb-simd)

(defmacro define-comparison (name cmp and truth)
  (with-accessors ((result-records primitive-record-result-records)
                   (argument-records primitive-record-argument-records))
      (find-instruction-record cmp)
    (destructuring-bind ((result-record) (argument-record other-argument-record))
        (list result-records argument-records)
      (assert (eq argument-record other-argument-record))
      (let ((result-type (value-record-name result-record))
            (argument-type (value-record-name argument-record)))
        `(progn
           (defun ,name (arg &rest more-args)
             (if (null more-args)
                 (progn (,argument-type arg) (,result-type ,truth))
                 (let* ((a (,argument-type arg))
                        (b (,argument-type (first more-args)))
                        (result (,cmp a b)))
                   (declare (,argument-type a b)
                            (,result-type result))
                   (loop for elt in (rest more-args)
                         do (shiftf a b (,argument-type elt))
                         do (setf result (,and result (,cmp a b))))
                   result)))
           (define-compiler-macro ,name (arg &rest more-args)
             (if (null more-args)
                 `(progn (,',argument-type ,arg) ,',truth)
                 (let ((bindings
                         (loop for arg in (list* arg more-args)
                               collect
                               (list (gensym "ARG") (list ',argument-type arg)))))
                   `(let ,bindings
                      (,',and ,@(loop for ((a nil) (b nil) . rest) on bindings
                                      collect `(,',cmp ,a ,b)
                                      until (null rest))))))))))))

(in-package #:sb-simd-sse)

(sb-simd::define-comparison f32.4=  two-arg-f32.4=  sb-simd-sse2:u32.4-and +u32-true+)
(sb-simd::define-comparison f32.4<  two-arg-f32.4<  sb-simd-sse2:u32.4-and +u32-true+)
(sb-simd::define-comparison f32.4<= two-arg-f32.4<= sb-simd-sse2:u32.4-and +u32-true+)
(sb-simd::define-comparison f32.4>  two-arg-f32.4>  sb-simd-sse2:u32.4-and +u32-true+)
(sb-simd::define-comparison f32.4>= two-arg-f32.4>= sb-simd-sse2:u32.4-and +u32-true+)

(in-package #:sb-simd-sse2)

(sb-simd::define-comparison f64.2=  two-arg-f64.2=  u64.2-and +u64-true+)
(sb-simd::define-comparison f64.2<  two-arg-f64.2<  u64.2-and +u64-true+)
(sb-simd::define-comparison f64.2<= two-arg-f64.2<= u64.2-and +u64-true+)
(sb-simd::define-comparison f64.2>  two-arg-f64.2>  u64.2-and +u64-true+)
(sb-simd::define-comparison f64.2>= two-arg-f64.2>= u64.2-and +u64-true+)

(in-package #:sb-simd-sse4.1)

(sb-simd::define-comparison u64.2=  two-arg-u64.2=  u64.2-and +u32-true+)

(in-package #:sb-simd-sse4.2)

(sb-simd::define-comparison u64.2<  two-arg-u64.2<  u64.2-and +u32-true+)
(sb-simd::define-comparison u64.2<= two-arg-u64.2<= u64.2-and +u32-true+)
(sb-simd::define-comparison u64.2>  two-arg-u64.2>  u64.2-and +u32-true+)
(sb-simd::define-comparison u64.2>= two-arg-u64.2>= u64.2-and +u32-true+)

(in-package #:sb-simd-avx)

(sb-simd::define-comparison f32.4=  two-arg-f32.4=  u32.4-and +u32-true+)
(sb-simd::define-comparison f32.4<  two-arg-f32.4<  u32.4-and +u32-true+)
(sb-simd::define-comparison f32.4<= two-arg-f32.4<= u32.4-and +u32-true+)
(sb-simd::define-comparison f32.4>  two-arg-f32.4>  u32.4-and +u32-true+)
(sb-simd::define-comparison f32.4>= two-arg-f32.4>= u32.4-and +u32-true+)

(sb-simd::define-comparison f64.2=  two-arg-f64.2=  u64.2-and +u64-true+)
(sb-simd::define-comparison f64.2<  two-arg-f64.2<  u64.2-and +u64-true+)
(sb-simd::define-comparison f64.2<= two-arg-f64.2<= u64.2-and +u64-true+)
(sb-simd::define-comparison f64.2>  two-arg-f64.2>  u64.2-and +u64-true+)
(sb-simd::define-comparison f64.2>= two-arg-f64.2>= u64.2-and +u64-true+)

(sb-simd::define-comparison f32.8=  two-arg-f32.8=  u32.8-and +u32-true+)
(sb-simd::define-comparison f32.8<  two-arg-f32.8<  u32.8-and +u32-true+)
(sb-simd::define-comparison f32.8<= two-arg-f32.8<= u32.8-and +u32-true+)
(sb-simd::define-comparison f32.8>  two-arg-f32.8>  u32.8-and +u32-true+)
(sb-simd::define-comparison f32.8>= two-arg-f32.8>= u32.8-and +u32-true+)

(sb-simd::define-comparison f64.4=  two-arg-f64.4=  u64.4-and +u64-true+)
(sb-simd::define-comparison f64.4<  two-arg-f64.4<  u64.4-and +u64-true+)
(sb-simd::define-comparison f64.4<= two-arg-f64.4<= u64.4-and +u64-true+)
(sb-simd::define-comparison f64.4>  two-arg-f64.4>  u64.4-and +u64-true+)
(sb-simd::define-comparison f64.4>= two-arg-f64.4>= u64.4-and +u64-true+)

(sb-simd::define-comparison u8.16=  two-arg-u8.16=  u8.16-and +u8-true+)
(sb-simd::define-comparison u8.16<  two-arg-u8.16<  u8.16-and +u8-true+)
(sb-simd::define-comparison u8.16<= two-arg-u8.16<= u8.16-and +u8-true+)
(sb-simd::define-comparison u8.16>  two-arg-u8.16>  u8.16-and +u8-true+)
(sb-simd::define-comparison u8.16>= two-arg-u8.16>= u8.16-and +u8-true+)

(sb-simd::define-comparison u16.8=  two-arg-u16.8=  u16.8-and +u16-true+)
(sb-simd::define-comparison u16.8<  two-arg-u16.8<  u16.8-and +u16-true+)
(sb-simd::define-comparison u16.8<= two-arg-u16.8<= u16.8-and +u16-true+)
(sb-simd::define-comparison u16.8>  two-arg-u16.8>  u16.8-and +u16-true+)
(sb-simd::define-comparison u16.8>= two-arg-u16.8>= u16.8-and +u16-true+)

(sb-simd::define-comparison u32.4=  two-arg-u32.4=  u32.4-and +u32-true+)
(sb-simd::define-comparison u32.4<  two-arg-u32.4<  u32.4-and +u32-true+)
(sb-simd::define-comparison u32.4<= two-arg-u32.4<= u32.4-and +u32-true+)
(sb-simd::define-comparison u32.4>  two-arg-u32.4>  u32.4-and +u32-true+)
(sb-simd::define-comparison u32.4>= two-arg-u32.4>= u32.4-and +u32-true+)

(sb-simd::define-comparison u64.2=  two-arg-u64.2=  u64.2-and +u64-true+)
(sb-simd::define-comparison u64.2<  two-arg-u64.2<  u64.2-and +u64-true+)
(sb-simd::define-comparison u64.2<= two-arg-u64.2<= u64.2-and +u64-true+)
(sb-simd::define-comparison u64.2>  two-arg-u64.2>  u64.2-and +u64-true+)
(sb-simd::define-comparison u64.2>= two-arg-u64.2>= u64.2-and +u64-true+)

(sb-simd::define-comparison s8.16=  two-arg-s8.16=  s8.16-and +s8-true+)
(sb-simd::define-comparison s8.16<  two-arg-s8.16<  s8.16-and +s8-true+)
(sb-simd::define-comparison s8.16<= two-arg-s8.16<= s8.16-and +s8-true+)
(sb-simd::define-comparison s8.16>  two-arg-s8.16>  s8.16-and +s8-true+)
(sb-simd::define-comparison s8.16>= two-arg-s8.16>= s8.16-and +s8-true+)

(sb-simd::define-comparison s16.8=  two-arg-s16.8=  s16.8-and +s16-true+)
(sb-simd::define-comparison s16.8<  two-arg-s16.8<  s16.8-and +s16-true+)
(sb-simd::define-comparison s16.8<= two-arg-s16.8<= s16.8-and +s16-true+)
(sb-simd::define-comparison s16.8>  two-arg-s16.8>  s16.8-and +s16-true+)
(sb-simd::define-comparison s16.8>= two-arg-s16.8>= s16.8-and +s16-true+)

(sb-simd::define-comparison s32.4=  two-arg-s32.4=  s32.4-and +s32-true+)
(sb-simd::define-comparison s32.4<  two-arg-s32.4<  s32.4-and +s32-true+)
(sb-simd::define-comparison s32.4<= two-arg-s32.4<= s32.4-and +s32-true+)
(sb-simd::define-comparison s32.4>  two-arg-s32.4>  s32.4-and +s32-true+)
(sb-simd::define-comparison s32.4>= two-arg-s32.4>= s32.4-and +s32-true+)

(sb-simd::define-comparison s64.2=  two-arg-s64.2=  s64.2-and +s64-true+)
(sb-simd::define-comparison s64.2<  two-arg-s64.2<  s64.2-and +s64-true+)
(sb-simd::define-comparison s64.2<= two-arg-s64.2<= s64.2-and +s64-true+)
(sb-simd::define-comparison s64.2>  two-arg-s64.2>  s64.2-and +s64-true+)
(sb-simd::define-comparison s64.2>= two-arg-s64.2>= s64.2-and +s64-true+)

(in-package #:sb-simd-avx2)

(sb-simd::define-comparison u8.32=  two-arg-u8.32=  u8.32-and +u8-true+)
(sb-simd::define-comparison u8.32<  two-arg-u8.32<  u8.32-and +u8-true+)
(sb-simd::define-comparison u8.32<= two-arg-u8.32<= u8.32-and +u8-true+)
(sb-simd::define-comparison u8.32>  two-arg-u8.32>  u8.32-and +u8-true+)
(sb-simd::define-comparison u8.32>= two-arg-u8.32>= u8.32-and +u8-true+)

(sb-simd::define-comparison u16.16=  two-arg-u16.16=  u16.16-and +u16-true+)
(sb-simd::define-comparison u16.16<  two-arg-u16.16<  u16.16-and +u16-true+)
(sb-simd::define-comparison u16.16<= two-arg-u16.16<= u16.16-and +u16-true+)
(sb-simd::define-comparison u16.16>  two-arg-u16.16>  u16.16-and +u16-true+)
(sb-simd::define-comparison u16.16>= two-arg-u16.16>= u16.16-and +u16-true+)

(sb-simd::define-comparison u32.8=  two-arg-u32.8=  u32.8-and +u32-true+)
(sb-simd::define-comparison u32.8<  two-arg-u32.8<  u32.8-and +u32-true+)
(sb-simd::define-comparison u32.8<= two-arg-u32.8<= u32.8-and +u32-true+)
(sb-simd::define-comparison u32.8>  two-arg-u32.8>  u32.8-and +u32-true+)
(sb-simd::define-comparison u32.8>= two-arg-u32.8>= u32.8-and +u32-true+)

(sb-simd::define-comparison u64.4=  two-arg-u64.4=  u64.4-and +u64-true+)
(sb-simd::define-comparison u64.4<  two-arg-u64.4<  u64.4-and +u64-true+)
(sb-simd::define-comparison u64.4<= two-arg-u64.4<= u64.4-and +u64-true+)
(sb-simd::define-comparison u64.4>  two-arg-u64.4>  u64.4-and +u64-true+)
(sb-simd::define-comparison u64.4>= two-arg-u64.4>= u64.4-and +u64-true+)


(sb-simd::define-comparison s8.32=  two-arg-s8.32=  s8.32-and +s8-true+)
(sb-simd::define-comparison s8.32<  two-arg-s8.32<  s8.32-and +s8-true+)
(sb-simd::define-comparison s8.32<= two-arg-s8.32<= s8.32-and +s8-true+)
(sb-simd::define-comparison s8.32>  two-arg-s8.32>  s8.32-and +s8-true+)
(sb-simd::define-comparison s8.32>= two-arg-s8.32>= s8.32-and +s8-true+)

(sb-simd::define-comparison s16.16=  two-arg-s16.16=  s16.16-and +s16-true+)
(sb-simd::define-comparison s16.16<  two-arg-s16.16<  s16.16-and +s16-true+)
(sb-simd::define-comparison s16.16<= two-arg-s16.16<= s16.16-and +s16-true+)
(sb-simd::define-comparison s16.16>  two-arg-s16.16>  s16.16-and +s16-true+)
(sb-simd::define-comparison s16.16>= two-arg-s16.16>= s16.16-and +s16-true+)

(sb-simd::define-comparison s32.8=  two-arg-s32.8=  s32.8-and +s32-true+)
(sb-simd::define-comparison s32.8<  two-arg-s32.8<  s32.8-and +s32-true+)
(sb-simd::define-comparison s32.8<= two-arg-s32.8<= s32.8-and +s32-true+)
(sb-simd::define-comparison s32.8>  two-arg-s32.8>  s32.8-and +s32-true+)
(sb-simd::define-comparison s32.8>= two-arg-s32.8>= s32.8-and +s32-true+)

(sb-simd::define-comparison s64.4=  two-arg-s64.4=  s64.4-and +s64-true+)
(sb-simd::define-comparison s64.4<  two-arg-s64.4<  s64.4-and +s64-true+)
(sb-simd::define-comparison s64.4<= two-arg-s64.4<= s64.4-and +s64-true+)
(sb-simd::define-comparison s64.4>  two-arg-s64.4>  s64.4-and +s64-true+)
(sb-simd::define-comparison s64.4>= two-arg-s64.4>= s64.4-and +s64-true+)

