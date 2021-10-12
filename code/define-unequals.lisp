(in-package #:sb-simd-internals)

(defmacro define-unequal (name neq and truth)
  (with-accessors ((result-records instruction-record-result-records)
                   (argument-records instruction-record-argument-records))
      (find-function-record neq)
    (destructuring-bind ((result-record) (argument-record other-argument-record))
        (list result-records argument-records)
      (assert (eq argument-record other-argument-record))
      (let ((result-type (value-record-name result-record))
            (argument-type (value-record-name argument-record)))
        `(progn
           (defun ,name (arg &rest more-args)
             (let ((args (list* (,argument-type arg) (mapcar #',argument-type more-args)))
                   (result (,result-type ,truth)))
               (declare (,result-type result))
               (loop for (a . rest) on args do
                 (loop for b in rest do
                   (setf result (,and result (,neq a b)))))
               result))
           (define-compiler-macro ,name (arg &rest more-args)
             (if (null more-args)
                 `(progn (,',argument-type ,arg) (,',result-type ,',truth))
                 (let ((bindings
                         (loop for arg in (list* arg more-args)
                               collect
                               (list (gensym "ARG") (list ',argument-type arg)))))
                   `(let ,bindings
                      (,',and
                       ,@(loop for ((a nil) . rest) on bindings
                               append
                               (loop for (b nil) in rest
                                     collect `(,',neq ,a ,b)))))))))))))

(in-package #:sb-simd)

(define-unequal f32/= two-arg-f32/= u32-and +u32-true+)
(define-unequal f64/= two-arg-f64/= u64-and +u64-true+)

(define-unequal u8/= two-arg-u8/= u8-and +u8-true+)
(define-unequal u16/= two-arg-u16/= u16-and +u16-true+)
(define-unequal u32/= two-arg-u32/= u32-and +u32-true+)
(define-unequal u64/= two-arg-u64/= u64-and +u64-true+)

(define-unequal s8/= two-arg-s8/= u8-and +u8-true+)
(define-unequal s16/= two-arg-s16/= u16-and +u16-true+)
(define-unequal s32/= two-arg-s32/= u32-and +u32-true+)
(define-unequal s64/= two-arg-s64/= u64-and +u64-true+)

(in-package #:sb-simd-sse)

(define-unequal f32/= two-arg-f32/= u32-and +u32-true+)

(in-package #:sb-simd-sse2)

(define-unequal f64/= two-arg-f64/= u64-and +u64-true+)

(define-unequal f32.4/= two-arg-f32.4/= sb-simd-sse2:u32.4-and +u32-true+)
(define-unequal f64.2/= two-arg-f64.2/= u64.2-and +u64-true+)

(define-unequal u8.16/= two-arg-u8.16/= u8.16-and +u8-true+)
(define-unequal u16.8/= two-arg-u16.8/= u16.8-and +u16-true+)
(define-unequal u32.4/= two-arg-u32.4/= u32.4-and +u32-true+)

(define-unequal s8.16/= two-arg-s8.16/= u8.16-and +u8-true+)
(define-unequal s16.8/= two-arg-s16.8/= u16.8-and +u16-true+)
(define-unequal s32.4/= two-arg-s32.4/= u32.4-and +u32-true+)

(in-package #:sb-simd-sse4.1)

(define-unequal u64.2/= two-arg-u64.2/= u64.2-and +u64-true+)
(define-unequal s64.2/= two-arg-s64.2/= u64.2-and +u64-true+)

(in-package #:sb-simd-avx)

(define-unequal f32/= two-arg-f32/= u32-and +u32-true+)
(define-unequal f64/= two-arg-f64/= u64-and +u64-true+)

(define-unequal f32.4/= two-arg-f32.4/= u32.4-and +u32-true+)
(define-unequal f64.2/= two-arg-f64.2/= u64.2-and +u64-true+)
(define-unequal f32.8/= two-arg-f32.8/= sb-simd-avx2:u32.8-and +u32-true+)
(define-unequal f64.4/= two-arg-f64.4/= sb-simd-avx2:u64.4-and +u64-true+)

(define-unequal u8.16/= two-arg-u8.16/= u8.16-and +u8-true+)
(define-unequal u16.8/= two-arg-u16.8/= u16.8-and +u16-true+)
(define-unequal u32.4/= two-arg-u32.4/= u32.4-and +u32-true+)
(define-unequal u64.2/= two-arg-u64.2/= u64.2-and +u64-true+)

(define-unequal s8.16/= two-arg-s8.16/= u8.16-and +u8-true+)
(define-unequal s16.8/= two-arg-s16.8/= u16.8-and +u16-true+)
(define-unequal s32.4/= two-arg-s32.4/= u32.4-and +u32-true+)
(define-unequal s64.2/= two-arg-s64.2/= u64.2-and +u64-true+)

(in-package #:sb-simd-avx2)

(define-unequal u8.32/=  two-arg-u8.32/=  u8.32-and  +u8-true+)
(define-unequal u16.16/= two-arg-u16.16/= u16.16-and +u16-true+)
(define-unequal u32.8/=  two-arg-u32.8/=  u32.8-and  +u32-true+)
(define-unequal u64.4/=  two-arg-u64.4/=  u64.4-and  +u64-true+)

(define-unequal s8.32/=  two-arg-s8.32/=  u8.32-and  +u8-true+)
(define-unequal s16.16/= two-arg-s16.16/= u16.16-and +u16-true+)
(define-unequal s32.8/=  two-arg-s32.8/=  u32.8-and  +u32-true+)
(define-unequal s64.4/=  two-arg-s64.4/=  u64.4-and  +u64-true+)
