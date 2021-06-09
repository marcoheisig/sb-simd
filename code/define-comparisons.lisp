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

(in-package #:sb-simd-avx2)

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

