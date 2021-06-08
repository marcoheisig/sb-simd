(in-package #:sb-simd)

(defmacro define-non-equality (name neq and truth)
  (with-accessors ((result-records primitive-record-result-records)
                   (argument-records primitive-record-argument-records))
      (find-instruction-record neq)
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
                 `(progn (,',argument-type ,arg) ,',truth)
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

(in-package #:sb-simd-sse)

(sb-simd::define-non-equality f32.4/= two-arg-f32.4/= sb-simd-sse2:u32.4-and (1- (expt 2 32)))

(in-package #:sb-simd-sse2)

(sb-simd::define-non-equality f64.2/= two-arg-f64.2/= u64.2-and (1- (expt 2 64)))

(in-package #:sb-simd-avx)

(sb-simd::define-non-equality f32.4/= two-arg-f32.4/= u32.4-and (1- (expt 2 32)))
(sb-simd::define-non-equality f64.2/= two-arg-f64.2/= u64.2-and (1- (expt 2 64)))
(sb-simd::define-non-equality f32.8/= two-arg-f32.8/= u32.8-and (1- (expt 2 32)))
(sb-simd::define-non-equality f64.4/= two-arg-f64.4/= u64.4-and (1- (expt 2 64)))
