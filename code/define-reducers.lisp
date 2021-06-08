(in-package #:sb-simd)

(defmacro define-reducer (name binary-operation initial-element)
  (with-accessors ((result-records primitive-record-result-records)
                   (argument-records primitive-record-argument-records))
      (find-instruction-record binary-operation)
    (destructuring-bind ((value-record) (arg1-record arg2-record))
        (list result-records argument-records)
      (assert (eq value-record arg1-record))
      (assert (eq value-record arg2-record))
      (let ((type (value-record-name value-record)))
        `(progn
           (defun ,name (arg &rest more-args)
             (if (null more-args)
                 (,binary-operation (,type ,initial-element) (,type arg))
                 (let ((result (,type arg)))
                   (declare (,type result))
                   (loop for arg in more-args
                         do (setf result (,binary-operation result (,type arg))))
                   result)))
           (define-compiler-macro ,name (arg &rest more-args)
             (cond ((null more-args)
                    `(,',binary-operation ,',initial-element (,',type ,arg)))
                   (t (reduce
                       (lambda (a b) `(,',binary-operation (,',type ,a) (,',type ,b)))
                       more-args
                       :initial-value `(,',type ,arg))))))))))

(in-package #:sb-simd-sse)

(sb-simd::define-reducer f32.4- two-arg-f32.4- 0f0)
(sb-simd::define-reducer f32.4/ two-arg-f32.4/ 1f0)

(in-package #:sb-simd-sse2)

(sb-simd::define-reducer f64.2- two-arg-f64.2- 0d0)
(sb-simd::define-reducer f64.2/ two-arg-f64.2/ 1d0)

(sb-simd::define-reducer u32.4-andnot two-arg-u32.4-andnot (1- (expt 2 32)))
(sb-simd::define-reducer u32.4- two-arg-u32.4- 0)

(sb-simd::define-reducer u64.2-andnot two-arg-u64.2-andnot (1- (expt 2 64)))
(sb-simd::define-reducer u64.2- two-arg-u64.2- 0)

(in-package #:sb-simd-avx)

(sb-simd::define-reducer f32.4- two-arg-f32.4- 0f0)
(sb-simd::define-reducer f32.4/ two-arg-f32.4/ 1f0)

(sb-simd::define-reducer f64.2- two-arg-f64.2- 0d0)
(sb-simd::define-reducer f64.2/ two-arg-f64.2/ 1d0)

(sb-simd::define-reducer f32.8- two-arg-f32.8- 0f0)
(sb-simd::define-reducer f32.8/ two-arg-f32.8/ 1f0)

(sb-simd::define-reducer f64.4- two-arg-f64.4- 0d0)
(sb-simd::define-reducer f64.4/ two-arg-f64.4/ 1d0)

(sb-simd::define-reducer u32.4-andnot two-arg-u32.4-andnot (1- (expt 2 32)))

(sb-simd::define-reducer u64.2-andnot two-arg-u64.2-andnot (1- (expt 2 64)))

(sb-simd::define-reducer u32.8-andnot two-arg-u32.8-andnot (1- (expt 2 32)))

(sb-simd::define-reducer u64.4-andnot two-arg-u64.4-andnot (1- (expt 2 64)))

(in-package #:sb-simd-avx2)

(sb-simd::define-reducer u32.4- two-arg-u32.4- 0)

(sb-simd::define-reducer u64.2- two-arg-u64.2- 0)

(sb-simd::define-reducer u32.8- two-arg-u32.8- 0)

(sb-simd::define-reducer u64.4- two-arg-u64.4- 0)
