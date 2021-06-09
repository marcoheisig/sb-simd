(in-package #:sb-simd)

(defmacro define-commutative
    (name binary-operation &optional (identity-element nil identity-element-p))
  (with-accessors ((result-records primitive-record-result-records)
                   (argument-records primitive-record-argument-records)
                   (commutative primitive-record-commutative))
      (find-instruction-record binary-operation)
    (assert commutative)
    (destructuring-bind ((value-record) (arg1-record arg2-record))
        (list result-records argument-records)
      (assert (eq value-record arg1-record))
      (assert (eq value-record arg2-record))
      (let ((type (value-record-name value-record)))
        (if identity-element-p
            `(progn
               (defun ,name (&rest args)
                 (if (null args)
                     (,type ,identity-element)
                     (let ((result (,type (first args))))
                       (declare (,type result))
                       (loop for arg in (rest args)
                             do (setf result (,binary-operation result (,type arg))))
                       result)))
               (define-compiler-macro ,name (&rest args)
                 (let ((n (length args)))
                   (case n
                     (0 `(,',type ,,identity-element))
                     (1 `(,',type ,(first args)))
                     (otherwise
                      `(,',binary-operation
                        (,',name ,@(subseq args 0 (floor n 2)))
                        (,',name ,@(subseq args (floor n 2)))))))))
            `(progn
               (defun ,name (arg &rest more-args)
                 (let ((result (,type arg)))
                   (declare (,type result))
                   (loop for arg in more-args
                         do (setf result (,binary-operation result (,type arg))))
                   result))
               (define-compiler-macro ,name (&whole whole &rest args)
                 (let ((n (length args)))
                   (case n
                     (0 whole)
                     (1 `(,',type ,(first args)))
                     (otherwise
                      `(,',binary-operation
                        (,',name ,@(subseq args 0 (floor n 2)))
                        (,',name ,@(subseq args (floor n 2))))))))))))))

(in-package #:sb-simd-sse)

(sb-simd::define-commutative f32.4-and two-arg-f32.4-and +f32-true+)
(sb-simd::define-commutative f32.4-or  two-arg-f32.4-or  +f32-false+)
(sb-simd::define-commutative f32.4-xor two-arg-f32.4-xor +f32-false+)
(sb-simd::define-commutative f32.4-max two-arg-f32.4-max)
(sb-simd::define-commutative f32.4-min two-arg-f32.4-min)
(sb-simd::define-commutative f32.4+    two-arg-f32.4+ 0f0)
(sb-simd::define-commutative f32.4*    two-arg-f32.4* 1f0)

(in-package #:sb-simd-sse2)

(sb-simd::define-commutative f64.2-and two-arg-f64.2-and +f64-true+)
(sb-simd::define-commutative f64.2-or  two-arg-f64.2-or  +f64-false+)
(sb-simd::define-commutative f64.2-xor two-arg-f64.2-xor +f64-false+)
(sb-simd::define-commutative f64.2-max two-arg-f64.2-max)
(sb-simd::define-commutative f64.2-min two-arg-f64.2-min)
(sb-simd::define-commutative f64.2+    two-arg-f64.2+ 0d0)
(sb-simd::define-commutative f64.2*    two-arg-f64.2* 1d0)

(sb-simd::define-commutative u32.4-and two-arg-u32.4-and +u32-true+)
(sb-simd::define-commutative u32.4-or  two-arg-u32.4-or  +u32-false+)
(sb-simd::define-commutative u32.4-xor two-arg-u32.4-xor +u32-false+)
(sb-simd::define-commutative u32.4+    two-arg-u32.4+    0)

(sb-simd::define-commutative u64.2-and two-arg-u64.2-and +u64-true+)
(sb-simd::define-commutative u64.2-or  two-arg-u64.2-or  +u64-false+)
(sb-simd::define-commutative u64.2-xor two-arg-u64.2-xor +u64-false+)
(sb-simd::define-commutative u64.2+    two-arg-u64.2+    0)

(in-package #:sb-simd-avx)

(sb-simd::define-commutative f32.4-and two-arg-f32.4-and +f32-true+)
(sb-simd::define-commutative f32.4-or  two-arg-f32.4-or  +f32-false+)
(sb-simd::define-commutative f32.4-xor two-arg-f32.4-xor +f32-false+)
(sb-simd::define-commutative f32.4-max two-arg-f32.4-max)
(sb-simd::define-commutative f32.4-min two-arg-f32.4-min)
(sb-simd::define-commutative f32.4+    two-arg-f32.4+ 0f0)
(sb-simd::define-commutative f32.4*    two-arg-f32.4* 1f0)

(sb-simd::define-commutative f64.2-and two-arg-f64.2-and +f64-true+)
(sb-simd::define-commutative f64.2-or  two-arg-f64.2-or  +f64-false+)
(sb-simd::define-commutative f64.2-xor two-arg-f64.2-xor +f64-false+)
(sb-simd::define-commutative f64.2-max two-arg-f64.2-max)
(sb-simd::define-commutative f64.2-min two-arg-f64.2-min)
(sb-simd::define-commutative f64.2+    two-arg-f64.2+ 0d0)
(sb-simd::define-commutative f64.2*    two-arg-f64.2* 1d0)

(sb-simd::define-commutative f32.8-and two-arg-f32.8-and +f32-true+)
(sb-simd::define-commutative f32.8-or  two-arg-f32.8-or  +f32-false+)
(sb-simd::define-commutative f32.8-xor two-arg-f32.8-xor +f32-false+)
(sb-simd::define-commutative f32.8-max two-arg-f32.8-max)
(sb-simd::define-commutative f32.8-min two-arg-f32.8-min)
(sb-simd::define-commutative f32.8+    two-arg-f32.8+ 0f0)
(sb-simd::define-commutative f32.8*    two-arg-f32.8* 1f0)

(sb-simd::define-commutative f64.4-and two-arg-f64.4-and +f64-true+)
(sb-simd::define-commutative f64.4-or  two-arg-f64.4-or  +f64-false+)
(sb-simd::define-commutative f64.4-xor two-arg-f64.4-xor +f64-false+)
(sb-simd::define-commutative f64.4-max two-arg-f64.4-max)
(sb-simd::define-commutative f64.4-min two-arg-f64.4-min)
(sb-simd::define-commutative f64.4+    two-arg-f64.4+ 0d0)
(sb-simd::define-commutative f64.4*    two-arg-f64.4* 1d0)

(sb-simd::define-commutative u32.4-and two-arg-u32.4-and +u32-true+)
(sb-simd::define-commutative u32.4-or  two-arg-u32.4-or  +u32-false+)
(sb-simd::define-commutative u32.4-xor two-arg-u32.4-xor +u32-false+)

(sb-simd::define-commutative u64.2-and two-arg-u64.2-and +u64-true+)
(sb-simd::define-commutative u64.2-or  two-arg-u64.2-or  +u64-false+)
(sb-simd::define-commutative u64.2-xor two-arg-u64.2-xor +u64-false+)

(sb-simd::define-commutative u32.8-and two-arg-u32.8-and +u32-true+)
(sb-simd::define-commutative u32.8-or  two-arg-u32.8-or  +u32-false+)
(sb-simd::define-commutative u32.8-xor two-arg-u32.8-xor +u32-false+)

(sb-simd::define-commutative u64.4-and two-arg-u64.4-and +u64-true+)
(sb-simd::define-commutative u64.4-or  two-arg-u64.4-or  +u64-false+)
(sb-simd::define-commutative u64.4-xor two-arg-u64.4-xor +u64-false+)

(in-package #:sb-simd-avx2)

(sb-simd::define-commutative u32.4-max two-arg-u32.4-max)
(sb-simd::define-commutative u32.4-min two-arg-u32.4-min)
(sb-simd::define-commutative u32.4+    two-arg-u32.4+ 0)

(sb-simd::define-commutative u64.2+    two-arg-u64.2+ 0)

(sb-simd::define-commutative u32.8-max two-arg-u32.8-max)
(sb-simd::define-commutative u32.8-min two-arg-u32.8-min)
(sb-simd::define-commutative u32.8+    two-arg-u32.8+ 0)

(sb-simd::define-commutative u64.4+    two-arg-u64.4+ 0)
