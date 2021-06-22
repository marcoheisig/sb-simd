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

(sb-simd::define-commutative u8.16-and two-arg-u8.16-and +u8-true+)
(sb-simd::define-commutative u8.16-or  two-arg-u8.16-or  +u8-false+)
(sb-simd::define-commutative u8.16-xor two-arg-u8.16-xor +u8-false+)
(sb-simd::define-commutative u8.16+    two-arg-u8.16+    0)

(sb-simd::define-commutative u16.8-and two-arg-u16.8-and +u16-true+)
(sb-simd::define-commutative u16.8-or  two-arg-u16.8-or  +u16-false+)
(sb-simd::define-commutative u16.8-xor two-arg-u16.8-xor +u16-false+)
(sb-simd::define-commutative u16.8+    two-arg-u16.8+    0)

(sb-simd::define-commutative u32.4-and two-arg-u32.4-and +u32-true+)
(sb-simd::define-commutative u32.4-or  two-arg-u32.4-or  +u32-false+)
(sb-simd::define-commutative u32.4-xor two-arg-u32.4-xor +u32-false+)
(sb-simd::define-commutative u32.4+    two-arg-u32.4+    0)

(sb-simd::define-commutative u64.2-and two-arg-u64.2-and +u64-true+)
(sb-simd::define-commutative u64.2-or  two-arg-u64.2-or  +u64-false+)
(sb-simd::define-commutative u64.2-xor two-arg-u64.2-xor +u64-false+)
(sb-simd::define-commutative u64.2+    two-arg-u64.2+    0)

(sb-simd::define-commutative s8.16-and two-arg-s8.16-and +s8-true+)
(sb-simd::define-commutative s8.16-or  two-arg-s8.16-or  +s8-false+)
(sb-simd::define-commutative s8.16-xor two-arg-s8.16-xor +s8-false+)
(sb-simd::define-commutative s8.16+    two-arg-s8.16+    0)

(sb-simd::define-commutative s16.8-and two-arg-s16.8-and +s16-true+)
(sb-simd::define-commutative s16.8-or  two-arg-s16.8-or  +s16-false+)
(sb-simd::define-commutative s16.8-xor two-arg-s16.8-xor +s16-false+)
(sb-simd::define-commutative s16.8+    two-arg-s16.8+    0)

(sb-simd::define-commutative s32.4-and two-arg-s32.4-and +s32-true+)
(sb-simd::define-commutative s32.4-or  two-arg-s32.4-or  +s32-false+)
(sb-simd::define-commutative s32.4-xor two-arg-s32.4-xor +s32-false+)
(sb-simd::define-commutative s32.4+    two-arg-s32.4+    0)

(sb-simd::define-commutative s64.2-and two-arg-s64.2-and +s64-true+)
(sb-simd::define-commutative s64.2-or  two-arg-s64.2-or  +s64-false+)
(sb-simd::define-commutative s64.2-xor two-arg-s64.2-xor +s64-false+)
(sb-simd::define-commutative s64.2+    two-arg-s64.2+    0)

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

(sb-simd::define-commutative u8.16-and two-arg-u8.16-and +u8-true+)
(sb-simd::define-commutative u8.16-or  two-arg-u8.16-or  +u8-false+)
(sb-simd::define-commutative u8.16-xor two-arg-u8.16-xor +u8-false+)
(sb-simd::define-commutative u8.16+    two-arg-u8.16+ 0)

(sb-simd::define-commutative u16.8-and two-arg-u16.8-and +u16-true+)
(sb-simd::define-commutative u16.8-or  two-arg-u16.8-or  +u16-false+)
(sb-simd::define-commutative u16.8-xor two-arg-u16.8-xor +u16-false+)
(sb-simd::define-commutative u16.8+    two-arg-u16.8+ 0)

(sb-simd::define-commutative u32.4-and two-arg-u32.4-and +u32-true+)
(sb-simd::define-commutative u32.4-or  two-arg-u32.4-or  +u32-false+)
(sb-simd::define-commutative u32.4-xor two-arg-u32.4-xor +u32-false+)
(sb-simd::define-commutative u32.4+    two-arg-u32.4+ 0)

(sb-simd::define-commutative u64.2-and two-arg-u64.2-and +u64-true+)
(sb-simd::define-commutative u64.2-or  two-arg-u64.2-or  +u64-false+)
(sb-simd::define-commutative u64.2-xor two-arg-u64.2-xor +u64-false+)
(sb-simd::define-commutative u64.2+    two-arg-u64.2+ 0)

(sb-simd::define-commutative s8.16-and two-arg-s8.16-and +s8-true+)
(sb-simd::define-commutative s8.16-or  two-arg-s8.16-or  +s8-false+)
(sb-simd::define-commutative s8.16-xor two-arg-s8.16-xor +s8-false+)
(sb-simd::define-commutative s8.16+    two-arg-s8.16+ 0)

(sb-simd::define-commutative s16.8-and two-arg-s16.8-and +s16-true+)
(sb-simd::define-commutative s16.8-or  two-arg-s16.8-or  +s16-false+)
(sb-simd::define-commutative s16.8-xor two-arg-s16.8-xor +s16-false+)
(sb-simd::define-commutative s16.8+    two-arg-s16.8+ 0)
(sb-simd::define-commutative s16.8-mullo two-arg-s16.8-mullo 0)

(sb-simd::define-commutative s32.4-and two-arg-s32.4-and +s32-true+)
(sb-simd::define-commutative s32.4-or  two-arg-s32.4-or  +s32-false+)
(sb-simd::define-commutative s32.4-xor two-arg-s32.4-xor) +s32-false+
(sb-simd::define-commutative s32.4+    two-arg-s32.4+ 0)
(sb-simd::define-commutative s32.4-mullo two-arg-s32.4-mullo 0)

(sb-simd::define-commutative s64.2-and two-arg-s64.2-and +s64-true+)
(sb-simd::define-commutative s64.2-or  two-arg-s64.2-or  +s64-false+)
(sb-simd::define-commutative s64.2-xor two-arg-s64.2-xor +s64-false+)
(sb-simd::define-commutative s64.2+    two-arg-s64.2+ 0)

(in-package #:sb-simd-avx2)

(sb-simd::define-commutative u8.32-and two-arg-u8.32-and +u8-true+)
(sb-simd::define-commutative u8.32-or  two-arg-u8.32-or  +u8-false+)
(sb-simd::define-commutative u8.32-xor two-arg-u8.32-xor +u8-false+)
(sb-simd::define-commutative u8.32+    two-arg-u8.32+ 0)

(sb-simd::define-commutative u16.16-and two-arg-u16.16-and +u16-true+)
(sb-simd::define-commutative u16.16-or  two-arg-u16.16-or  +u16-false+)
(sb-simd::define-commutative u16.16-xor two-arg-u16.16-xor +u16-false+)
(sb-simd::define-commutative u16.16+    two-arg-u16.16+ 0)
(sb-simd::define-commutative s16.16-mullo two-arg-s16.16-mullo 0)

(sb-simd::define-commutative u32.8-and two-arg-u32.8-and +u32-true+)
(sb-simd::define-commutative u32.8-or  two-arg-u32.8-or  +u32-false+)
(sb-simd::define-commutative u32.8-xor two-arg-u32.8-xor +u32-false+)
(sb-simd::define-commutative u32.8+    two-arg-u32.8+ 0)
(sb-simd::define-commutative s32.8-mullo  two-arg-s32.8-mullo 0)

(sb-simd::define-commutative u64.4-and two-arg-u64.4-and +u64-true+)
(sb-simd::define-commutative u64.4-or  two-arg-u64.4-or  +u64-false+)
(sb-simd::define-commutative u64.4-xor two-arg-u64.4-xor +u64-false+)
(sb-simd::define-commutative u64.4+    two-arg-u64.4+ 0)

(sb-simd::define-commutative s8.32-and two-arg-s8.32-and +s8-true+)
(sb-simd::define-commutative s8.32-or  two-arg-s8.32-or  +s8-false+)
(sb-simd::define-commutative s8.32-xor two-arg-s8.32-xor +s8-false+)
(sb-simd::define-commutative s8.32+    two-arg-s8.32+ 0)

(sb-simd::define-commutative s16.16-and two-arg-s16.16-and +s16-true+)
(sb-simd::define-commutative s16.16-or  two-arg-s16.16-or  +s16-false+)
(sb-simd::define-commutative s16.16-xor two-arg-s16.16-xor +s16-false+)
(sb-simd::define-commutative s16.16+    two-arg-s16.16+ 0)

(sb-simd::define-commutative s32.8-and two-arg-s32.8-and +s32-true+)
(sb-simd::define-commutative s32.8-or  two-arg-s32.8-or  +s32-false+)
(sb-simd::define-commutative s32.8-xor two-arg-s32.8-xor +s32-false+)
(sb-simd::define-commutative s32.8+    two-arg-s32.8+ 0)

(sb-simd::define-commutative s64.4-and two-arg-s64.4-and +s64-true+)
(sb-simd::define-commutative s64.4-or  two-arg-s64.4-or  +s64-false+)
(sb-simd::define-commutative s64.4-xor two-arg-s64.4-xor +s64-false+)
(sb-simd::define-commutative s64.4+    two-arg-s64.4+ 0)

(sb-simd::define-commutative s16.16-mullo two-arg-s16.16-mullo 0)
(sb-simd::define-commutative s32.8-mullo  two-arg-s32.8-mullo 0)

