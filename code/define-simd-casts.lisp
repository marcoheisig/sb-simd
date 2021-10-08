(in-package #:sb-simd-internals)

;;; For each SIMD data type X.Y, define two functions:
;;;
;;; 1. A function named X.Y that ensures that an object is either of the
;;;    type X.Y, or a scalar that can be broadcast to the type X.Y.
;;;
;;; 2. A function named X.Y! that reinterprets the bits of another SIMD
;;;    pack or suitable scalar as an X.Y.  If the supplied argument has
;;;    more bits than the target data type, the excess bits are discarded.
;;;    If the supplied argument has less bits than the target data types,
;;;    the remaining bits are set to zero.

(defmacro call-vop (instruction-record-name &rest arguments)
  (with-accessors ((instruction-set instruction-record-instruction-set)
                   (vop instruction-record-vop))
      (find-function-record instruction-record-name)
    (if (instruction-set-available-p instruction-set)
        `(,vop ,@arguments)
        `(progn
           (missing-instruction (load-time-value (find-function-record ',instruction-record-name)))
           (touch ,@arguments)))))

(defmacro define-simd-cast (simd-record-name broadcast)
  (with-accessors ((name simd-record-name)
                   (scalar-record simd-record-scalar-record))
      (find-value-record simd-record-name)
    (let* ((package (symbol-package broadcast))
           (cast (mksym package (symbol-name name)))
           (err (mksym package "CANNOT-CONVERT-TO-" name))
           (instruction-set (find-instruction-set package)))
      `(progn
         (define-notinline ,err (x)
           (error "Cannot convert ~S to ~S." x ',name))
         (define-inline ,cast (x)
           (declare (sb-vm::instruction-sets ,@(included-instruction-sets instruction-set)))
           (typecase x
             (,name x)
             (real (call-vop ,broadcast (,(value-record-name scalar-record) x)))
             (otherwise (,err x))))))))

(defmacro define-simd-cast! (cast! pack!-from-scalar &optional pack!-from-p128 pack!-from-p256)
  (let* ((package (symbol-package cast!))
         (name (symbol-name cast!))
         (pack (find-symbol (subseq name 0 (1- (length name))) package))
         (err (mksym package "CANNOT-REINTERPRET-AS-" name))
         (instruction-set (find-instruction-set package)))
    (flet ((argument-type (instruction)
             (value-record-name
              (first
               (instruction-record-argument-records
                (find-function-record instruction))))))
      `(progn
         (define-notinline ,err (x)
           (error "Cannot reinterpret ~S as ~S." x ',pack))
         (define-inline ,cast! (x)
           (declare (sb-vm::instruction-sets ,@(included-instruction-sets instruction-set)))
           (typecase x
             (real (call-vop ,pack!-from-scalar (,(argument-type pack!-from-scalar) x)))
             ,@(unless (not pack!-from-p128)
                 `((,(argument-type pack!-from-p128)
                    (call-vop ,pack!-from-p128 x))))
             ,@(unless (not pack!-from-p256)
                 `((,(argument-type pack!-from-p256)
                    (call-vop ,pack!-from-p256 x))))
             (otherwise (,err x))))))))

(in-package #:sb-simd-sse)

(define-inline p128 (x) (the p128 x))

(define-simd-cast f32.4 f32.4-broadcast)
(define-simd-cast! f32.4! f32.4!-from-f32)

(in-package #:sb-simd-sse2)

(define-simd-cast f64.2 f64.2-broadcast)
(define-simd-cast! f64.2! f64.2!-from-f64 f64.2!-from-p128)

(define-simd-cast u8.16 u8.16-broadcast)
(define-simd-cast! u8.16! u8.16!-from-u8 u8.16!-from-p128)

(define-simd-cast u16.8 u16.8-broadcast)
(define-simd-cast! u16.8! u16.8!-from-u16 u16.8!-from-p128)

(define-simd-cast u32.4 u32.4-broadcast)
(define-simd-cast! u32.4! u32.4!-from-u32 u32.4!-from-p128)

(define-simd-cast u64.2 u64.2-broadcast)
(define-simd-cast! u64.2! u64.2!-from-u64 u64.2!-from-p128)

(define-simd-cast s8.16 s8.16-broadcast)
(define-simd-cast! s8.16! s8.16!-from-s8 s8.16!-from-p128)

(define-simd-cast s16.8 s16.8-broadcast)
(define-simd-cast! s16.8! s16.8!-from-s16 s16.8!-from-p128)

(define-simd-cast s32.4 s32.4-broadcast)
(define-simd-cast! s32.4! s32.4!-from-s32 s32.4!-from-p128)

(define-simd-cast s64.2 s64.2-broadcast)
(define-simd-cast! s64.2! s64.2!-from-s64 s64.2!-from-p128)

(in-package #:sb-simd-avx)

(define-inline p128 (x) (the p128 x))
(define-inline p256 (x) (the p256 x))

(define-simd-cast f32.4 f32.4-broadcast)
(define-simd-cast! f32.4! f32.4!-from-f32 f32.4!-from-p256)

(define-simd-cast f64.2 f64.2-broadcast)
(define-simd-cast! f64.2! f64.2!-from-f64 f64.2!-from-p128)

(define-simd-cast f32.8 f32.8-broadcast)
(define-simd-cast! f32.8! f32.8!-from-f32 f32.8!-from-p128 f32.8!-from-p256)

(define-simd-cast f64.4 f64.4-broadcast)
(define-simd-cast! f64.4! f64.4!-from-f64 f64.4!-from-p128 f64.4!-from-p256)

(define-simd-cast u8.16 u8.16-broadcast)
(define-simd-cast! u8.16! u8.16!-from-u8 u8.16!-from-p128 u8.16!-from-p256)

(define-simd-cast u16.8 u16.8-broadcast)
(define-simd-cast! u16.8! u16.8!-from-u16 u16.8!-from-p128 u16.8!-from-p256)

(define-simd-cast u32.4 u32.4-broadcast)
(define-simd-cast! u32.4! u32.4!-from-u32 u32.4!-from-p128 u32.4!-from-p256)

(define-simd-cast u64.2 u64.2-broadcast)
(define-simd-cast! u64.2! u64.2!-from-u64 u64.2!-from-p128 u64.2!-from-p256)

(define-simd-cast u8.32 u8.32-broadcast)
(define-simd-cast! u8.32! u8.32!-from-u8 u8.32!-from-p128 u8.32!-from-p256)

(define-simd-cast u16.16 u16.16-broadcast)
(define-simd-cast! u16.16! u16.16!-from-u16 u16.16!-from-p128 u16.16!-from-p256)

(define-simd-cast u32.8 u32.8-broadcast)
(define-simd-cast! u32.8! u32.8!-from-u32 u32.8!-from-p128 u32.8!-from-p256)

(define-simd-cast u64.4 u64.4-broadcast)
(define-simd-cast! u64.4! u64.4!-from-u64 u64.4!-from-p128 u64.4!-from-p256)

(define-simd-cast s8.16 s8.16-broadcast)
(define-simd-cast! s8.16! s8.16!-from-s8 s8.16!-from-p128 s8.16!-from-p256)

(define-simd-cast s16.8 s16.8-broadcast)
(define-simd-cast! s16.8! s16.8!-from-s16 s16.8!-from-p128 s16.8!-from-p256)

(define-simd-cast s32.4 s32.4-broadcast)
(define-simd-cast! s32.4! s32.4!-from-s32 s32.4!-from-p128 s32.4!-from-p256)

(define-simd-cast s64.2 s64.2-broadcast)
(define-simd-cast! s64.2! s64.2!-from-s64 s64.2!-from-p128 s64.2!-from-p256)

(define-simd-cast s8.32 s8.32-broadcast)
(define-simd-cast! s8.32! s8.32!-from-s8 s8.32!-from-p128 s8.32!-from-p256)

(define-simd-cast s16.16 s16.16-broadcast)
(define-simd-cast! s16.16! s16.16!-from-s16 s16.16!-from-p128 s16.16!-from-p256)

(define-simd-cast s32.8 s32.8-broadcast)
(define-simd-cast! s32.8! s32.8!-from-s32 s32.8!-from-p128 s32.8!-from-p256)

(define-simd-cast s64.4 s64.4-broadcast)
(define-simd-cast! s64.4! s64.4!-from-s64 s64.4!-from-p128 s64.4!-from-p256)

(in-package #:sb-simd-avx2)

(define-simd-cast! u8.16! sb-simd-avx::u8.16!-from-u8  sb-simd-avx::u8.16!-from-p128 u8.16!-from-p256)

(define-simd-cast! u16.8! sb-simd-avx::u16.8!-from-u16 sb-simd-avx::u16.8!-from-p128 u16.8!-from-p256)

(define-simd-cast! u32.4! sb-simd-avx::u32.4!-from-u32 sb-simd-avx::u32.4!-from-p128 u32.4!-from-p256)

(define-simd-cast! u64.2! sb-simd-avx::u64.2!-from-u64 sb-simd-avx::u64.2!-from-p128 u64.2!-from-p256)

(define-simd-cast! s8.16! sb-simd-avx::s8.16!-from-s8  sb-simd-avx::s8.16!-from-p128 s8.16!-from-p256)

(define-simd-cast! s16.8! sb-simd-avx::s16.8!-from-s16 sb-simd-avx::s16.8!-from-p128 s16.8!-from-p256)

(define-simd-cast! s32.4! sb-simd-avx::s32.4!-from-s32 sb-simd-avx::s32.4!-from-p128 s32.4!-from-p256)

(define-simd-cast! s64.2! sb-simd-avx::s64.2!-from-s64 sb-simd-avx::s64.2!-from-p128 s64.2!-from-p256)

(define-simd-cast u8.32 u8.32-broadcast)

(define-simd-cast u16.16 u16.16-broadcast)

(define-simd-cast u32.8 u32.8-broadcast)

(define-simd-cast u64.4 u64.4-broadcast)

(define-simd-cast s8.32 s8.32-broadcast)

(define-simd-cast s16.16 s16.16-broadcast)

(define-simd-cast s32.8 s32.8-broadcast)

(define-simd-cast s64.4 s64.4-broadcast)
