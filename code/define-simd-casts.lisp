(in-package #:sb-simd)

(defmacro call-vop (instruction-record-name &rest arguments)
  (with-accessors ((instruction-set instruction-record-instruction-set)
                   (vop instruction-record-vop))
      (find-instruction-record instruction-record-name)
    (if (instruction-set-available-p instruction-set)
        `(,vop ,@arguments)
        `(progn
           (missing-instruction (load-time-value (find-instruction-record ',instruction-record-name)))
           (touch ,@arguments)))))

(defmacro define-simd-cast (simd-record-name constructor)
  (with-accessors ((name simd-record-name)
                   (size simd-record-size)
                   (scalar-record simd-record-scalar-record))
      (find-value-record simd-record-name)
    (let* ((package (symbol-package constructor))
           (cast (mksym package (symbol-name name)))
           (err (mksym package "CANNOT-CONVERT-TO-" name)))
      `(progn
         (define-notinline ,err (x)
           (error "Cannot convert ~S to ~S." x ',name))
         (define-inline ,cast (x)
           (typecase x
             (,name x)
             (real
              (let ((v (,(scalar-record-name scalar-record) x)))
                (call-vop ,constructor ,@(loop repeat size collect 'v))))
             (otherwise (,err x))))))))

(defmacro define-simd-cast! (cast! pack!-from-scalar &optional pack!-from-p128 pack!-from-p256)
  (let* ((package (symbol-package cast!))
         (name (symbol-name cast!))
         (pack (find-symbol (subseq name 0 (1- (length name))) package))
         (err (mksym package "CANNOT-REINTERPRET-AS-" name)))
    (flet ((argument-type (instruction)
             (value-record-name
              (first
               (primitive-record-argument-records
                (find-instruction-record instruction))))))
      `(progn
         (define-notinline ,err (x)
           (error "Cannot reinterpret ~S as ~S." x ',pack))
         (define-inline ,cast! (x)
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

(sb-simd::define-simd-cast f32.4 make-f32.4)
(sb-simd::define-simd-cast! f32.4! f32.4!-from-f32)

(in-package #:sb-simd-sse2)

(sb-simd::define-simd-cast f64.2 make-f64.2)
(sb-simd::define-simd-cast! f64.2! f64.2!-from-f64 f64.2!-from-p128)

(sb-simd::define-simd-cast u8.16 make-u8.16)
(sb-simd::define-simd-cast! u8.16! u8.16!-from-u8 u8.16!-from-p128)

(sb-simd::define-simd-cast u16.8 make-u16.8)
(sb-simd::define-simd-cast! u16.8! u16.8!-from-u16 u16.8!-from-p128)

(sb-simd::define-simd-cast u32.4 make-u32.4)
(sb-simd::define-simd-cast! u32.4! u32.4!-from-u32 u32.4!-from-p128)

(sb-simd::define-simd-cast u64.2 make-u64.2)
(sb-simd::define-simd-cast! u64.2! u64.2!-from-u64 u64.2!-from-p128)

(sb-simd::define-simd-cast s8.16 make-s8.16)
#+(or)
(sb-simd::define-simd-cast! s8.16! s8.16!-from-s8 s8.16!-from-p128)

(sb-simd::define-simd-cast s16.8 make-s16.8)
#+(or)
(sb-simd::define-simd-cast! s16.8! s16.8!-from-s16 s16.8!-from-p128)

(sb-simd::define-simd-cast s32.4 make-s32.4)
#+(or)
(sb-simd::define-simd-cast! s32.4! s32.4-from-s32 s32.4!-from-p128)

(sb-simd::define-simd-cast s64.2 make-s64.2)
#+(or)
(sb-simd::define-simd-cast! s64.2! s64.2!-from-s64 s64.2!-from-p128)

(in-package #:sb-simd-avx)

(define-inline p128 (x) (the p128 x))
(define-inline p256 (x) (the p256 x))

(sb-simd::define-simd-cast f32.4 make-f32.4)
(sb-simd::define-simd-cast! f32.4! f32.4!-from-f32)

(sb-simd::define-simd-cast f64.2 make-f64.2)
(sb-simd::define-simd-cast! f64.2! f64.2!-from-f64 f64.2!-from-p128)

(sb-simd::define-simd-cast f32.8 make-f32.8)
(sb-simd::define-simd-cast! f32.8! f32.8!-from-f32 f32.8!-from-p128 f32.8!-from-p256)

(sb-simd::define-simd-cast f64.4 make-f64.4)
(sb-simd::define-simd-cast! f64.4! f64.4!-from-f64 f64.4!-from-p128 f64.4!-from-p256)

(sb-simd::define-simd-cast u8.16 make-u8.16)
(sb-simd::define-simd-cast! u8.16! u8.16!-from-u8 u8.16!-from-p128 u8.16!-from-p256)

(sb-simd::define-simd-cast u16.8 make-u16.8)
(sb-simd::define-simd-cast! u16.8! u16.8!-from-u16 u16.8!-from-p128 u16.8!-from-p256)

(sb-simd::define-simd-cast u32.4 make-u32.4)
(sb-simd::define-simd-cast! u32.4! u32.4!-from-u32 u32.4!-from-p128 u32.4!-from-p256)

(sb-simd::define-simd-cast u64.2 make-u64.2)
(sb-simd::define-simd-cast! u64.2! u64.2!-from-u64 u64.2!-from-p128 u64.2!-from-p256)

(sb-simd::define-simd-cast u8.32 make-u8.32)
(sb-simd::define-simd-cast! u8.32! u8.32!-from-u8 u8.32!-from-p128 u8.32!-from-p256)

(sb-simd::define-simd-cast u16.16 make-u16.16)
(sb-simd::define-simd-cast! u16.16! u16.16!-from-u16 u16.16!-from-p128 u16.16!-from-p256)

(sb-simd::define-simd-cast u32.8 make-u32.8)
(sb-simd::define-simd-cast! u32.8! u32.8!-from-u32 u32.8!-from-p128 u32.8!-from-p256)

(sb-simd::define-simd-cast u64.4 make-u64.4)
(sb-simd::define-simd-cast! u64.4! u64.4!-from-u64 u64.4!-from-p128 u64.4!-from-p256)

(sb-simd::define-simd-cast s8.16 make-s8.16)
#+(or)
(sb-simd::define-simd-cast! s8.16! s8.16!-from-s8 s8.16!-from-p128 s8.16!-from-p256)

(sb-simd::define-simd-cast s16.8 make-s16.8)
#+(or)
(sb-simd::define-simd-cast! s16.8! s16.8!-from-s16 s16.8!-from-p128 s16.8!-from-p256)

(sb-simd::define-simd-cast s32.4 make-s32.4)
#+(or)
(sb-simd::define-simd-cast! s32.4! s32.4-from-s32 s32.4!-from-p128 s32.4!-from-p256)

(sb-simd::define-simd-cast s64.2 make-s64.2)
#+(or)
(sb-simd::define-simd-cast! s64.2! s64.2!-from-s64 s64.2!-from-p128 s64.2!-from-p256)

(sb-simd::define-simd-cast s8.32 make-s8.32)
#+(or)
(sb-simd::define-simd-cast! s8.32! s8.32!-from-s8 s8.32!-from-p128 s8.32!-from-p256)

(sb-simd::define-simd-cast s16.16 make-s16.16)
#+(or)
(sb-simd::define-simd-cast! s16.16! s16.16!-from-s16 s16.16!-from-p128 s16.16!-from-p256)

(sb-simd::define-simd-cast s32.8 make-s32.8)
#+(or)
(sb-simd::define-simd-cast! s32.8! s32.8-from-s32 s32.8!-from-p128 s32.8!-from-p256)

(sb-simd::define-simd-cast s64.4 make-s64.4)
#+(or)
(sb-simd::define-simd-cast! s64.4! s64.4!-from-s64 s64.4!-from-p128 s64.4!-from-p256)

(in-package #:sb-simd-avx2)

(sb-simd::define-simd-cast u8.32 make-u8.32)

(sb-simd::define-simd-cast u16.16 make-u16.16)

(sb-simd::define-simd-cast u32.8 make-u32.8)

(sb-simd::define-simd-cast u64.4 make-u64.4)

(sb-simd::define-simd-cast s8.32 make-s8.32)

(sb-simd::define-simd-cast s16.16 make-s16.16)

(sb-simd::define-simd-cast s32.8 make-s32.8)

(sb-simd::define-simd-cast s64.4 make-s64.4)
