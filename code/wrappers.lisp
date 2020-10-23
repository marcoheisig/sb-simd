(in-package #:sb-simd)

(defmacro define-nary-wrapper (name simd-type two-arg-fn neutral-element)
  (export name)
  (if (and (value-record-name-p simd-type)
           (instruction-record-name-p two-arg-fn))
      `(progn
         (defun ,name (&rest args)
           (let ((acc ,neutral-element))
             (declare (,simd-type acc))
             (loop for arg in args do
               (setf acc (,two-arg-fn acc (,simd-type arg))))
             acc))
         (define-compiler-macro ,name (&rest args)
           (cond ((null args) ',neutral-element)
                 ((null (cdr args)) `(,',simd-type ,(first args)))
                 (t (reduce
                     (lambda (a b)
                       `(,',two-arg-fn (,',simd-type ,a) (,',simd-type ,b)))
                     args)))))
      `(defun ,name (&rest args)
         (error "The function ~S is not available on this platform."
                ',name))))

;; 128 bit instructions
(define-nary-wrapper u64.2+ u64.2 u64.2-two-arg-+ (make-u64.2 0 0))
(define-nary-wrapper u64.2* u64.2 u64.2-two-arg-* (make-u64.2 1 1))
(define-nary-wrapper f64.2+ f64.2 f64.2-two-arg-+ (make-f64.2 0d0 0d0))
(define-nary-wrapper f64.2* f64.2 f64.2-two-arg-* (make-f64.2 1d0 1d0))
(define-nary-wrapper f32.4+ f32.4 f32.4-two-arg-+ (make-f32.4 0f0 0f0 0f0 0f0))
(define-nary-wrapper f32.4* f32.4 f32.4-two-arg-* (make-f32.4 1f0 1f0 1f0 1f0))
;; 256 bit instructions
(define-nary-wrapper u64.4+ u64.4 u64.4-two-arg-+ (make-u64.4 0 0 0 0))
(define-nary-wrapper u64.4* u64.4 u64.4-two-arg-* (make-u64.4 1 1 1 1))
(define-nary-wrapper f64.4+ f64.4 f64.4-two-arg-+ (make-f64.4 0d0 0d0 0d0 0d0))
(define-nary-wrapper f64.4* f64.4 f64.4-two-arg-* (make-f64.4 1d0 1d0 1d0 1d0))
(define-nary-wrapper f32.8+ f32.8 f32.8-two-arg-+ (make-f32.8 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0))
(define-nary-wrapper f32.8* f32.8 f32.8-two-arg-* (make-f32.8 1f0 1f0 1f0 1f0 1f0 1f0 1f0 1f0))

(defmacro define-nary-wrapper* (name simd-type two-arg-fn one-arg-fn)
  (export name)
  (if (and (value-record-name-p simd-type)
           (instruction-record-name-p two-arg-fn)
           (instruction-record-name-p one-arg-fn))
      `(progn
         (defun ,name (arg &rest more-args)
           (if (null more-args)
               (,one-arg-fn (,simd-type arg))
               (let ((acc (,simd-type arg)))
                 (declare (,simd-type acc))
                 (loop for arg in more-args do
                   (setf acc (,two-arg-fn acc (,simd-type arg))))
                 acc)))
         (define-compiler-macro ,name (arg &rest more-args)
           (cond ((null more-args)
                  `(,',one-arg-fn (,',simd-type ,(first args))))
                 (t (reduce
                     (lambda (a b) `(,',two-arg-fn a (,',simd-type ,b)))
                     args
                     :initial-value `(,',simd-type ,arg))))))
      `(defun ,name (arg &rest more-args)
         (error "The function ~S is not available on this platform."
                ',name))))

;; 128 bit instructions
(define-nary-wrapper* u64.2- u64.2 u64.2-two-arg-- u64.2-one-arg--)
(define-nary-wrapper* u64.2/ u64.2 u64.2-two-arg-/ u64.2-one-arg-/)
(define-nary-wrapper* f64.2- f64.2 f64.2-two-arg-- f64.2-one-arg--)
(define-nary-wrapper* f64.2/ f64.2 f64.2-two-arg-/ f64.2-one-arg-/)
(define-nary-wrapper* f32.4- f32.4 f32.4-two-arg-- f32.4-one-arg--)
(define-nary-wrapper* f32.4/ f32.4 f32.4-two-arg-/ f32.4-one-arg-/)
;; 256 bit instructions
(define-nary-wrapper* u64.4- u64.4 u64.4-two-arg-- u64.4-one-arg--)
(define-nary-wrapper* u64.4/ u64.4 u64.4-two-arg-/ u64.4-one-arg-/)
(define-nary-wrapper* f64.4- f64.4 f64.4-two-arg-- f64.4-one-arg--)
(define-nary-wrapper* f64.4/ f64.4 f64.4-two-arg-/ f64.4-one-arg-/)
(define-nary-wrapper* f32.8- f32.8 f32.8-two-arg-- f32.8-one-arg--)
(define-nary-wrapper* f32.8/ f32.8 f32.8-two-arg-/ f32.8-one-arg-/)
