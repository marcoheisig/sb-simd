(in-package #:sb-simd)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Commutative Monoids

(defmacro define-commutative-monoid-op
    (name binary-operation identity-element)
  (let* ((instruction-record (find-instruction-record binary-operation))
         (result-record (first (instruction-record-result-records instruction-record)))
         (type (value-record-name result-record)))
    (assert (instruction-record-commutative instruction-record))
    (assert (every (lambda (argument-record) (eq argument-record result-record))
                   (instruction-record-argument-records instruction-record)))
    (when (instruction-record-supported-p instruction-record)
      `(progn
         (export ',name)
         (defun ,name (&rest args)
           (if (null args)
               (,type ,identity-element)
               (let ((result (,type (first args))))
                 (declare (,type result))
                 (loop for arg in (rest args)
                       do (setf result (,binary-operation result (,type arg))))
                 result)))
         ;; Other than the default implementation, the compiler macro doesn't
         ;; combine arguments from left to right, but in a hierarchical
         ;; fashion.
         (define-compiler-macro ,name (&rest args)
           (let ((n (length args)))
             (case n
               (0 `(,',type ,,identity-element))
               (1 `(,',type ,(first args)))
               (otherwise
                `(,',binary-operation
                    (,',name ,@(subseq args 0 (floor n 2)))
                    (,',name ,@(subseq args (floor n 2))))))))))))

;; f32.4
(define-commutative-monoid-op f32.4+    two-arg-f32.4+    0f0)
(define-commutative-monoid-op f32.4*    two-arg-f32.4*    1f0)
;; f64.2
(define-commutative-monoid-op f64.2+    two-arg-f64.2+    0d0)
(define-commutative-monoid-op f64.2*    two-arg-f64.2*    1d0)
;; f32.8
(define-commutative-monoid-op f32.8+    two-arg-f32.8+    0f0)
(define-commutative-monoid-op f32.8*    two-arg-f32.8*    1f0)
;; f64.4
(define-commutative-monoid-op f64.4+    two-arg-f64.4+    0d0)
(define-commutative-monoid-op f64.4*    two-arg-f64.4*    1d0)
;; u32.4
(define-commutative-monoid-op u32.4-and two-arg-u32.4-and (1- (expt 2 32)))
(define-commutative-monoid-op u32.4-or  two-arg-u32.4-or  0)
(define-commutative-monoid-op u32.4-xor two-arg-u32.4-xor 0)
(define-commutative-monoid-op u32.4+    two-arg-u32.4+    0)
(define-commutative-monoid-op u32.4*    two-arg-u32.4*    1)
;; u64.2
(define-commutative-monoid-op u64.2-and two-arg-u64.2-and (1- (expt 2 64)))
(define-commutative-monoid-op u64.2-or  two-arg-u64.2-or  0)
(define-commutative-monoid-op u64.2-xor two-arg-u64.2-xor 0)
(define-commutative-monoid-op u64.2+    two-arg-u64.2+    0)
;(define-commutative-monoid-op u64.2*    two-arg-u64.2*    1)
;; u32.8
(define-commutative-monoid-op u32.8-and two-arg-u32.8-and (1- (expt 2 32)))
(define-commutative-monoid-op u32.8-or  two-arg-u32.8-or  0)
(define-commutative-monoid-op u32.8-xor two-arg-u32.8-xor 0)
(define-commutative-monoid-op u32.8+    two-arg-u32.8+    0)
(define-commutative-monoid-op u32.8*    two-arg-u32.8*    1)
;; u16.8
(define-commutative-monoid-op u16.8+    two-arg-u16.8+    0)
;; u8.16
(define-commutative-monoid-op u8.16+    two-arg-u8.16+    0)
;; u64.4
(define-commutative-monoid-op u64.4-and two-arg-u64.4-and (1- (expt 2 64)))
(define-commutative-monoid-op u64.4-or  two-arg-u64.4-or  0)
(define-commutative-monoid-op u64.4-xor two-arg-u64.4-xor 0)
(define-commutative-monoid-op u64.4+    two-arg-u64.4+    0)
(define-commutative-monoid-op u64.4*    two-arg-u64.4*    1)
;; s16.8
(define-commutative-monoid-op s16.8+    two-arg-s16.8+    0)
(define-commutative-monoid-op s16.8*    two-arg-s16.8*    1)
;; s32.4
(define-commutative-monoid-op s32.4+    two-arg-s32.4+    0)
(define-commutative-monoid-op s32.4*    two-arg-s32.4*    1)
;; s16.16
(define-commutative-monoid-op s16.16+   two-arg-s16.16+   0)
(define-commutative-monoid-op s16.16*   two-arg-s16.16*   1)
;; s64.2
(define-commutative-monoid-op s64.2+    two-arg-s64.2+    0)
;; s8.16
(define-commutative-monoid-op s8.16+    two-arg-s8.16+    0)
(define-commutative-monoid-op s8.16*    two-arg-s8.16*    1)
;; s8.32
;(define-commutative-monoid-op s8.32+    two-arg-s8.32+    0)
;(define-commutative-monoid-op s8.32*    two-arg-s8.32*    1)
;; s16.16
(define-commutative-monoid-op s16.16+   two-arg-s16.16+   0)
(define-commutative-monoid-op s16.16*   two-arg-s16.16*   1)
;; s32.8
(define-commutative-monoid-op s32.8+    two-arg-s32.8+    0)
(define-commutative-monoid-op s32.8*    two-arg-s32.8*    0)
;; s64.4
(define-commutative-monoid-op s64.4+    two-arg-s64.4+    0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reducers

(defmacro define-reducer (name binary-operation initial-element)
  (let* ((instruction-record (find-instruction-record binary-operation))
         (result-record (first (instruction-record-result-records instruction-record)))
         (type (value-record-name result-record)))
    (assert (every (lambda (argument-record) (eq argument-record result-record))
                   (instruction-record-argument-records instruction-record)))
    (when (instruction-record-supported-p instruction-record)
      `(progn
         (export ',name)
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
                     :initial-value `(,',type ,arg)))))))))

;; f32.4
(define-reducer f32.4- two-arg-f32.4- 0f0)
(define-reducer f32.4/ two-arg-f32.4/ 1f0)
;; f64.2
(define-reducer f64.2- two-arg-f64.2- 0d0)
(define-reducer f64.2/ two-arg-f64.2/ 1d0)
;; f32.8
(define-reducer f32.8- two-arg-f32.8- 0f0)
(define-reducer f32.8/ two-arg-f32.8/ 1f0)
;; f64.4
(define-reducer f64.4- two-arg-f64.4- 0d0)
(define-reducer f64.4/ two-arg-f64.4/ 1d0)
;; u8.16
(define-reducer u8.16- two-arg-u8.16- 0)
;; u16.8
(define-reducer u16.8- two-arg-u16.8- 0)
;; u32.4
(define-reducer u32.4- two-arg-u32.4- 0)
;; u64.2
(define-reducer u64.2- two-arg-u64.2- 0)
;; u32.8
(define-reducer u32.8- two-arg-u32.8- 0)
;; u64.4
(define-reducer u64.4- two-arg-u64.4- 0)
;; s32.4
(define-reducer s32.4- two-arg-s32.4- 0)
;; s64.2
(define-reducer s64.2- two-arg-s64.2- 0)
;; s8.16
(define-reducer s8.16- two-arg-s8.16- 0)
;; s8.32
;(define-reducer s8.32- two-arg-s8.32- 0)
;; s16.16
(define-reducer s16.16- two-arg-s16.16- 0)
;; s32.8
(define-reducer s32.8- two-arg-s32.8- 0)
;; s64.4
(define-reducer s64.4- two-arg-s64.4- 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Comparisons

(defmacro define-comparison (name cmp and truth)
  (let* ((cmp-record (find-instruction-record cmp))
         (result-record (first (instruction-record-result-records cmp-record)))
         (result-type (value-record-name result-record))
         (argument-record (first (instruction-record-argument-records cmp-record)))
         (argument-type (value-record-name argument-record)))
    (when (instruction-record-supported-p cmp-record)
      `(progn
         (export ',name)
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
                                    until (null rest)))))))))))

;; f32.4
(define-comparison f32.4=  two-arg-f32.4=  u32.4-and (1- (expt 2 32)))
(define-comparison f32.4<  two-arg-f32.4<  u32.4-and (1- (expt 2 32)))
(define-comparison f32.4<= two-arg-f32.4<= u32.4-and (1- (expt 2 32)))
(define-comparison f32.4>  two-arg-f32.4>  u32.4-and (1- (expt 2 32)))
(define-comparison f32.4>= two-arg-f32.4>= u32.4-and (1- (expt 2 32)))
;; f64.2
(define-comparison f64.2=  two-arg-f64.2=  u64.2-and (1- (expt 2 64)))
(define-comparison f64.2<  two-arg-f64.2<  u64.2-and (1- (expt 2 64)))
(define-comparison f64.2<= two-arg-f64.2<= u64.2-and (1- (expt 2 64)))
(define-comparison f64.2>  two-arg-f64.2>  u64.2-and (1- (expt 2 64)))
(define-comparison f64.2>= two-arg-f64.2>= u64.2-and (1- (expt 2 64)))
;; f32.8
(define-comparison f32.8=  two-arg-f32.8=  u32.8-and (1- (expt 2 32)))
(define-comparison f32.8<  two-arg-f32.8<  u32.8-and (1- (expt 2 32)))
(define-comparison f32.8<= two-arg-f32.8<= u32.8-and (1- (expt 2 32)))
(define-comparison f32.8>  two-arg-f32.8>  u32.8-and (1- (expt 2 32)))
(define-comparison f32.8>= two-arg-f32.8>= u32.8-and (1- (expt 2 32)))
;; f64.4
(define-comparison f64.4=  two-arg-f64.4=  u64.4-and (1- (expt 2 64)))
(define-comparison f64.4<  two-arg-f64.4<  u64.4-and (1- (expt 2 64)))
(define-comparison f64.4<= two-arg-f64.4<= u64.4-and (1- (expt 2 64)))
(define-comparison f64.4>  two-arg-f64.4>  u64.4-and (1- (expt 2 64)))
(define-comparison f64.4>= two-arg-f64.4>= u64.4-and (1- (expt 2 64)))
;; u32.4
(define-comparison u32.4=  two-arg-u32.4=  u32.4-and (1- (expt 2 32)))
;(define-comparison u32.4<  two-arg-u32.4<  u32.4-and (1- (expt 2 32)))
;(define-comparison u32.4<= two-arg-u32.4<= u32.4-and (1- (expt 2 32)))
(define-comparison u32.4>  two-arg-u32.4>  u32.4-and (1- (expt 2 32)))
;(define-comparison u32.4>= two-arg-u32.4>= u32.4-and (1- (expt 2 32)))
;; u64.2
(define-comparison u64.2=  two-arg-u64.2=  u64.2-and (1- (expt 2 64)))
;(define-comparison u64.2<  two-arg-u64.2<  u64.2-and (1- (expt 2 64)))
;(define-comparison u64.2<= two-arg-u64.2<= u64.2-and (1- (expt 2 64)))
(define-comparison u64.2>  two-arg-u64.2>  u64.2-and (1- (expt 2 64)))
;(define-comparison u64.2>= two-arg-u64.2>= u64.2-and (1- (expt 2 64)))
;; u32.8
(define-comparison u32.8=  two-arg-u32.8=  u32.8-and (1- (expt 2 32)))
;(define-comparison u32.8<  two-arg-u32.8<  u32.8-and (1- (expt 2 32)))
;(define-comparison u32.8<= two-arg-u32.8<= u32.8-and (1- (expt 2 32)))
(define-comparison u32.8>  two-arg-u32.8>  u32.8-and (1- (expt 2 32)))
;(define-comparison u32.8>= two-arg-u32.8>= u32.8-and (1- (expt 2 32)))
;; u64.4
(define-comparison u64.4=  two-arg-u64.4=  u64.4-and (1- (expt 2 64)))
;(define-comparison u64.4<  two-arg-u64.4<  u64.4-and (1- (expt 2 64)))
;(define-comparison u64.4<= two-arg-u64.4<= u64.4-and (1- (expt 2 64)))
(define-comparison u64.4>  two-arg-u64.4>  u64.4-and (1- (expt 2 64)))
;(define-comparison u64.4>= two-arg-u64.4>= u64.4-and (1- (expt 2 64)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Pairwise Non-Equality

(defmacro define-non-equality (name neq and truth)
  (let* ((neq-record (find-instruction-record neq))
         (result-record (first (instruction-record-result-records neq-record)))
         (result-type (value-record-name result-record))
         (argument-record (first (instruction-record-argument-records neq-record)))
         (argument-type (value-record-name argument-record)))
    (when (instruction-record-supported-p neq-record)
      `(progn
         (export ',name)
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
                                     collect `(,',neq ,a ,b))))))))))))

(define-non-equality f32.4/= two-arg-f32.4/= u32.4-and (1- (expt 2 32)))
(define-non-equality f64.2/= two-arg-f64.2/= u64.2-and (1- (expt 2 64)))
(define-non-equality f32.8/= two-arg-f32.8/= u32.8-and (1- (expt 2 32)))
(define-non-equality f64.4/= two-arg-f64.4/= u64.4-and (1- (expt 2 64)))
