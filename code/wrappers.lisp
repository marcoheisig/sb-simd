(in-package #:sb-simd)

(defmacro define-nary-wrapper (name simd-type two-arg-fn neutral-element)
  (if (and (find-value-record-by-name simd-type)
           (find-instruction-record-by-name two-arg-fn))
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

(define-nary-wrapper f64.2+ f64.2 f64.2-two-arg-* (make-f64.2 0d0 0d0))
(define-nary-wrapper f64.2* f64.2 f64.2-two-arg-* (make-f64.2 1d0 1d0))

(define-nary-wrapper* f64.2- f64.2 f64.2-two-arg-- f64.2-one-arg--)
(define-nary-wrapper* f64.2/ f64.2 f64.2-two-arg-/ f64.2-one-arg-/)


(macrolet ((define-+ (simd-record)
             (let ((name (make-external-symbol (simd-record-name simd-record) '+))
                   (binary-fn (make-internal-symbol (simd-record-name simd-record) '-binary+))
                   (neutral-element
                     `(,(constructor-name simd-record)
                       ,@(loop repeat (simd-record-width simd-record) collect 0)))
                   (one
                     `(,(constructor-name simd-record)
                       ,@(loop repeat (simd-record-width simd-record) collect 1))))
               `(progn
                  (defun ,name (&rest numbers)
                    (if (null numbers)
                        ,neutral-element
                        (let ((acc (first numbers)))
                          (declare (type ,(simd-record-name simd-record) acc))
                          (loop for number of-type ,(simd-record-name simd-record)
                                  in (rest numbers) do (setf acc (,binary-fn acc number)))
                          acc)))
                  (define-compiler-macro ,name (&rest numbers)
                    (if (null numbers)
                        ,neutral-element
                        (reduce (lambda (a b) `(,',binary-fn ,a ,b)) numbers)))
                  (define-modify-macro ,(make-external-symbol (simd-record-name simd-record) '-incf)
                      (&optional (delta ,one))
                    ,binary-fn))))
           (define-- (simd-record)
             (let ((name (make-external-symbol (simd-record-name simd-record) '-))
                   (binary-fn (make-internal-symbol (simd-record-name simd-record) '-binary-))
                   (neutral-element
                     `(,(constructor-name simd-record)
                       ,@(loop repeat (simd-record-width simd-record) collect 0)))
                   (one
                     `(,(constructor-name simd-record)
                       ,@(loop repeat (simd-record-width simd-record) collect 1))))
               `(progn
                  (defun ,name (number &rest more-numbers)
                    (if (null more-numbers)
                        (,binary-fn ,neutral-element number)
                        (let ((acc number))
                          (declare (type ,(simd-record-name simd-record) acc))
                          (loop for number of-type ,(simd-record-name simd-record)
                                  in more-numbers do (setf acc (,binary-fn acc number)))
                          acc)))
                  (define-compiler-macro ,name (number &rest more-numbers)
                    (if (null more-numbers)
                        `(,',binary-fn ,',neutral-element ,number)
                        (reduce (lambda (a b) `(,',binary-fn ,a ,b)) more-numbers
                                :initial-value number)))
                  (define-modify-macro ,(make-external-symbol (simd-record-name simd-record) '-decf)
                      (&optional (delta ,one))
                    ,binary-fn))))
           (define-* (simd-record)
             (let ((name (make-external-symbol (simd-record-name simd-record) '*))
                   (binary-fn (make-internal-symbol (simd-record-name simd-record) '-binary*))
                   (neutral-element
                     `(,(constructor-name simd-record)
                       ,@(loop repeat (simd-record-width simd-record) collect 1))))
               `(progn
                  (defun ,name (&rest numbers)
                    (if (null numbers)
                        ,neutral-element
                        (let ((acc (first numbers)))
                          (declare (type ,(simd-record-name simd-record) acc))
                          (loop for number of-type ,(simd-record-name simd-record)
                                  in (rest numbers) do (setf acc (,binary-fn acc number)))
                          acc)))
                  (define-compiler-macro ,name (&rest numbers)
                    (if (null numbers)
                        ,neutral-element
                        (reduce (lambda (a b) `(,',binary-fn ,a ,b)) numbers))))))
           (define-/ (simd-record)
             (let ((name (make-external-symbol (simd-record-name simd-record) '/))
                   (binary-fn (make-internal-symbol (simd-record-name simd-record) '-binary/))
                   (neutral-element
                     `(,(constructor-name simd-record)
                       ,@(loop repeat (simd-record-width simd-record) collect 0))))
               `(progn
                  (defun ,name (number &rest more-numbers)
                    (if (null more-numbers)
                        (,binary-fn ,neutral-element number)
                        (let ((acc number))
                          (declare (type ,(simd-record-name simd-record) acc))
                          (loop for number of-type ,(simd-record-name simd-record)
                                  in more-numbers do (setf acc (,binary-fn acc number)))
                          acc)))
                  (define-compiler-macro ,name (number &rest more-numbers)
                    (if (null more-numbers)
                        `(,',binary-fn ,',neutral-element ,number)
                        (reduce (lambda (a b) `(,',binary-fn ,a ,b)) more-numbers
                                :initial-value number))))))
           (define-arithmetic-functions (simd-record)
             `(progn (define-+ ,simd-record)
                     (define-- ,simd-record)
                     (define-* ,simd-record)
                     (define-/ ,simd-record)))
           (define-all-arithmetic-functions ()
             `(progn
                ,@(loop for simd-record in *simd-records*
                        collect
                        `(define-arithmetic-functions ,simd-record)))))
  (define-all-arithmetic-functions))
