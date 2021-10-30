(in-package #:sb-simd-vectorizer)

(defparameter *default-vectorize-instruction-set*
  (find-if
   (lambda (x) (instruction-set-available-p (find-instruction-set x)))
   '(:avx2 :avx :sse4.2 :sse4.1 :ssse3 :sse3 :sse2 :sse :sb-simd)))

(defmacro do-vectorized
    (&whole whole-form (variable start end)
     &body options-and-body
     &environment env)
  (multiple-value-bind (body instruction-set unroll)
      (untangle-do-vectorized-options-and-body options-and-body)
    (let ((*vectorizer-context*
            (make-instance 'vectorizer-context
              :variable variable
              :start start
              :end end
              :unroll unroll
              :instruction-set (find-instruction-set instruction-set)
              :whole-form whole-form)))
      (vir-convert (macroexpand-all `(locally ,@body) env) '())
      (vir-expand))))

(defun untangle-do-vectorized-options-and-body (options-and-body)
  (let* ((pos (position-if-not (lambda (x) (and (consp x) (keywordp (first x)))) options-and-body))
         (instruction-set '())
         (unroll '()))
    (loop for option in (subseq options-and-body 0 pos) do
      (typecase (first option)
        ((eql :instruction-set)
         (unless (and (= 2 (length option))
                      (find-instruction-set (second option)))
           (error "Invalid :INSTRUCTION-SET option: ~S" option))
         (push (second option) instruction-set))
        ((eql :unroll)
         (unless (and (= 2 (length option))
                      (typep (second option) '(integer 1)))
           (error "Invalid :UNROLL option: ~S" option))
         (push (second option) unroll))
        (otherwise
         (error "Unknown DO-VECTORIZED option: ~S" (first option)))))
    (unless (= 1 (length instruction-set))
      (error "Multiple :INSTRUCTION-SET clauses."))
    (unless (= 1 (length unroll))
      (error "Multiple :UNROLL clauses."))
    (values (subseq options-and-body pos)
            (first instruction-set)
            (first unroll))))
