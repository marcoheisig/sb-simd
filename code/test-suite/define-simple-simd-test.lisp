(in-package #:sb-simd-test-suite)

(defmacro define-simple-simd-test (simd-foo result-types argtypes foo)
  `(define-test ,simd-foo
     ,@(loop for argtype-variant in (argtypes-variants argtypes)
             collect `(test-simple-simd-function ,simd-foo ,result-types ,argtype-variant ,foo))))

(defmacro test-simple-simd-function (simd-foo result-types simplified-argtypes foo)
  (let* ((result-infos (mapcar #'simd-info result-types))
         (argument-infos (mapcar #'simd-info simplified-argtypes))
         (argument-symbols (prefixed-symbols "ARGUMENT-" (length argument-infos)))
         (result-symbols (prefixed-symbols "RESULT-" (length result-infos)))
         (output-symbols (prefixed-symbols "OUTPUT-" (length result-infos)))
         (simd-width (the integer (second (first result-infos)))))
    (assert (apply #'= (append (mapcar #'second result-infos) (mapcar #'second argument-infos))))
    `(loop repeat ,(min (expt 99 (length argument-infos)) 10000) do
      (multiple-value-bind (inputs outputs)
          (find-valid-simd-call
           (lambda ,argument-symbols
             (declare ,@(loop for argument-symbol in argument-symbols
                              for argument-type in (mapcar #'first argument-infos)
                              collect `(type ,argument-type ,argument-symbol)))
             (,foo ,@argument-symbols))
           ',(mapcar #'find-generator (mapcar #'first argument-infos))
           ,simd-width
           ',(mapcar #'third argument-infos)
           ',(mapcar #'third result-infos))
        (destructuring-bind ,argument-symbols inputs
          (destructuring-bind ,output-symbols outputs
            (multiple-value-bind ,result-symbols (,simd-foo ,@argument-symbols)
              ,@(loop for result-type in result-types
                      for result-symbol in result-symbols
                      for output-symbol in output-symbols
                      collect
                      `(is (simd= ,result-symbol ,output-symbol))))))))))

(defun find-valid-simd-call (scalar-function input-generators simd-width
                             input-constructors output-constructors)
  (let ((inputs-list '())
        (outputs-list '()))
    (loop repeat simd-width do
      (multiple-value-bind (inputs outputs)
          (find-valid-scalar-call scalar-function input-generators)
        (push inputs inputs-list)
        (push outputs outputs-list)))
    (values
     (apply #'mapcar #'funcall input-constructors inputs-list)
     (apply #'mapcar #'funcall output-constructors outputs-list))))

(defun find-valid-scalar-call (scalar-function input-generators)
  (let ((attempts 0))
    (loop
      (let ((inputs (mapcar #'funcall input-generators)))
        (handler-case (return (values inputs (multiple-value-list (apply scalar-function inputs))))
          (condition ()
            (incf attempts)
            (when (> attempts 1000)
              (error "Failed to find a valid call to ~S." scalar-function))))))))
