(in-package #:sb-simd-test-suite)

(defun shuffle (list)
  (let ((result (copy-seq list)))
    (loop for tail on result
          for tail-length from (length result) downto 2
          do (rotatef (first tail)
                      (nth (random tail-length) tail)))
    result))

(defun simd-info (name)
  "Returns, as list:

 1. The element type of the SIMD pack.

 2. The number of elements of the SIMD pack.

 3. The name of the function for creating the SIMD pack from individual
    elements.

 4. The name of the function for returning the elements of the SIMD pack as
    multiple values.

 5. The name of the function for comparing such SIMD packs for equality."
  (with-accessors ((scalar-record simd-record-scalar-record)
                   (size simd-record-size))
      (find-value-record name)
    (list
     (scalar-record-name scalar-record)
     size
     (or (find-symbol (format nil "MAKE-~A" (symbol-name name))
                      (symbol-package name))
         (error "No constructor found for ~S." name))
     (or (find-symbol (format nil "~A-VALUES" (symbol-name name))
                      (symbol-package name))
         (error "No unpacker found for ~S." name))
     (or (find-symbol (format nil "~A=" (symbol-name name))
                      (symbol-package name))
         (error "No equality function found for ~S." name)))))

(defmacro simd= (simd-record-name a b)
  (destructuring-bind (element-type simd-width packer unpacker)
      (simd-info simd-record-name)
    (declare (ignore packer))
    (let ((asyms (prefixed-symbols "A" simd-width))
          (bsyms (prefixed-symbols "B" simd-width)))
      `(multiple-value-bind ,asyms (,unpacker ,a)
         (declare (type ,element-type ,@asyms))
         (multiple-value-bind ,bsyms (,unpacker ,b)
           (declare (type ,element-type ,@bsyms))
           (and
            ,@(loop for asym in asyms
                    for bsym in bsyms
                    collect `(eql ,asym ,bsym))))))))

(defun parse-argtypes (argtypes)
  "Returns, as multiple values:

 1. The list of mandatory argtypes.

 2. The list of optional argtypes.

 3. The argtype of the rest argument, or NIL if there is no &rest argtype."
  (labels ((fail ()
             (error "Malformed argtypes list: ~S" argtypes))
           (process-mandatory (argtypes mandatory)
             (if (null argtypes)
                 (values (reverse mandatory) '() nil)
                 (case (first argtypes)
                   ((&optional)
                    (process-optional (rest argtypes) mandatory '()))
                   ((&rest)
                    (process-rest (rest argtypes) mandatory '()))
                   (#.(set-difference lambda-list-keywords '(&optional &rest))
                    (fail))
                   (otherwise
                    (process-mandatory (rest argtypes) (cons (first argtypes) mandatory))))))
           (process-optional (argtypes mandatory optional)
             (if (null argtypes)
                 (values (reverse mandatory) (reverse optional) nil)
                 (case (first argtypes)
                   ((&rest)
                    (process-rest (rest argtypes) mandatory optional))
                   (#.(set-difference lambda-list-keywords '(&rest))
                    (fail))
                   (otherwise
                    (process-optional (rest argtypes) mandatory (cons (first argtypes) optional))))))
           (process-rest (argtypes mandatory optional)
             (if (or (endp argtypes)
                     (not (endp (rest argtypes))))
                 (fail)
                 (values (reverse mandatory) (reverse optional) (first argtypes)))))
    (process-mandatory argtypes '())))

(defun argtypes-variants (argtypes)
  "Returns a list of lists of type specifiers such that each list of type
specifiers satisfies the argument type specification given by ARGTYPES."
  (multiple-value-bind (mandatory optional rest)
      (parse-argtypes argtypes)
    (let ((result '()))
      (loop for n-optional to (length optional) do
        (loop for n-rest below (if (not rest) 0 4) do
          (push (append mandatory
                        (subseq optional 0 n-optional)
                        (make-list n-rest :initial-element rest))
                result)))
      (reverse result))))
