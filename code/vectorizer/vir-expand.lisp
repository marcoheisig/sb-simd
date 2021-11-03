(in-package #:sb-simd-vectorizer)

;;; Turn all the expressions that have been converted in the current
;;; vectorizer context into a vectorized, unrolled loop.

;; The SIMD width the vectorizer should aim for.
(defvar *width*)
;; During expansion, we gather a lot of bindings and declarations into
;; different special variables.  The content of those special variables is
;; later embedded in a form that is the result of VIR-EXPAND.
(defvar *top-bindings*)
(defvar *vec-outer-bindings*)
(defvar *vec-inner-bindings*)
(defvar *vec-epilogue*)
(defvar *rem-outer-bindings*)
(defvar *rem-inner-bindings*)
(defvar *rem-epilogue*)

(defmacro define-cached (name place)
  `(defun ,name (expr &optional (values 1))
     (loop for (vars form) in ,place do
       (when (equal form expr)
         (return-from ,name (values-list vars))))
     (let ((vars (loop repeat values collect (gensym))))
       (push (list vars expr) ,place)
       (values-list vars))))

(define-cached emit-top *top-bindings*)
(define-cached emit-vec-outer *vec-outer-bindings*)
(define-cached emit-vec-inner *vec-inner-bindings*)
(define-cached emit-vec-epilogue *vec-epilogue*)
(define-cached emit-rem-outer *rem-outer-bindings*)
(define-cached emit-rem-inner *rem-inner-bindings*)
(define-cached emit-rem-epilogue *rem-epilogue*)

(defvar *emit-outer*)

(defvar *emit-inner*)

(defvar *emit-epilogue*)

(defun emit-outer (expr &optional (values 1))
  (funcall *emit-outer* expr values))

(defun emit-inner (expr &optional (values 1))
  (funcall *emit-inner* expr values))

(defun emit-epilogue (expr &optional (values 1))
  (funcall *emit-epilogue* expr values))

;;; An alist, mapping from VIR nodes to variables.
(defvar *vir-cache*)

(defmacro bind ((&rest clauses) &body body)
  `(sb-int:binding* ,clauses
     (declare (ignorable ,@(loop for (bindings) in clauses append (ensure-list bindings))))
     ,@body))

(defun vir-expand ()
  (let ((width (apply #'max 1 (vir-possible-simd-widths *vectorizer-context*)))
        (var *vir-variable*)
        (unroll *vir-unroll*)
        (*top-bindings* '())
        (*vec-outer-bindings* '())
        (*vec-inner-bindings* '())
        (*vec-epilogue* '())
        (*rem-outer-bindings* '())
        (*rem-inner-bindings* '())
        (*rem-epilogue* '()))
    (let ((*width* width)
          (*emit-outer* #'emit-vec-outer)
          (*emit-inner* #'emit-vec-inner)
          (*emit-epilogue* #'emit-vec-epilogue)
          (*vir-cache* '()))
      (mapcar #'emit-vir *vir-roots*))
    (let ((*width* 1)
          (*emit-outer* #'emit-rem-outer)
          (*emit-inner* #'emit-rem-inner)
          (*emit-epilogue* #'emit-rem-epilogue)
          (*vir-cache* '()))
      (mapcar #'emit-vir *vir-roots*))
    (sb-int:with-unique-names (end vend)
      `(let* ((,var ,*vir-start*)
              (,end ,*vir-end*))
         (declare (sb-simd:index ,var))
         (bind ,(reverse *top-bindings*)
           #+(or)(declare (optimize (speed 3) (safety 0)))
           ;; The unrolled, vectorized loop.
           ,@(unless (= unroll width 1)
               `((let* ((,vend (- ,end ,(1- (* width unroll)))))
                   (bind ,(reverse *vec-outer-bindings*)
                     (loop
                       (unless (< ,var ,vend) (return))
                       ,@(loop repeat unroll
                               for offset from 0 by width
                               collect
                               `(bind ((,var (+ ,var ,offset))
                                       ,@(reverse *vec-inner-bindings*))
                                  (values)))
                       (incf ,var ,(* width unroll))))
                   ,@(reverse *vec-epilogue*))))
           ;; The remainder loop.
           (bind ,(reverse *rem-outer-bindings*)
             (loop
               (unless (< ,var ,end) (return))
               (bind (,@(reverse *rem-inner-bindings*))
                 (values))
               (incf ,var))
             ,@(reverse *rem-epilogue*)))))))

(defun vectorize (vir-funcall)
  (let* ((vectorizers (vir-funcall-vectorizers vir-funcall)))
    (or (find *width* vectorizers :key #'function-record-simd-width)
        (vir-funcall-function-record vir-funcall))))

(defgeneric emit-vir (vir))

;;; Ensure that each VIR node is only processed once.
(defmethod emit-vir :around ((vir-node vir-node))
  (let ((entry (assoc vir-node *vir-cache*)))
    (if (consp entry)
        (cdr entry)
        (let ((var (call-next-method)))
          (push (cons vir-node var) *vir-cache*)
          var))))

(defmethod emit-vir ((vir-ref vir-ref))
  (vir-ref-variable vir-ref))

(defmethod emit-vir ((vir-funcall vir-funcall))
  (with-accessors ((function-record vir-funcall-function-record)
                   (arguments vir-funcall-arguments)
                   (loop-dependency-p loop-dependency-p)) vir-funcall
    (let* ((record (vectorize vir-funcall))
           (fn (function-record-name record))
           (args (mapcar #'emit-vir arguments)))
      (if loop-dependency-p
          (emit-inner `(,fn ,@args))
          (emit-outer `(,fn ,@args))))))

(defmethod emit-vir ((vir-store vir-store))
  (with-accessors ((function-record vir-funcall-function-record)
                   (arguments vir-funcall-arguments)
                   (loop-dependency-p loop-dependency-p)) vir-store
    (destructuring-bind (vir-value vir-array &rest vir-subscripts) arguments
      (let* ((record (vectorize vir-store))
             (fn (function-record-name (reffer-record-primitive record)))
             (value (emit-vir vir-value)))
        (multiple-value-bind (vector index)
            (emit-array-access vir-array vir-subscripts)
          (if loop-dependency-p
              (emit-inner `(funcall #',fn ,value ,vector ,index))
              (emit-outer `(funcall #',fn ,value ,vector ,index))))))))

(defmethod emit-vir ((vir-load vir-load))
  (with-accessors ((function-record vir-funcall-function-record)
                   (arguments vir-funcall-arguments)
                   (loop-dependency-p loop-dependency-p)) vir-load
    (destructuring-bind (vir-array &rest vir-subscripts) arguments
      (let* ((record (vectorize vir-load))
             (fn (function-record-name (reffer-record-primitive record))))
        (multiple-value-bind (vector index)
            (emit-array-access vir-array vir-subscripts)
          (if loop-dependency-p
              (emit-inner `(,fn ,vector ,index))
              (emit-outer `(,fn ,vector ,index))))))))

(defun sum (emit-fn expressions)
  (let ((filtered-expressions (remove 0 expressions)))
    (case (length filtered-expressions)
      (0 0)
      (1 (first filtered-expressions))
      (otherwise (funcall emit-fn `(index+ ,@filtered-expressions))))))

(defun product (emit-fn expressions)
  (if (find 0 expressions)
      0
      (let ((filtered-expressions (remove 1 expressions)))
        (case (length filtered-expressions)
          (0 1)
          (1 (first filtered-expressions))
          (otherwise (funcall emit-fn `(index* ,@filtered-expressions)))))))

(defun emit-array-access (vir-array vir-subscripts)
  (let* ((array (emit-vir vir-array))
         (rank (length vir-subscripts))
         (dimensions
           (loop for dim from 1 below rank
                 collect
                 (emit-top
                  `(array-dimension ,array ,dim))))
         (strides
           (reverse
            (loop for axis from (1- rank) downto 0
                  for stride = 1 then (product #'emit-top (list (nth axis dimensions) stride))
                  collect stride)))
         (dependent-expressions '())
         (independent-expressions '()))
    (loop for vir-subscript in (reverse vir-subscripts)
          for index-expression = (vir-index-expression vir-subscript)
          do (let ((dependent '())
                   (independent '()))
               (loop for addend in (reverse index-expression) do
                 (if (addend-depends-on addend *vir-variable*)
                     (push addend dependent)
                     (push addend independent)))
               (push dependent dependent-expressions)
               (push independent independent-expressions)))
    (let ((inner-index 0)
          (top-index 0))
      (loop for stride in strides
            for independent in independent-expressions do
              (let* ((products
                       (loop for addend in independent
                             collect (product #'emit-top (addend-expression addend))))
                     (sum (sum #'emit-top products))
                     (new (product #'emit-top (list sum stride))))
                (setf top-index (sum #'emit-top (list new top-index)))))
      (loop for stride in strides
            for dependent in dependent-expressions do
              (let* ((products
                       (loop for addend in dependent
                             collect (product #'emit-inner (addend-expression addend))))
                     (sum (sum #'emit-inner products))
                     (new (product #'emit-inner (list sum stride))))
                (setf inner-index (sum #'emit-inner (list new inner-index)))))
      (multiple-value-bind (vector base)
          (emit-top `(sb-kernel:%data-vector-and-index ,array 0) 2)
        (values
         vector
         (sum #'emit-inner (list (sum #'emit-outer (list base top-index)) inner-index))
         ;; Determine whether the array access is linear in the loop variable.
         (let ((n (length dependent-expressions)))
           (or (zerop n) ;; Constants are trivially linear.
               (and
                (loop for expression in dependent-expressions
                      for index below n
                      always
                      (if (= index (1- n))
                          (index-expression-contiguous-in expression *vir-variable*)
                          (not (index-expression-depends-on expression *vir-variable*))))))))))))

(defmethod emit-vir ((vir-constant vir-constant))
  (vir-constant-object vir-constant))

(defmethod emit-vir ((vir-index vir-index))
  (break "TODO"))
