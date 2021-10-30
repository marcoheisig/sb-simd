(in-package #:sb-simd-vectorizer)

;;; Turn all the expressions that have been converted in the current
;;; vectorizer context into a vectorized, unrolled loop.

;;; The SIMD width the vectorizer should aim for.
(defvar *width*)
;;; During expansion, we gather a lot of forms and declarations into
;;; different special variables.  The content of those special variables is
;;; later embedded in a form that is the result of VIR-EXPAND.
(defvar *toplevel-bindings*)
(defvar *vec-outer-bindings*)
(defvar *vec-inner-bindings*)
(defvar *vec-epilogue*)
(defvar *rem-outer-bindings*)
(defvar *rem-inner-bindings*)
(defvar *rem-epilogue*)

;; We process each VIR node, introduce a variable that holds its result,
;; and push the necessary forms such that that variable is properly
;; defined.  When the same VIR node is processed again, we simply retrieve
;; that variable from the cache, which is a (VIR-NODE . VARIABLE) alist.
(defvar *cache*)

(defun vir-expand ()
  (let ((*width* (apply #'max 1 (vir-possible-simd-widths *vectorizer-context*)))
        (*toplevel-bindings* '())
        (*vec-outer-bindings* '())
        (*vec-inner-bindings* '())
        (*vec-epilogue* '())
        (*rem-outer-bindings* '())
        (*rem-inner-bindings* '())
        (*rem-epilogue* '())
        (*cache* '()))
    (mapcar #'reify *vir-roots*)
    (sb-int:with-unique-names (end vend)
      (let ((var *vir-variable*)
            (unroll *vir-unroll*)
            (width *width*))
        `(let* ((,var ,*vir-start*)
                (,end ,*vir-end*)
                ,@(reverse *toplevel-bindings*))
           (declare (index i))
           ;; The unrolled, vectorized loop.
           ,@(unless (= unroll width 1)
               `((let* ((,vend (- ,end ,(1- (* width unroll))))
                        ,@(reverse *vec-outer-bindings*))
                   (declare (ignorable ,@(mapcar #'first *vec-outer-bindings*)))
                   (loop
                     (unless (< ,var ,vend) (return))
                     ,@(loop repeat unroll
                             for offset from 0 by width
                             collect
                             `(let* ((,var (+ ,var ,offset))
                                     ,@(reverse *vec-inner-bindings*))
                                (declare (ignorable ,@(mapcar #'first *vec-inner-bindings*)))
                                (values)))
                     (incf ,var ,(* width unroll)))
                   ,@(reverse *vec-epilogue*))))
           ;; The remainder loop.
           (let* (,@(reverse *rem-outer-bindings*))
             (declare (ignorable ,@(mapcar #'first *rem-outer-bindings*)))
             (loop
               (unless (< ,var ,end) (return))
               (let* (,@(reverse *rem-inner-bindings*))
                 (declare (ignorable ,@(mapcar #'first *rem-inner-bindings*)))
                 (values))
               (incf ,var))
             ,@(reverse *rem-epilogue*)))))))

(defun vectorize (vir-funcall)
  (let* ((vectorizers (vir-funcall-vectorizers vir-funcall))
         (vec-record
           (or (find *width* vectorizers :key #'function-record-simd-width)
               (vir-funcall-function-record vir-funcall)))
         (rem-record
           (or (find 1 vectorizers :key #'function-record-simd-width)
               (vir-funcall-function-record vir-funcall))))
    (values vec-record rem-record)))

(defgeneric reify (vir))

(defmethod reify :around ((vir-node vir-node))
  (let ((entry (assoc vir-node *cache*)))
    (if (consp entry)
        (cdr entry)
        (let ((variable (call-next-method)))
          (push (cons vir-node variable) *cache*)
          variable))))

(defmethod reify ((vir-funcall vir-funcall))
  (with-accessors ((vectorizers vir-funcall-vectorizers)
                   (function-record vir-funcall-function-record)
                   (arguments vir-funcall-arguments)) vir-funcall
    (multiple-value-bind (vec-record rem-record) (vectorize vir-funcall)
      (let* ((vec-fn (function-record-name vec-record))
             (rem-fn (function-record-name rem-record))
             (args (mapcar #'reify arguments))
             (var (gensym)))
        (push `(,var (funcall #',vec-fn ,@args))
              *vec-inner-bindings*)
        (push `(,var (funcall #',rem-fn ,@args))
              *rem-inner-bindings*)
        var))))

(defmethod reify ((vir-store vir-store))
  (with-accessors ((vectorizers vir-funcall-vectorizers)
                   (function-record vir-funcall-function-record)
                   (arguments vir-funcall-arguments)) vir-store
    (multiple-value-bind (vec-record rem-record) (vectorize vir-store)
      (let* ((vec-fn (function-record-name vec-record))
             (rem-fn (function-record-name rem-record))
             (args (mapcar #'reify arguments))
             (var (gensym)))
        (push `(,var (funcall #',vec-fn ,@args))
              *vec-inner-bindings*)
        (push `(,var (funcall #',rem-fn ,@args))
              *rem-inner-bindings*)
        var))))

(defmethod reify ((vir-load vir-load))
  (with-accessors ((vectorizers vir-funcall-vectorizers)
                   (function-record vir-funcall-function-record)
                   (arguments vir-funcall-arguments)) vir-load
    (multiple-value-bind (vec-record rem-record) (vectorize vir-load)
      (let* ((vec-fn (function-record-name vec-record))
             (rem-fn (function-record-name rem-record))
             (args (mapcar #'reify arguments))
             (var (gensym)))
        (push `(,var (funcall #',vec-fn ,@args))
              *vec-inner-bindings*)
        (push `(,var (funcall #',rem-fn ,@args))
              *rem-inner-bindings*)
        var))))

(defmethod reify ((vir-ref vir-ref))
  (vir-ref-variable vir-ref))

(defmethod reify ((vir-index vir-index))
  (let* ((var (gensym))
        (expr `(,var
                (+ ,@(loop for (c . vir-nodes) in (vir-index-expression vir-index)
                           collect
                           `(* ,c ,@(mapcar #'vir-ref-variable vir-nodes)))))))
    (push expr *vec-inner-bindings*)
    (push expr *rem-inner-bindings*)))
