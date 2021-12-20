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
      `(let (;; Rebind all referenced variables and ensure they actually
             ;; have the type derived by the vectorizer.
             ,@(loop for (variable type) in *vir-type-information*
                     unless (eq variable *vir-variable*)
                       collect `(,variable (the ,type ,variable)))
             (,var (the sb-simd:index ,*vir-start*))
             (,end (the sb-simd:index ,*vir-end*)))
         (declare (sb-simd:index ,var ,end))
         (declare (optimize (speed 3) (safety 0)))
         (bind ,(reverse *top-bindings*)
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

(defun vectorize (vir-funcall &key (width *width*))
  (let ((vectorizers (vir-funcall-vectorizers vir-funcall)))
    (or (find width vectorizers :key #'function-record-simd-width)
        (vir-funcall-function-record vir-funcall))))

(defgeneric emit-vir (vir))

;;; Ensure that each VIR node is only processed once.
(defmethod emit-vir :around ((vir-node vir-node))
  (let ((entry (assoc vir-node *vir-cache*)))
    (if (consp entry)
        (cdr entry)
        (let ((var (if (loop-dependency-p vir-node)
                       (call-next-method)
                       (let ((*emit-inner* *emit-outer*))
                         (call-next-method)))))
          (push (cons vir-node var) *vir-cache*)
          var))))

(defmethod emit-vir ((vir-ref vir-ref))
  (vir-ref-variable vir-ref))

(defmethod emit-vir ((vir-constant vir-constant))
  (vir-constant-object vir-constant))

(defmethod emit-vir ((vir-index vir-index))
  (break "TODO"))

(defmethod emit-vir ((vir-funcall vir-funcall))
  (with-accessors ((function-record vir-funcall-function-record)
                   (arguments vir-funcall-arguments)) vir-funcall
    (let* ((record (vectorize vir-funcall))
           (fn (function-record-name record))
           (args (mapcar #'emit-vir arguments)))
      (emit-inner `(,fn ,@args)))))

(defmethod emit-vir ((vir-store vir-store))
  (emit-array-access vir-store))

(defmethod emit-vir ((vir-load vir-load))
  (emit-array-access vir-load))

(defun packer (value-record)
  (let* ((record-name (value-record-name value-record))
         (packer-name (concatenate 'string "MAKE-" (symbol-name record-name)))
         (package (instruction-set-package *vir-instruction-set*)))
    (or (find-symbol packer-name package)
        (vectorizer-error
         "Don't know how to pack objects of type ~S."
         record-name))))

(defun unpacker (value-record)
  (let* ((record-name (value-record-name value-record))
         (constructor-name (concatenate 'string (symbol-name record-name) "-VALUES"))
         (package (instruction-set-package *vir-instruction-set*)))
    (or (find-symbol constructor-name package)
        (vectorizer-error
         "Don't know how to unpack the components objects of type ~S."
         record-name))))

(defun isum (emit-fn &rest expressions)
  (let ((filtered-expressions (remove 0 expressions)))
    (case (length filtered-expressions)
      (0 0)
      (1 (first filtered-expressions))
      (otherwise (funcall emit-fn `(index+ ,@filtered-expressions))))))

(defun iprod (emit-fn &rest expressions)
  (if (find 0 expressions)
      0
      (let ((filtered-expressions (remove 1 expressions)))
        (case (length filtered-expressions)
          (0 1)
          (1 (first filtered-expressions))
          (otherwise (funcall emit-fn `(index* ,@filtered-expressions)))))))

(defun emit-index-expression (emit-fn expression)
  (apply
   #'isum
   emit-fn
   (loop for addend in expression
         collect
         (apply #'iprod emit-fn `(,(first addend) ,@(mapcar #'vir-ref-variable (rest addend)))))))

(defmethod emit-row-major-base-index-constant (subscripts strides offset)
  (let* ((base (if (symbolp offset) offset 0))
         (index 0)
         (constant (if (integerp offset) offset 0))
         (match (lambda (addened) (addend-depends-on addened *vir-variable*))))
    (loop for stride in strides
          for subscript in subscripts
          do (let* ((independent (remove-if match subscript))
                    (value (emit-index-expression #'emit-top independent))
                    (new (iprod #'emit-top value stride)))
               (if (integerp new)
                   (incf constant new)
                   (setf base (isum #'emit-top new base)))))
    (loop for stride in strides
          for subscript in subscripts
          do (let* ((dependent (remove-if-not match subscript))
                    (value (emit-index-expression #'emit-inner dependent))
                    (new (iprod #'emit-inner value stride)))
               (setf index (isum #'emit-inner new index))))
    (values base index constant)))

(defun emit-row-major-index (subscripts strides offset)
  (multiple-value-bind (base index constant)
      (emit-row-major-base-index-constant subscripts strides offset)
    (let ((top-index (isum #'emit-top base constant)))
      (isum #'emit-inner top-index index))))

(defun emit-array-access (vir-access)
  (with-accessors ((reffer-record vir-funcall-function-record)
                   (arguments vir-funcall-arguments)) vir-access
    (multiple-value-bind (value array subscripts)
        (if (typep vir-access 'vir-store)
            (values (first arguments) (second arguments) (rest (rest arguments)))
            (values nil (first arguments) (rest arguments)))
      (let* ((simd-reffer-record (vectorize vir-access))
             (simd-value-record (function-record-result-record simd-reffer-record))
             (scalar-reffer-record (vectorize vir-access :width 1))
             (scalar-value-record (function-record-result-record scalar-reffer-record))
             (bytes-per-element (ceiling (value-record-bits scalar-value-record) 8))
             (value (if (null value) nil (emit-vir value)))
             (array (emit-vir array))
             (subscripts (mapcar #'vir-index-expression subscripts))
             (rank (length subscripts))
             (dimensions
               (loop for dim from 1 below rank
                     collect (emit-top `(array-dimension ,array ,dim))))
             (strides
               (reverse
                (loop for axis from (1- rank) downto 0
                      for stride = 1 then (iprod #'emit-top (nth axis dimensions) stride)
                      collect stride))))
        (multiple-value-bind (vector offset)
            (emit-top `(sb-kernel:%data-vector-and-index ,array 0) 2)
          ;; Determine whether the array access can be handled by a single,
          ;; possibly vectorized memory reference, or requires one memory
          ;; reference per element.
          (if (or (= *width* 1)
                  (loop for subscript in subscripts
                        for axis below rank
                        always
                        (if (= axis (1- rank))
                            (index-expression-contiguous-in subscript *vir-variable*)
                            (not (index-expression-depends-on subscript *vir-variable*)))))
              ;; Emit one memory reference.
              (multiple-value-bind (fn raw-reffer-p) (optimize-reffer simd-reffer-record)
                (multiple-value-bind (base index constant)
                    (emit-row-major-base-index-constant subscripts strides offset)
                  (if raw-reffer-p
                      (let* ((sap (emit-top `(sb-sys:vector-sap ,vector)))
                             (increment (iprod #'emit-top base bytes-per-element))
                             (address (emit-top `(sb-sys:sap-int (sb-sys:sap+ ,sap ,increment)))))
                        (emit-inner
                         `(funcall #',fn ,@(when value `(,value)) ,address ,index ,constant)))
                      (let* ((top-index (isum #'emit-top base constant))
                             (full-index (isum #'emit-inner top-index index)))
                        `(funcall #',fn ,@(when value `(,value)) ,vector ,full-index)))))
              ;; Emit a load or store instruction with one memory reference
              ;; per SIMD pack element.
              (let* ((primitive-record (reffer-record-primitive scalar-reffer-record))
                     (fn (vref-record-vop-raw primitive-record))
                     (sap (emit-top `(sb-sys:vector-sap ,vector)))
                     (addresses '())
                     (indices '())
                     (constants '()))
                (loop for w downfrom (1- *width*) to 0 do
                  (multiple-value-bind (base index constant)
                      (emit-row-major-base-index-constant
                       (subscripts-increment subscripts *vir-variable* w)
                       strides
                       offset)
                    (let ((increment (iprod #'emit-top base bytes-per-element)))
                      (push (emit-top `(sb-sys:sap-int (sb-sys:sap+ ,sap ,increment)))
                            addresses)
                      (push index indices)
                      (push constant constants))))
                (if (not value)
                    ;; Emit one load instruction per row-major index and
                    ;; combine the resulting values.
                    (emit-inner
                     `(,(packer simd-value-record)
                       ,@(loop for address in addresses
                               for index in indices
                               for constant in constants
                               collect
                               (emit-inner `(funcall #',fn ,address ,index ,constant)))))
                    ;; Unpack the scalar values of the supplied value and
                    ;; store them using scalar store instructions.
                    (loop with unpack = (unpacker simd-value-record)
                          for address in addresses
                          for index in indices
                          for constant in constants
                          for scalar in (multiple-value-list (emit-inner `(,unpack ,value) *width*))
                          do (emit-inner `(funcall #',fn ,scalar ,address ,index ,constant))
                          finally (return value))))))))))

(defun subscripts-increment (subscripts variable offset)
  (loop for subscript in subscripts
        collect
        (index-expression-subst
         `((,offset) (1 ,(make-vir-ref variable)))
         variable
         subscript)))

;; Returns two values - the name of the optimized reffer, and whether that
;; reffer is a raw memory reference.
(defun optimize-reffer (reffer-record)
  (let ((primitive (reffer-record-primitive reffer-record)))
    (if (not (vref-record-p primitive))
        (values (reffer-record-name reffer-record) nil)
        (values (vref-record-vop-raw primitive) t))))
