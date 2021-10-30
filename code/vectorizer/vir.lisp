(in-package #:sb-simd-vectorizer)

;;; VIR is the vectorizer intermediate representation.  It is a data flow
;;; graph of scalar operations that is executed once for each point of the
;;; iteration space of the surrounding loop.  The roots of the graph are
;;; stores to memory or reductions, and the leaves of the graph are
;;; constants, references to variables from outside of the loop, and loads
;;; from memory.
;;;
;;; The processing of VIR always occurs in a particular context, which we
;;; describe with the vectorizer-context class.

(defvar *vectorizer-context*)

(defclass vectorizer-context (printable)
  (;; The iteration variable.
   (%variable
    :type non-nil-symbol
    :initarg :variable
    :initform (required-argument :variable)
    :reader vectorizer-context-variable)
   ;; A form that, when evaluated, returns the start of the iteration
   ;; space.
   (%start
    :initarg :start
    :initform (required-argument :start)
    :reader vectorizer-context-start)
   ;; A form that, when evaluated, returns the end of the iteration space.
   (%end
    :initarg :end
    :initform (required-argument :end)
    :reader vectorizer-context-end)
   ;; The unrolling factor of that VIR.
   (%unroll
    :type (integer 1)
    :initarg :unroll
    :initform (required-argument :unroll)
    :reader vectorizer-context-unroll)
   ;; The instruction set being used to vectorize this VIR.
   (%instruction-set
    :type instruction-set
    :initarg :instruction-set
    :reader vectorizer-context-instruction-set)
   ;; The original form that was supplied to the VIR conversion.
   (%whole-form
    :initarg :whole-form
    :initform (required-argument :whole-form)
    :reader vectorizer-context-whole-form)
   ;; All data flow graph roots.
   (%roots
    :type list
    :initform '()
    :accessor vectorizer-context-roots)
   ;; All data flow graph leaves.
   (%leaves
    :type list
    :initform '()
    :accessor vectorizer-context-leaves)
   ;; An integer that gets incremented for each new VIR node.
   (%node-counter
    :type unsigned-byte
    :initform 0
    :accessor vectorizer-context-node-counter)
   ;; A list of (VARIABLE TYPE) entries.  We use this data structure to
   ;; record the assumed type of each variable that appears free in the
   ;; loop body, and to signal an error if there are contradictory assumed
   ;; types.
   (%type-information
    :type list
    :initform '()
    :accessor vectorizer-context-type-information)
   ;; The following three tables are used to perform Common Subexpression
   ;; Elimination (CSE) during VIR conversion.
   ;;
   ;; A hash table, mapping from lists whose CAR is a function record and
   ;; whose CDR is a list of VIR nodes to the VIR node that combines them.
   (%funcall-table
    :initform (make-hash-table :test #'equal)
    :reader vectorizer-context-funcall-table)
   ;; A hash table, mapping from canonical index expressions to index
   ;; nodes.
   (%index-table
    :initform (make-hash-table :test #'equal)
    :reader vectorizer-context-index-table)
   ;; A hash table, mapping from literal objects to the VIR nodes that wrap
   ;; them.
   (%constant-table
    :initform (make-hash-table :test #'eql)
    :reader vectorizer-context-constant-table)
   ;; A hash table, mapping from variables to the VIR nodes that access them.
   (%variable-table
    :initform (make-hash-table :test #'eq)
    :reader vectorizer-context-variable-table)))

(defmethod sb-simd-internals:printable-slot-plist append ((vectorizer-context vectorizer-context))
  `(:variable ,(vectorizer-context-variable vectorizer-context)
    :start ,(vectorizer-context-start vectorizer-context)
    :end ,(vectorizer-context-end vectorizer-context)
    :unroll ,(vectorizer-context-unroll vectorizer-context)
    :instruction-set ,(vectorizer-context-instruction-set vectorizer-context)
    :whole-form ,(vectorizer-context-whole-form vectorizer-context)
    :roots ,(vectorizer-context-roots vectorizer-context)
    :leaves ,(vectorizer-context-leaves vectorizer-context)
    :node-counter ,(vectorizer-context-node-counter vectorizer-context)
    :type-information ,(vectorizer-context-type-information vectorizer-context)
    :funcall-table ,(vectorizer-context-funcall-table vectorizer-context)
    :index-table ,(vectorizer-context-index-table vectorizer-context)
    :constant-table ,(vectorizer-context-constant-table vectorizer-context)
    :variable-table ,(vectorizer-context-variable-table vectorizer-context)))

;;; Define a bunch of symbol macros to pretend each slot of the vectorizer
;;; context is a special variable.
(define-symbol-macro *vir-variable* (vectorizer-context-variable *vectorizer-context*))
(define-symbol-macro *vir-start* (vectorizer-context-start *vectorizer-context*))
(define-symbol-macro *vir-end* (vectorizer-context-end *vectorizer-context*))
(define-symbol-macro *vir-unroll* (vectorizer-context-unroll *vectorizer-context*))
(define-symbol-macro *vir-instruction-set* (vectorizer-context-instruction-set *vectorizer-context*))
(define-symbol-macro *vir-whole-form* (vectorizer-context-whole-form *vectorizer-context*))
(define-symbol-macro *vir-roots* (vectorizer-context-roots *vectorizer-context*))
(define-symbol-macro *vir-leaves* (vectorizer-context-leaves *vectorizer-context*))
(define-symbol-macro *vir-node-counter* (vectorizer-context-node-counter *vectorizer-context*))
(define-symbol-macro *vir-type-information* (vectorizer-context-type-information *vectorizer-context*))
(define-symbol-macro *vir-funcall-table* (vectorizer-context-funcall-table *vectorizer-context*))
(define-symbol-macro *vir-index-table* (vectorizer-context-index-table *vectorizer-context*))
(define-symbol-macro *vir-constant-table* (vectorizer-context-constant-table *vectorizer-context*))
(define-symbol-macro *vir-variable-table* (vectorizer-context-variable-table *vectorizer-context*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Error Handling

(define-condition vectorizer-error (error)
  ((%context
    :type vectorizer-context
    :initform *vectorizer-context*
    :reader vectorizer-error-context)
   (%format-control
    :initarg :format-control
    :initform (required-argument :format-control)
    :reader vectorizer-error-format-control)
   (%format-arguments
    :initarg :format-arguments
    :initform (required-argument :format-arguments)
    :reader vectorizer-error-format-arguments))
  (:report
   (lambda (condition stream)
     (format stream "~?~%~%While processing the form~%~S~%"
             (vectorizer-error-format-control condition)
             (vectorizer-error-format-arguments condition)
             (vectorizer-context-whole-form
              (vectorizer-error-context condition))))))

(defun vectorizer-error (format-control &rest format-arguments)
  (error 'vectorizer-error
          :format-control format-control
          :format-arguments format-arguments))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; VIR Nodes

(defclass vir-node (sb-simd-internals:printable)
  ((%number
    :type unsigned-byte
    :initform (incf *vir-node-counter*)
    :reader vir-node-number)))

(defclass vir-leaf (vir-node)
  ())

(defmethod shared-initialize :after ((vir-leaf vir-leaf) slot-names &key &allow-other-keys)
  (pushnew vir-leaf *vir-leaves*))

;;; Constants

(defclass vir-constant (vir-leaf)
  ((%object
    :initarg :object
    :initform (required-argument :object)
    :reader vir-constant-object)
   (%value-record
    :initarg :value-record
    :initform (required-argument :value-record)
    :reader vir-constant-value-record)))

(defmethod sb-simd-internals:printable-slot-plist append ((vir-constant vir-constant))
  `(:object ,(vir-constant-object vir-constant)))

(defun make-vir-constant (object)
  (symbol-macrolet ((place (gethash object *vir-constant-table*)))
    (multiple-value-bind (value present) place
      (if present value
          (setf place (make-instance 'vir-constant
                        :object object
                        :value-record (value-record-of object)))))))

(defun value-record-of (object)
  (typecase object
    (sb-simd:u8  (find-value-record 'sb-simd:u8))
    (sb-simd:u16 (find-value-record 'sb-simd:u16))
    (sb-simd:u32 (find-value-record 'sb-simd:u32))
    (sb-simd:u64 (find-value-record 'sb-simd:u64))
    (sb-simd:s8  (find-value-record 'sb-simd:s8))
    (sb-simd:s16 (find-value-record 'sb-simd:s16))
    (sb-simd:s32 (find-value-record 'sb-simd:s32))
    (sb-simd:s64 (find-value-record 'sb-simd:s64))
    (sb-simd:f32 (find-value-record 'sb-simd:f32))
    (sb-simd:f64 (find-value-record 'sb-simd:f64))
    (otherwise
     (vectorizer-error
      "The object ~S doesn't have a type supported by the vectorizer."
      object))))

;;; Variable References

(defclass vir-ref (vir-leaf)
  (;; The variable being referenced.
   (%variable
    :initarg :variable
    :initform (required-argument :variable)
    :reader vir-ref-variable)))

(defmethod sb-simd-internals:printable-slot-plist append ((vir-ref vir-ref))
  `(:variable ,(vir-ref-variable vir-ref)))

(defun make-vir-ref (variable)
  (symbol-macrolet ((place (gethash variable *vir-variable-table*)))
    (multiple-value-bind (value present) place
      (if present value
          (setf place (make-instance 'vir-ref :variable variable))))))

;;; Index Expressions
;;;
;;; We represent each index as an index expression, which is as a list of
;;; addends.  Each added is a list of the form (C V1 ... VN), where C is an
;;; integer and where V1 to VN are (possibly duplicate) VIR nodes.  The
;;; value of an addend is computed by multiplying the integer C and the
;;; values of each variable V1 to VN.  The value of the entire index
;;; expression is computed by summing up the value of each addend.
;;;
;;; A canonical addend is one where all VIR nodes have been sorted by their
;;; unique VIR number, such that nodes with a lower number come first, and
;;; whose first entry is not zero.  A canonical index expression consists
;;; of canonical addends where no two addends have the same list of VIR
;;; nodes.  Furthermore, these addends of a canonical index expression must
;;; be sorted by their length, and, in case they have the same length, by a
;;; pairwise comparison of the VIR numbers of their nodes, such that the
;;; addend with the first pair with a lower number comes first.

(defun canonicalize-index-expression (e)
  (flet ((canonicalize-addend (addend)
           `(,(first addend)
             ,@(sort (copy-list (rest addend)) #'< :key #'vir-node-number)))
         (addend< (a1 a2)
           (or (< (length a1) (length a2))
               (loop for ref1 in (rest a1)
                     for ref2 in (rest a2)
                     for n1 = (vir-node-number ref1)
                     for n2 = (vir-node-number ref2)
                     do (cond ((< n1 n2) (return t))
                              ((< n2 n1) (return nil)))))))
    (let ((result '())
          (current nil))
      (loop for addend in (sort (mapcar #'canonicalize-addend e) #'addend<) do
        (cond ((null current)
               (setf current addend))
              ((equal (rest current) (rest addend))
               (setf current `(,(+ (first current) (first addend)) ,@(rest current))))
              (t
               (unless (zerop (first current))
                 (push current result))
               (setf current addend))))
      (unless (or (null current) (zerop (first current)))
        (push current result))
      result)))

(defun negate-index-expression (e)
  (loop for (c . vir-refs) in e
        collect `((- c) ,@vir-refs)))

(defun add-index-expressions (e1 e2)
  (canonicalize-index-expression
   (append e1 e2)))

(defun multiply-index-expressions (e1 e2)
  (let ((e '()))
    (loop for (c1 . refs1) in e1 do
      (loop for (c2 . refs2) in e2 do
        (push `(,(* c1 c2) ,@refs1 ,@refs2) e)))
    (canonicalize-index-expression e)))

(defgeneric vir-index-expression (vir)
  (:method ((vir-constant vir-constant))
    `((,(vir-constant-object vir-constant))))
  (:method ((vir-node vir-node))
    `((1 ,vir-node))))

(defclass vir-index (vir-node)
  (;; The canonical encoding of the index.
   (%expression
    :type list
    :initarg :expression
    :initform (required-argument :expression)
    :reader vir-index-expression)))

(defun make-vir-index (index-expression)
  (symbol-macrolet ((place (gethash index-expression *vir-index-table*)))
    (or place (setf place (make-instance 'vir-index :expression index-expression)))))

(defun vir-index+ (&rest virs)
  (make-vir-index
   (reduce #'add-index-expressions virs
             :key #'vir-index-expression
             :initial-value `((0)))))

(defun vir-index* (&rest virs)
  (make-vir-index
   (reduce #'multiply-index-expressions virs
             :key #'vir-index-expression
             :initial-value `((1)))))

(defun vir-index- (vir &rest more-vir)
  (make-vir-index
   (reduce #'add-index-expressions more-vir
             :key (lambda (x) (negate-index-expression (vir-index-expression x)))
             :initial-value (vir-index-expression vir))))

;;; Function Calls

(defgeneric vir-funcall (function-record arguments))

(defclass vir-funcall (vir-node)
  (;; The function being called.
   (%function-record
    :initarg :function-record
    :initform (required-argument :function-record)
    :reader vir-funcall-function-record)
   ;; A list of VIR entities.
   (%arguments
    :initarg :arguments
    :initform '()
    :reader vir-funcall-arguments)
   ;; A list of SIMD function records.
   (%vectorizers
    :initarg :vectorizers
    :initform '()
    :reader vir-funcall-vectorizers)))

(defmethod sb-simd-internals:printable-slot-plist append ((vir-funcall vir-funcall))
  `(:function-record ,(vir-funcall-function-record vir-funcall)
    :arguments ,(vir-funcall-arguments vir-funcall)
    :vectorizers ,(vir-funcall-vectorizers vir-funcall)))

(defmethod vir-funcall :before ((function-record function-record) arguments)
  (let ((required (length (function-record-required-argument-records function-record)))
        (supplied (length arguments)))
    (unless (>= supplied required)
      (vectorizer-error
       "Only ~R argument~:P supplied to the function ~S that expects at least ~R argument~:P."
       supplied (function-record-name function-record) required))
    (when (not (function-record-rest-argument-record function-record))
      (unless (= supplied required)
        (vectorizer-error
         "~@(~R) argument~:P supplied to the function ~s that expects exactly ~R argument~:P."
         supplied (function-record-name function-record) required)))))

(defmethod vir-funcall :around ((function-record function-record) arguments)
  (let ((key (list* function-record arguments)))
    (symbol-macrolet ((place (gethash key *vir-funcall-table*)))
      (multiple-value-bind (value present) place
        (if present value
            (setf place (call-next-method)))))))

(defmethod vir-funcall ((function-record function-record) arguments)
  (make-instance 'vir-funcall
    :function-record function-record
    :arguments arguments
    :vectorizers (instruction-set-vectorizers *vir-instruction-set* function-record)))

;;; Loads

(defclass vir-load (vir-funcall)
  ())

(defmethod vir-funcall ((aref-record aref-record) arguments)
  (make-instance 'vir-load
    :function-record aref-record
    :arguments arguments
    :vectorizers (instruction-set-vectorizers *vir-instruction-set* aref-record)))

(defmethod vir-funcall ((row-major-aref-record row-major-aref-record) arguments)
  (make-instance 'vir-load
    :function-record row-major-aref-record
    :arguments arguments
    :vectorizers (instruction-set-vectorizers *vir-instruction-set* row-major-aref-record)))

;;; Stores

(defclass vir-root (vir-funcall)
  ())

(defmethod shared-initialize :after ((vir-root vir-root) slot-names &key &allow-other-keys)
  (pushnew vir-root *vir-roots*))

(defclass vir-store (vir-root)
  ())

(defmethod vir-funcall ((setf-aref-record setf-aref-record) arguments)
  (make-instance 'vir-store
    :function-record setf-aref-record
    :arguments arguments
    :vectorizers (instruction-set-vectorizers *vir-instruction-set* setf-aref-record)))

(defmethod vir-funcall ((setf-row-major-aref-record setf-row-major-aref-record) arguments)
  (make-instance 'vir-store
    :function-record setf-row-major-aref-record
    :arguments arguments
    :vectorizers (instruction-set-vectorizers *vir-instruction-set* setf-row-major-aref-record)))

;;; Reductions

(defclass vir-reduce (vir-root)
  ((%variable
    :initarg :variable
    :initform (required-argument :variable)
    :reader vir-reduce-variable)
   (%value
    :initarg :value
    :initform (required-argument :value)
    :reader vir-reduce-value)))

(defmethod sb-simd-internals:printable-slot-plist append ((vir-reduce vir-reduce))
  `(:variable ,(vir-reduce-variable vir-reduce)
    :value ,(vir-reduce-value vir-reduce)))

(defclass vir-sum (vir-reduce)
  ())

;;; Type Checking

(defgeneric vir-declare-type (vir-node type)
  (:method ((vir-ref vir-ref) type)
    (let* ((variable (vir-ref-variable vir-ref))
           (entry (assoc variable *vir-type-information*)))
      (if (null entry)
          (push (list variable type) *vir-type-information*)
          (let* ((old-type (second entry))
                 (new-type `(and ,old-type ,type)))
            (when (subtypep new-type nil)
              (vectorizer-error
               "Contradictory expected types ~S and ~S for variable ~S."
               type old-type variable))
            (unless (subtypep old-type new-type)
              (if (subtypep new-type type)
                  (setf (second entry) type)
                  (setf (second entry) new-type)))))))
  (:method ((vir-constant vir-constant) type)
    (unless (typep (vir-constant-object vir-constant) type)
      (vectorizer-error
       "The object ~S is of the expected type ~S."
       (vir-constant-object vir-constant) type)))
  (:method ((vir-funcall vir-funcall) type)
    (let* ((function-record (vir-funcall-function-record vir-funcall))
           (value-record (function-record-result-record function-record)))
      (unless (subtypep (value-record-type value-record) type)
        (vectorizer-error
         "The function ~S produces values of type ~S ~
          but is expected to return values of type ~S."
         (function-record-name function-record)
         (value-record-name value-record)
         type))))
  (:method ((vir-index vir-index) type)
    (unless (subtypep type 'sb-simd:index)
      (vectorizer-error
       "Found an index where a ~S was expected."
       type))))

(defmethod vir-funcall :after ((aref-record aref-record) arguments)
  (let ((result-record (function-record-result-record aref-record))
        (array (first arguments))
        (indices (rest arguments)))
    (assert (scalar-record-p result-record))
    (dolist (index indices)
      (vir-declare-type index 'sb-simd:index))
    (vir-declare-type array `(array ,(value-record-type result-record) ,(length indices)))))

(defmethod vir-funcall :after ((setf-aref-record setf-aref-record) arguments)
  (let ((result-record (function-record-result-record setf-aref-record))
        (array (second arguments))
        (indices (rest (rest arguments))))
    (assert (scalar-record-p result-record))
    (dolist (index indices)
      (vir-declare-type index 'sb-simd:index))
    (vir-declare-type array `(array ,(value-record-type result-record) ,(length indices)))))

(defmethod vir-funcall :after ((row-major-aref-record row-major-aref-record) arguments)
  (let ((result-record (function-record-result-record row-major-aref-record))
        (array (first arguments))
        (index (second arguments)))
    (assert (scalar-record-p result-record))
    (vir-declare-type array `(array ,(value-record-type result-record)))
    (vir-declare-type index 'sb-simd:index)))

(defmethod vir-funcall :after ((setf-row-major-aref-record setf-row-major-aref-record) arguments)
  (let ((result-record (function-record-result-record setf-row-major-aref-record))
        (array (second arguments))
        (index (third arguments)))
    (assert (scalar-record-p result-record))
    (vir-declare-type array `(array ,(value-record-type result-record)))
    (vir-declare-type index 'sb-simd:index)))

;;; Auxiliary Functions

(defgeneric vir-possible-simd-widths (vir)
  (:method ((vectorizer-context vectorizer-context))
    (let ((roots (vectorizer-context-roots vectorizer-context)))
      (if (null roots)
          '(1)
           (reduce #'intersection roots :key #'vir-possible-simd-widths))))
  (:method ((vir-leaf vir-leaf))
    '(1 2 4 8 16 32))
  (:method ((vir-funcall vir-funcall))
    (reduce #'intersection
              (vir-funcall-arguments vir-funcall)
              :key #'vir-possible-simd-widths
              :initial-value
              (remove-duplicates
               (mapcar
                (lambda (vectorizer)
                  (value-record-simd-width
                   (function-record-result-record vectorizer)))
                (vir-funcall-vectorizers vir-funcall))))))
