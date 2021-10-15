(in-package #:sb-simd-internals)

;;; A record describes a particular function or data type.  Macros later
;;; use the information stored in these records to generate most of the
;;; code of this library.

(defclass record (printable)
  ((%name
    :type (or non-nil-symbol function-name)
    :initarg :name
    :initform (required-argument :name)
    :reader record-name)
   (%instruction-set
    :type instruction-set
    :initarg :instruction-set
    :initform *instruction-set*
    :reader record-instruction-set)))

(defun record-p (x)
  (typep x 'record))

;;; Ensure that the home package of the name of the record is the same as
;;; the package of its instruction set.
(defmethod shared-initialize :after
    ((record record) slot-names &key &allow-other-keys)
  (with-accessors ((name record-name)
                   (instruction-set record-instruction-set)) record
    (unless (eq (instruction-set-package instruction-set)
                (symbol-package name))
      (error "Wrong home package ~S for ~S record ~S."
             (symbol-package name)
             (instruction-set-name instruction-set)
             name))))

(defmethod printable-slot-plist append ((record record))
  (list :name (record-name record)
        :instruction-set (record-instruction-set record)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Value Record
;;;
;;; A value record describes a specialized set of Common Lisp objects.
;;; Each value record consists of a Common Lisp type specifier, a
;;; corresponding primitive type specifier used by SBCL's VM, the number of
;;; bits required to represent all such objects, and a list of storage
;;; classes in which such objects can be stored.

(defclass value-record (record)
  (;; Define aliases for inherited slots.
   (%name :reader value-record-name)
   (%instruction-set :reader value-record-instruction-set)
   ;; The Common Lisp type of this value.
   (%type
    :type type-specifier
    :initarg :type
    :initform (required-argument :type)
    :reader value-record-type)
   ;; The primitive type of this value as used by SBCL's VM.
   (%primitive-type
    :type type-specifier
    :initarg :primitive-type
    :initform (required-argument :primitive-type)
    :reader value-record-primitive-type)
   ;; The number of bits that are necessary to represent this value in
   ;; memory.
   (%bits
    :type unsigned-byte
    :initarg :bits
    :initform (required-argument :bits)
    :reader value-record-bits)
   ;; A list of storage classes where this value can be placed.
   (%scs
    :type list
    :initarg :scs
    :initform (required-argument :scs)
    :reader value-record-scs)))

(defun value-record-p (x)
  (typep x 'value-record))

(defmethod printable-slot-plist append ((value-record value-record))
  (list :type (value-record-type value-record)
        :primitive-type (value-record-primitive-type value-record)
        :bits (value-record-bits value-record)
        :scs (value-record-scs value-record)))

;;; A hash table, mapping from value record names to value records.
(declaim (hash-table *value-records*))
(defparameter *value-records* (make-hash-table :test #'eq))

(defun find-value-record (name &optional (errorp t))
  (or (gethash name *value-records*)
      (when errorp
        (error "There is no value record with the name ~S."
               name))))

;;; Ensure that each value record is registered in the *VALUE-RECORDS* hash
;;; table.
(defmethod shared-initialize :after
    ((value-record value-record) slot-names &key &allow-other-keys)
  (setf (gethash (value-record-name value-record) *value-records*)
        value-record))

;; Interns a string designator into the SB-VM package, while gracefully
;; handling the case where the symbol is not present.
(defun find-sc (sc)
  (or (find-symbol (string sc) "SB-VM")
      'sb-vm::descriptor-reg))

;; Intern all symbols and strings in EXPR that have no home package in the
;; SB-VM package.
(defun intern-primitive-type (expr)
  (etypecase expr
    (string (intern expr "SB-VM"))
    (symbol (if (null (symbol-package expr))
                (intern-primitive-type (symbol-name expr))
                expr))
    (integer expr)
    (list (mapcar #'intern-primitive-type expr))))

(defmethod decode-record-definition ((_ (eql 'value-record)) expr)
  (destructuring-bind (name bits type primitive-type &optional (scs '(#:descriptor-reg))) expr
    `(make-instance 'value-record
       :name ',name
       :bits ,bits
       :type ',type
       :primitive-type ',(intern-primitive-type primitive-type)
       :scs ',(mapcar #'find-sc scs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SIMD Record

(defclass simd-record (value-record)
  (;; Define aliases for inherited slots.
   (%name :reader simd-record-name)
   (%instruction-set :reader simd-record-instruction-set)
   (%type :reader simd-record-type)
   (%primitive-type :reader simd-record-primitive-type)
   (%bits :reader simd-record-bits)
   (%scs :reader simd-record-scs)
   ;; The scalar record of the elements of this SIMD pack.
   (%scalar-record
    :type value-record
    :initarg :scalar-record
    :initform (required-argument :scalar-record)
    :reader simd-record-scalar-record)
   ;; The number of scalar elements in this SIMD pack.
   (%length
    :type unsigned-byte
    :initarg :length
    :initform (required-argument :length)
    :reader simd-record-length)))

(defun simd-record-p (x)
  (typep x 'simd-record))

(defmethod decode-record-definition ((_ (eql 'simd-record)) expr):w
  (destructuring-bind (name scalar-record-name bits primitive-type scs) expr
    (let ((simd-pack-type
            (let ((base-type
                    (ecase bits
                      (128 (find-symbol "SIMD-PACK" "SB-EXT"))
                      (256 (find-symbol "SIMD-PACK-256" "SB-EXT")))))
              (cond ((not base-type) 't)
                    ((not scalar-record-name) base-type)
                    (t `(,base-type ,scalar-record-name))))))
      `(let ((.scalar-record. (find-value-record ',(or scalar-record-name (find-symbol "U64")))))
         (make-instance 'simd-record
           :name ',name
           :scalar-record .scalar-record.
           :bits ',bits
           :length (the unsigned-byte (/ ,bits (value-record-bits .scalar-record.)))
           :type ',simd-pack-type
           :primitive-type ',(intern-primitive-type primitive-type)
           :scs ',(mapcar #'find-sc scs))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function Record
;;;
;;; A function record describes one or more Common Lisp functions.
;;; Depending on its attributes, the function record will later be used to
;;; define zero or more VOPs, defknowns, deftransforms, defuns and compiler
;;; macros.

(defclass function-record (record)
  (;; Define aliases for inherited slots.
   (%name :reader function-record-name)
   (%instruction-set :reader function-record-instruction-set)
   ;; A list of function records of scalar functions that can be vectorized
   ;; by the function denoted by this function record.
   (%scalar-variants
    :type list
    :initarg :scalar-variants
    :initform '()
    :reader function-record-scalar-variants)))

;;; A generic function that returns, as multiple values, the value records
;;; returned by the function(s) denoted by this function record.
(defgeneric function-record-return-values (function-record))

(defun function-record-p (x)
  (typep x 'function-record))

(defun scalar-function-record-p (x)
  (and (function-record-p x)
       (not (simd-record-p (function-record-return-values x)))))

(defun simd-function-record-p (x)
  (and (function-record-p x)
       (simd-record-p (function-record-return-values x))))

;;; Translate a supplied :VECTORIZES keyword argument into a
;;; :SCALAR-VARIANTS keyword.
(defmethod shared-initialize :around
    ((function-record function-record) slot-names &rest rest &key vectorizes)
  (let ((function-records (mapcar #'find-function-record (ensure-list vectorizes))))
    (loop for function-record in function-records do
      (assert (scalar-function-record-p function-record)))
    (apply #'call-next-method function-record slot-names
           :scalar-variants function-records
           rest)))

;;; A hash table, mapping from instruction names to instruction records.
(declaim (hash-table *function-records*))
(defparameter *function-records* (make-hash-table :test #'equal))

(defun find-function-record (name &optional (errorp t))
  (or (gethash name *function-records*)
      (when errorp
        (error "There is no function with the name ~S."
               name))))

;;; Ensure that each function record is registered in *FUNCTION-RECORDS*,
;;; and that vectorizing functions are registered in their instruction set.
(defmethod shared-initialize :after
    ((function-record function-record) slot-names &key &allow-other-keys)
  (loop for scalar-variant in (function-record-scalar-variants function-record) do
    (register-vectorizer scalar-variant function-record))
  (setf (gethash (function-record-name function-record) *function-records*)
        function-record))

(defun filter-function-records (predicate)
  (loop for function-record being the hash-values of *function-records*
        when (funcall predicate function-record)
          collect function-record))

(defun filter-available-function-records (predicate)
  (filter-function-records
   (lambda (function-record)
     (and (instruction-set-available-p (function-record-instruction-set function-record))
          (funcall predicate function-record)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction Record
;;;
;;; An instruction record describes a function that can more or less
;;; directly be expressed as a single assembler instruction.

(defclass instruction-record (function-record)
  (;; Define aliases for inherited slots.
   (%name :reader instruction-record-name)
   (%instruction-set :reader instruction-record-instruction-set)
   ;; The name of the VOP that translates this instruction.
   (%vop
    :type non-nil-symbol
    :initarg :vop
    :initform (required-argument :vop)
    :reader instruction-record-vop)
   ;; The mnemonic that is used within the VOP to emit this instruction.
   (%mnemonic
    :type symbol
    :initarg :mnemonic
    :initform (required-argument :mnemonic)
    :reader instruction-record-mnemonic)
   ;; A list of value records - one for each result.
   (%result-records
    :type list
    :initarg :result-records
    :initform (required-argument :result-records)
    :reader instruction-record-result-records)
   ;; A list of value records - one for each argument.
   (%argument-records
    :type list
    :initarg :argument-records
    :initform (required-argument :argument-records)
    :reader instruction-record-argument-records)
   ;; A rough estimate of the cost of executing that instruction.
   (%cost
    :type unsigned-byte
    :initarg :cost
    :initform 1
    :reader instruction-record-cost)
   ;; Whether this instruction satisfies (INST a b) = (INST b a).
   (%commutative
    :type boolean
    :initarg :commutative
    :initform nil
    :reader instruction-record-commutative)
   ;; Whether this instruction is free of side-effects.
   (%pure
    :type boolean
    :initarg :pure
    :initform t
    :reader instruction-record-pure)
   ;; Whether this instruction can always be translated into a VOP.
   (%always-translatable
    :type boolean
    :initarg :always-translatable
    :initform t
    :reader instruction-record-always-translatable)
   ;; How the instruction is turned into a VOP.
   (%encoding
    :type (member :standard :sse :sse+xmm0 :custom :none :move)
    :initarg :encoding
    :initform :standard
    :reader instruction-record-encoding)
   ;; A list that, if provided, supplies the first arguments to the
   ;; mnemonic.
   (%prefix
    :type list
    :initarg :prefix
    :initform '()
    :reader instruction-record-prefix)
   ;; A list that, if provided, supplies the last arguments to the
   ;; mnemonic.
   (%suffix
    :type list
    :initarg :suffix
    :initform '()
    :reader instruction-record-suffix)))

(defun instruction-record-p (x)
  (typep x 'instruction-record))

(defmethod function-record-return-values ((instruction-record instruction-record))
  (values-list
   (instruction-record-result-records instruction-record)))

(defmethod decode-record-definition ((_ (eql 'instruction-record)) expr)
  (destructuring-bind (name mnemonic result-record-names argument-record-names &rest rest) expr
    `(make-instance 'instruction-record
       :name ',name
       :vop ',(mksym (symbol-package name) "%" name)
       :mnemonic ',(find-symbol (string mnemonic) sb-assem::*backend-instruction-set-package*)
       :result-records (mapcar #'find-value-record ',result-record-names)
       :argument-records (mapcar #'find-value-record ',argument-record-names)
       ,@rest)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Vref Record
;;;
;;; A vref record describes either a load or store instruction.

(defclass vref-record (function-record)
  (;; Define aliases for inherited slots.
   (%name :reader vref-record-name)
   (%instruction-set :reader vref-record-instruction-set)
   ;; The name of the VOP that translates this instruction.
   (%vop
    :type non-nil-symbol
    :initarg :vop
    :initform (required-argument :vop)
    :reader vref-record-vop)
   ;; The mnemonic that is used within the VOP to emit this instruction.
   (%mnemonic
    :type symbol
    :initarg :mnemonic
    :initform (required-argument :mnemonic)
    :reader vref-record-mnemonic)
   ;; A value record, describing which kinds of objects are loaded or stored.
   (%value-record
    :type value-record
    :initarg :value-record
    :initform (required-argument :value-record)
    :reader vref-record-value-record)
   ;; A value record, describing the vector being read from or written to.
   (%vector-record
    :type value-record
    :initarg :vector-record
    :initform (required-argument :vector-record)
    :reader vref-record-vector-record)
   ;; The name of the n-dimensional accessor to be generated.
   (%aref
    :type non-nil-symbol
    :initarg :aref
    :initform (required-argument :aref)
    :reader vref-record-aref)
   ;; The name of the vector accessor to be generated.
   (%row-major-aref
    :type non-nil-symbol
    :initarg :row-major-aref
    :initform (required-argument :row-major-aref)
    :reader vref-record-row-major-aref)))

(defun vref-record-p (x)
  (typep x 'vref-record))

(defmethod function-record-return-values ((vref-record vref-record))
  (vref-record-value-record vref-record))

(defun decode-vref-record-definition (expr instance)
  (destructuring-bind (name mnemonic value-type vector-type aref row-major-aref &rest rest) expr
    `(make-instance ',instance
       :name ',name
       :vop ',(mksym (symbol-package name) "%" name)
       :mnemonic ',(find-symbol (string mnemonic) sb-assem::*backend-instruction-set-package*)
       :value-record (find-value-record ',value-type)
       :vector-record (find-value-record ',vector-type)
       :aref ',aref
       :row-major-aref ',row-major-aref
       ,@rest)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Load Record

(defclass load-record (vref-record)
  (;; Define aliases for inherited slots.
   (%name :reader load-record-name)
   (%instruction-set :reader load-record-instruction-set)
   (%vop :reader load-record-vop)
   (%mnemonic :reader load-record-mnemonic)
   (%value-record :reader load-record-value-record)
   (%vector-record :reader load-record-vector-record)
   (%aref :reader load-record-aref)
   (%row-major-aref :reader load-record-row-major-aref)))

(defun load-record-p (x)
  (typep x 'load-record))

(defmethod decode-record-definition ((_ (eql 'load-record)) expr)
  (decode-vref-record-definition expr 'load-record))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Store Record

(defclass store-record (vref-record)
  (;; Define aliases for inherited slots.
   (%name :reader store-record-name)
   (%instruction-set :reader store-record-instruction-set)
   (%vop :reader load-record-vop)
   (%mnemonic :reader load-record-mnemonic)
   (%value-record :reader store-record-value-record)
   (%vector-record :reader store-record-vector-record)
   (%aref :reader store-record-aref)
   (%row-major-aref :reader store-record-row-major-aref)))

(defun store-record-p (x)
  (typep x 'store-record))

(defmethod decode-record-definition ((_ (eql 'store-record)) expr)
  (decode-vref-record-definition expr 'store-record))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reffer Record

(defclass reffer-record (function-record)
  (;; Define aliases for inherited slots.
   (%name :reader reffer-record-name :initarg :aref :reader reffer-record-aref)
   (%instruction-set :reader reffer-record-instruction-set)
   ;; The corresponding row-major-aref's name.
   (%row-major-aref
    :type symbol
    :initarg :row-major-aref
    :initform (required-argument :row-major-aref)
    :reader reffer-record-row-major-aref)
   (%value-record
    :type symbol
    :initarg :value-record
    :initform (required-argument :value-record)
    :reader reffer-record-value-record)))

(defun reffer-record-p (x)
  (typep x 'reffer-record))

(defmethod function-record-return-values ((reffer-record reffer-record))
  (reffer-record-value-record reffer-record))

(defmethod decode-record-definition ((_ (eql 'reffer-record)) expr)
  (destructuring-bind (type aref row-major-aref &rest rest) expr
    `(make-instance 'reffer-record
       :aref ',aref
       :row-major-aref ',row-major-aref
       :value-record (find-value-record ',type)
       ,@rest)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Commutative Record

(defclass commutative-record (function-record)
  (;; Define aliases for inherited slots.
   (%name :reader commutative-record-name)
   (%instruction-set :reader commutative-record-instruction-set)
   ;; The binary operation used to combine the arguments.
   (%binary-operation
    :initarg :binary-operation
    :initform (required-argument :binary-operation)
    :reader commutative-record-binary-operation)
   ;; The identity for that operation, or NIL if there is none.
   (%identity-element
    :initarg :identity-element
    :initform (required-argument :identity-element)
    :reader commutative-record-identity-element)))

(defun commutative-record-p (x)
  (typep x 'commutative-record))

(defmethod function-record-return-values ((commutative-record commutative-record))
  (values
   (function-record-return-values
    (commutative-record-binary-operation commutative-record))))

(defmethod decode-record-definition ((_ (eql 'commutative-record)) expr)
  (destructuring-bind (name binary-operation identity-element &rest rest) expr
    `(make-instance 'commutative-record
       :name ',name
       :binary-operation (find-function-record ',binary-operation)
       ;; We can safely use NIL to denote the case where no identity
       ;; element is supplied, because our commutative functions operate on
       ;; numbers only.
       :identity-element ,identity-element
       ,@rest)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reducer Record

(defclass reducer-record (function-record)
  (;; Define aliases for inherited slots.
   (%name :reader reducer-record-name)
   (%instruction-set :reader reducer-record-instruction-set)
   ;; The binary operation used to reduce the arguments.
   (%binary-operation
    :initarg :binary-operation
    :initform (required-argument :binary-operation)
    :reader reducer-record-binary-operation)
   ;; The initial element for the reduction.
   (%initial-element
    :initarg :initial-element
    :initform (required-argument :initial-element)
    :reader reducer-record-initial-element)))

(defun reducer-record-p (x)
  (typep x 'reducer-record))

(defmethod function-record-return-values ((reducer-record reducer-record))
  (values
   (function-record-return-values
    (reducer-record-binary-operation reducer-record))))

(defmethod decode-record-definition ((_ (eql 'reducer-record)) expr)
  (destructuring-bind (name binary-operation initial-element &rest rest) expr
    `(make-instance 'reducer-record
       :name ',name
       :binary-operation (find-function-record ',binary-operation)
       :initial-element ,initial-element
       ,@rest)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Comparison Record

(defclass comparison-record (function-record)
  (;; Define aliases for inherited slots.
   (%name :reader comparison-record-name)
   (%instruction-set :reader comparison-record-instruction-set)
   ;; The binary comparison function.
   (%cmp
    :type instruction-record
    :initarg :cmp
    :initform (required-argument :cmp)
    :reader comparison-record-cmp)
   ;; The function for combining the results of some comparisons.
   (%and
    :type function-record
    :initarg :and
    :initform (required-argument :and)
    :reader comparison-record-and)
   ;; The truth value returned for an empty comparison.
   (%truth
    :initarg :truth
    :initform (required-argument :truth)
    :reader comparison-record-truth)))

(defun comparison-record-p (x)
  (typep x 'comparison-record))

(defmethod function-record-return-values ((comparison-record comparison-record))
  (values
   (function-record-return-values
    (comparison-record-and comparison-record))))

(defmethod decode-record-definition ((_ (eql 'comparison-record)) expr)
  (destructuring-bind (name cmp and truth &rest rest) expr
    `(make-instance 'comparison-record
       :name ',name
       :cmp (find-function-record ',cmp)
       :and (find-function-record ',and)
       :truth ,truth
       ,@rest)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Unequal Record

(defclass unequal-record (function-record)
  (;; Define aliases for inherited slots.
   (%name :reader unequal-record-name)
   (%instruction-set :reader unequal-record-instruction-set)
   ;; The binary unequal function.
   (%neq
    :type instruction-record
    :initarg :neq
    :initform (required-argument :neq)
    :reader unequal-record-neq)
   ;; The function for combining the results of some unequals.
   (%and
    :type function-record
    :initarg :and
    :initform (required-argument :and)
    :reader unequal-record-and)
   ;; The truth value returned for an empty unequal.
   (%truth
    :initarg :truth
    :initform (required-argument :truth)
    :reader unequal-record-truth)))

(defun unequal-record-p (x)
  (typep x 'unequal-record))

(defmethod function-record-return-values ((unequal-record unequal-record))
  (values
   (function-record-return-values
    (unequal-record-and unequal-record))))

(defmethod decode-record-definition ((_ (eql 'unequal-record)) expr)
  (destructuring-bind (name neq and truth &rest rest) expr
    `(make-instance 'unequal-record
       :name ',name
       :neq (find-function-record ',neq)
       :and (find-function-record ',and)
       :truth ,truth
       ,@rest)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; If Record

(defclass if-record (function-record)
  (;; Define aliases for inherited slots.
   (%name :reader if-record-name)
   (%instruction-set :reader if-record-instruction-set)
   ;; The blend instruction used to implement this function
   (%blend
    :type instruction-record
    :initarg :blend
    :initform (required-argument :blend)
    :reader if-record-blend)))

(defun if-record-p (x)
  (typep x 'if-record))

(defmethod function-record-return-values ((if-record if-record))
  (values
   (function-record-return-values
    (if-record-blend if-record))))

(defmethod decode-record-definition ((_ (eql 'if-record)) expr)
  (destructuring-bind (name blend &rest rest) expr
    `(make-instance 'if-record
       :name ',name
       :blend (find-function-record ',blend)
       ,@rest)))
