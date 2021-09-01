(in-package #:sb-simd-internals)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction Set

(defstruct (instruction-set)
  ;; The instruction set's name.
  (name nil :type keyword :read-only t)
  ;; The package that holds the instruction set's symbols.
  (package nil :type package :read-only t)
  ;; A thunk, returning whether the instruction set is currently available.
  ;; Such a run time check is needed in the case where an executable is
  ;; created on one machine and run on another machine.  In that case, some
  ;; of the instructions sets available on the former might not be available
  ;; on the latter.
  (test nil :type function :read-only t)
  ;; A list of instruction sets included by this one.
  (includes nil :type list))

(defun instruction-set-available-p (instruction-set)
  (funcall (instruction-set-test instruction-set)))

;;; A hash table, mapping from instruction set names or packages to
;;; instruction sets.
(defparameter *instruction-sets* (make-hash-table :test #'eq))

(defun find-instruction-set (designator)
  (or (gethash designator *instruction-sets*)
      (typecase designator
        (symbol (error "There is no instruction set with the name ~S." designator))
        (package (error "There is not instruction set with the package ~S" designator))
        (otherwise (error "Not a valid instruction set designator: ~S" designator)))))

(defun register-instruction-set (instruction-set)
  (setf (gethash (instruction-set-name instruction-set) *instruction-sets*)
        instruction-set)
  (setf (gethash (instruction-set-package instruction-set) *instruction-sets*)
        instruction-set)
  instruction-set)

;;; Returns a list containing the name of the supplied instruction set, and
;;; the names of all instruction sets that are directly or indirectly
;;; included by it.
(defun included-instruction-sets (instruction-set)
  (let ((result '()))
    (labels ((scan (instruction-set)
               (with-accessors ((name instruction-set-name)
                                (includes instruction-set-includes))
                   instruction-set
                 (unless (member name result)
                   (push name result)
                   (mapcar #'scan includes)))))
      (scan instruction-set)
      result)))

;;; The currently active instruction set.
(defvar *instruction-set*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Records
;;;
;;; Each instruction set defines a variety of data types and instructions.
;;; We represent each piece of information about such data types or
;;; instructions as a record.
;;;
;;; Records have one important invariant: The home package of the name of
;;; the record must be the same as the package of its instruction set.

(defstruct (record)
  ;; The record's name.
  (name nil :type non-nil-symbol :read-only t)
  ;; The record's instruction set.
  (instruction-set *instruction-set* :type instruction-set :read-only t))

(defun check-record (record)
  (with-accessors ((name record-name)
                   (instruction-set record-instruction-set)) record
    (unless (eq (instruction-set-package instruction-set)
                (symbol-package name))
      (error "Wrong home package ~S for ~S record ~S."
             (symbol-package name)
             (instruction-set-name instruction-set)
             name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Value Records
;;;
;;; A value record describes a specialized set of Common Lisp objects.
;;; Each value record consists of a Common Lisp type specifier, a
;;; corresponding primitive type specifier used by SBCL's VM, the number of
;;; bits required to represent all such objects, and a list of storage
;;; classes in which such objects can be stored.

(defstruct (value-record
            (:include record))
  ;; The Common Lisp type of this value.
  (type nil :type type-specifier :read-only t)
  ;; The primitive type of this value as used by SBCL's VM.
  (primitive-type nil :type type-specifier :read-only t)
  ;; The minimum number of bits that are necessary to represent this value
  ;; in memory.
  (bits nil :type (unsigned-byte 16) :read-only t)
  ;; A list of storage classes where this value can be placed.
  (scs nil :type list :read-only t))

;;; A hash table, mapping from value record names to value records.
(declaim (hash-table *value-records*))
(defparameter *value-records* (make-hash-table :test #'eq))

(defun find-value-record (name)
  (or (gethash name *value-records*)
      (error "There is no value record with the name ~S."
             name)))

(defun value-record-name-p (name)
  (nth-value 1 (gethash name *value-records*)))

(defun register-value-record (value-record)
  (check-record value-record)
  (with-accessors ((name value-record-name)) value-record
    (setf (gethash name *value-records*) value-record)))

;; Interns a string designator into the SB-VM package, while gracefully
;; handling the case where the symbol is not present.
(defun find-sc (sc)
  (or (find-symbol (string sc) "SB-VM")
      'sb-vm::descriptor-reg))

;; Interns a string designator into the SB-VM package, while also accepting
;; special compound primitive type designators.  The latter is mainly used
;; for primitive types like (:CONSTANT TYPE).
(defun find-primitive-type (x)
  (typecase x
    (symbol (find-symbol (string x) "SB-VM"))
    (cons x)
    (otherwise 't)))

;;; Scalar Record

(defstruct (scalar-record
            (:include value-record)
            (:copier nil)
            (:constructor make-scalar-record
                (&key name bits type primitive-type scs))))

(defun decode-scalar (entry)
  (destructuring-bind (name bits type primitive-type &optional (scs '(#:descriptor-reg))) entry
    `(register-value-record
      (make-scalar-record
       :name ',name
       :bits ,bits
       :type ',type
       :primitive-type ',(find-primitive-type primitive-type)
       :scs ',(mapcar #'find-sc scs)))))

;;; SIMD Record

(defstruct (simd-record
            (:include value-record)
            (:copier nil)
            (:constructor make-simd-record
                (&key name type primitive-type scs scalar-record size
                 &aux
                   (bits (* size (scalar-record-bits scalar-record))))))
  ;; The scalar record of the elements of this SIMD pack.
  (scalar-record nil :type scalar-record :read-only t)
  ;; The number of scalar elements in this SIMD pack.
  (size nil :type unsigned-byte :read-only t))

(defun decode-simd-pack (entry)
  (destructuring-bind (name scalar-record-name bits primitive-type scs) entry
    (let ((simd-pack-type
            (let ((base-type
                    (ecase bits
                      (128 (find-symbol "SIMD-PACK" "SB-EXT"))
                      (256 (find-symbol "SIMD-PACK-256" "SB-EXT")))))
              (cond ((not base-type) 't)
                    ((not scalar-record-name) base-type)
                    (t `(,base-type ,scalar-record-name))))))
      `(let ((.scalar-record. (find-value-record ',(or scalar-record-name (find-symbol "U64")))))
         (register-value-record
          (make-simd-record
           :name ',name
           :scalar-record .scalar-record.
           :size (the unsigned-byte (/ ,bits (scalar-record-bits .scalar-record.)))
           :type ',simd-pack-type
           :primitive-type ',(find-primitive-type primitive-type)
           :scs ',(mapcar #'find-sc scs)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction Records

(defstruct (instruction-record
            (:include record))
  (vop nil :type non-nil-symbol :read-only t)
  (mnemonic nil :type symbol :read-only t))

;;; A hash table, mapping from instruction names to instruction records.
(declaim (hash-table *instruction-records*))
(defparameter *instruction-records* (make-hash-table :test #'eq))

(defun find-instruction-record (name)
  (or (gethash name *instruction-records*)
      (error "There is no instruction with the name ~S."
             name)))

(defun filter-instruction-records (predicate)
  (loop for instruction-record being the hash-values of *instruction-records*
        when (funcall predicate instruction-record)
          collect instruction-record))

(defun filter-available-instruction-records (predicate)
  (filter-instruction-records
   (lambda (instruction-record)
     (and (instruction-set-available-p (instruction-record-instruction-set instruction-record))
          (funcall predicate instruction-record)))))

;; Ensure that each instruction record is registered in the hash table of
;; its instruction set and in the global instruction table.
(defun register-instruction-record (instruction-record)
  (check-record instruction-record)
  (with-accessors ((name instruction-record-name)) instruction-record
    (setf (gethash name *instruction-records*) instruction-record)))

;;; Primitive Record

(defstruct (primitive-record
            (:include instruction-record))
  ;; A list of value records - one for each result.
  (result-records nil :type list :read-only t)
  ;; A list of value records - one for each argument.
  (argument-records nil :type list :read-only t)
  ;; A rough estimate of the cost of executing that primitive.
  (cost 1 :type unsigned-byte :read-only t)
  ;; Whether this primitive satisfies (INST a b) = (INST b a).
  (commutative nil :type boolean :read-only t)
  ;; Whether this primitive is free of side-effects.
  (pure t :type boolean :read-only t)
  ;; How the primitive is turned into a VOP.
  (encoding :standard :type (member :standard :sse :sse+xmm0 :custom :none :move) :read-only t)
  ;; A constant that, if provided, is included verbatim as the first
  ;; argument to the mnemonic.  Useful for comparison functions.
  (prefix nil :type (or null keyword integer) :read-only t)
  ;; A constant that, if provided, is included verbatim as the last
  ;; argument to the mnemonic.  Useful for functions that take a certain
  ;; constant last argument, e.g., for shuffle or permute operations.
  (suffix nil :type (or null keyword integer) :read-only t))

(defun decode-primitive (entry)
  (destructuring-bind (name mnemonic result-record-names argument-record-names &rest rest) entry
    `(register-instruction-record
      (make-primitive-record
       :name ',name
       :vop ',(mksym (symbol-package name) "%" name)
       :mnemonic ',(find-symbol (string mnemonic) sb-assem::*backend-instruction-set-package*)
       :result-records (mapcar #'find-value-record ',result-record-names)
       :argument-records (mapcar #'find-value-record ',argument-record-names)
       ,@rest))))

(defun primitive-record-arity (primitive-record)
  (length (primitive-record-argument-records primitive-record)))

;;; Load and Store Record

(defstruct (vref-record
            (:include instruction-record))
  (value-record nil :type value-record :read-only t)
  (vector-record nil :type scalar-record :read-only t)
  (aref nil :type non-nil-symbol :read-only t)
  (row-major-aref nil :type non-nil-symbol :read-only t))

(defun decode-vref (entry constructor)
  (destructuring-bind (name mnemonic value-type vector-type aref row-major-aref) entry
    `(register-instruction-record
      (,constructor
       :name ',name
       :vop ',(mksym (symbol-package name) "%" name)
       :mnemonic ',(find-symbol (string mnemonic) sb-assem::*backend-instruction-set-package*)
       :value-record (find-value-record ',value-type)
       :vector-record (find-value-record ',vector-type)
       :aref ',aref
       :row-major-aref ',row-major-aref))))

(defstruct (load-record (:include vref-record)))

(defun decode-load (entry)
  (decode-vref entry 'make-load-record))

(defstruct (store-record (:include vref-record)))

(defun decode-store (entry)
  (decode-vref entry 'make-store-record))

(defun decode-include (entry)
  (check-type entry symbol)
  `(find-instruction-set ',entry))

;;; Defining Instruction Sets

(defun decode-options (options keyword decoder)
  (declare (list options) (keyword keyword) (function decoder))
  (loop for (key . entries) in options
        when (eq key keyword)
          append (mapcar decoder entries)))

(defparameter *instruction-set-options*
  '(:include :test :scalars :simd-packs :primitives :loads :stores))

(defmacro define-instruction-set (name &body options)
  ;; Ensure that only valid options are supplied.
  (dolist (option options)
    (unless (and (listp option)
                 (member (first option) *instruction-set-options*))
      (error "Not a valid instruction set option:~% ~S" option)))
  `(let ((sb-ext:*evaluator-mode* :interpret))
     (eval
      '(let ((*instruction-set*
              (make-instruction-set
               :name ',name
               :package (find-package ,(concatenate 'string "SB-SIMD-" (string name)))
               :test (lambda () (and ,@(decode-options options :test #'identity)))
               :includes (list ,@(decode-options options :include #'decode-include)))))
        ,@(decode-options options :scalars #'decode-scalar)
        ,@(decode-options options :simd-packs #'decode-simd-pack)
        ,@(decode-options options :primitives #'decode-primitive)
        ,@(decode-options options :loads #'decode-load)
        ,@(decode-options options :stores #'decode-store)
        (register-instruction-set *instruction-set*)))))
