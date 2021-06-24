(in-package #:sb-simd)

(defstruct (instruction-set)
  ;; The instruction set's name.
  (name nil :type non-nil-symbol :read-only t)
  ;; The package that holds the instruction set's symbols.
  (package nil :type package :read-only t)
  ;; A thunk, returning whether the instruction set is currently available.
  ;; Such a run time check is needed in the case where an executable is
  ;; created on one machine and run on another machine.  In that case, some
  ;; of the instructions sets available on the former might not be available
  ;; on the latter.
  (test nil :type function :read-only t))

(defun instruction-set-available-p (instruction-set)
  (funcall (instruction-set-test instruction-set)))

;;; The currently active instruction set.
(defvar *instruction-set*)

;;; Instruction Record

(defstruct (instruction-record
            (:constructor nil))
  (name nil :type non-nil-symbol :read-only t)
  (vop nil :type non-nil-symbol :read-only t)
  (mnemonic nil :type symbol :read-only t)
  (instruction-set *instruction-set* :type instruction-set :read-only t))

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
  (with-accessors ((name instruction-record-name)
                   (mnemonic instruction-record-mnemonic)
                   (instruction-set instruction-record-instruction-set))
      instruction-record
    (assert (eq (instruction-set-package instruction-set)
                (symbol-package name)))
    (assert (or mnemonic (not (instruction-set-available-p instruction-set))))
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
  (encoding :standard :type (member :standard :sse :none) :read-only t)
  ;; A keyword that, if provided, is included as the first argument to the
  ;; mnemonic.  Useful for comparison functions.
  (prefix nil :type (or null keyword) :read-only t))

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

;;; Defining Instruction Sets

(defun decode-options (options keyword decoder)
  (declare (list options) (keyword keyword) (function decoder))
  (loop for (key . entries) in options
        when (eq key keyword)
          append (mapcar decoder entries)))

(defmacro define-instruction-set (name &body options)
  `(let ((sb-ext:*evaluator-mode* :interpret))
     (eval
      '(let ((*instruction-set*
              (make-instruction-set
               :name ',name
               :package (find-package ,(concatenate 'string "SB-SIMD-" (string name)))
               :test (lambda () (and ,@(decode-options options :test #'identity))))))
        ,@(decode-options options :primitives #'decode-primitive)
        ,@(decode-options options :loads #'decode-load)
        ,@(decode-options options :stores #'decode-store)
        *instruction-set*))))
