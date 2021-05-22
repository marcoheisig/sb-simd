(in-package #:sb-simd)

;;; Most of this library is automatically generated from a set of tables.
;;; Each table is a hash table mapping from names to records.  This file
;;; defines the various kinds of records.

;; A variable that can be bound to false to mark all records created in a
;; snippet of code as unsupported.
(defvar *supported-p* t)

(defstruct (record
            (:copier nil)
            (:constructor nil)
            (:predicate record-p))
  "The abstract base class of all records."
  ;; The name of the entity described by this record.
  (name nil :type name :read-only t)
  ;; Whether the host supports the entity described by this record.
  (supported-p *supported-p* :type boolean :read-only t))

(defstruct (value-record
            (:include record)
            (:copier nil)
            (:predicate value-record-p))
  "The abstract base class for scalar and SIMD records."
  ;; The minimum number of bits that are necessary to represent this value
  ;; in memory.
  (bits nil :type (unsigned-byte 16) :read-only t)
  ;; The Common Lisp type of this value.
  (type nil :type type-specifier :read-only t)
  ;; The primitive type of this value as used by SBCL's VM.
  (primitive-type nil :type type-specifier :read-only t)
  ;; The name of the most specialized VM register that can hold this value.
  (register nil :type symbol))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Scalar Record
;;;
;;; Each scalar record is used to define a corresponding type of that name,
;;; and a function of that name that coerces a supplied object to that
;;; type.

(defstruct (scalar-record
            (:include value-record)
            (:copier nil)
            (:predicate scalar-record-p)
            (:constructor make-scalar-record
                (&key name bits type primitive-type register primitive-array-type
                 &aux (supported-p (and *supported-p* (sb-ext:valid-type-specifier-p type))))))
  ;; The primitive array type used by SBCL's VM to store such scalars.
  (primitive-array-type nil :type symbol :read-only t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SIMD Record
;;;
;;; Each SIMD record is used to define a corresponding type of that name, a
;;; packer for combining a number of suitable scalars to such a pack, and
;;; an unpacker that returns all elements of the pack as multiple values.

(defstruct (simd-record
            (:include value-record)
            (:copier nil)
            (:predicate simd-record-p)
            (:constructor make-simd-record
                (&key name type primitive-type register scalar-record-name size
                   packer primitive-packer unpacker primitive-unpacker
                 &aux
                   (scalar-record (find-value-record scalar-record-name))
                   (bits (* size (scalar-record-bits scalar-record)))
                   (supported-p (and *supported-p* (sb-ext:valid-type-specifier-p type))))))
  ;; The scalar record of the elements of this SIMD pack.
  (scalar-record nil :type scalar-record :read-only t)
  ;; The number of scalar elements in this SIMD pack.
  (size nil :type unsigned-byte :read-only t)
  (packer nil :type symbol :read-only t)
  (primitive-packer nil :type symbol :read-only t)
  (unpacker nil :type symbol :read-only t)
  (primitive-unpacker nil :type symbol :read-only t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction Record
;;;
;;; Each instruction record is used to generate a function/VOP pair such
;;; that the instruction can be used from regular Common Lisp code.

(bitfield:define-bitfield instruction-record-bits
  (cost (unsigned-byte 8) :initform 1)
  (foldable boolean :initform t)
  (flushable boolean :initform t)
  (unsafely-flushable boolean :initform t)
  (movable boolean :initform t)
  (commutative boolean :initform nil)
  (first-arg-stores-result boolean :initform nil))

(declaim (ftype (function (name) (values (or null value-record))) find-value-record))

(defun default-emitter (mnemonic &rest args)
  `(sb-assem:inst ,mnemonic ,@args))

(defun cmp-emitter (condition)
  (lambda (mnemonic &rest args)
    `(sb-assem:inst ,mnemonic ,condition ,@args)))

(defstruct (instruction-record
            (:include record)
            (:copier nil)
            (:predicate instruction-record-p)
            (:constructor make-instruction-record
                (&rest args
                 &key name mnemonic result-record-names argument-record-names emitter &allow-other-keys
                 &aux
                   (result-records (mapcar #'find-value-record result-record-names))
                   (argument-records (mapcar #'find-value-record argument-record-names))
                   (bits (apply #'make-instruction-record-bits :allow-other-keys t args))
                   (supported-p
                    (and *supported-p*
                         (find-symbol (string mnemonic) sb-assem::*backend-instruction-set-package*)
                         (every #'value-record-supported-p result-records)
                         (every #'value-record-supported-p argument-records))))))
  ;; The mnemonic of this instruction.
  (mnemonic nil :type symbol :read-only t)
  ;; A list of value records - one for each result.
  (result-records nil)
  ;; A list of value records - one for each argument.
  (argument-records nil)
  ;; A function that turns result symbols and argument symbols into an
  ;; instruction emitting form that can be used as the :GENERATOR argument
  ;; of a VOP.
  (emitter #'default-emitter :type function :read-only t)
  ;; Additional instruction properties, encoded as a bitfield.
  (bits nil :type instruction-record-bits :read-only t))

(defmacro define-instruction-bits-attribute (name)
  (flet ((reader-symbol (prefix)
           (intern (concatenate 'string (string prefix) (symbol-name name))
                   (symbol-package name))))
    `(define-inline ,(reader-symbol 'instruction-record-) (instruction-record)
       (,(reader-symbol 'instruction-record-bits-)
        (instruction-record-bits instruction-record)))))

(define-instruction-bits-attribute cost)
(define-instruction-bits-attribute foldable)
(define-instruction-bits-attribute flushable)
(define-instruction-bits-attribute unsafely-flushable)
(define-instruction-bits-attribute movable)
(define-instruction-bits-attribute commutative)
(define-instruction-bits-attribute first-arg-stores-result)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reffer Record
;;;
;;; Each reffer record defines a set of load and store instructions for a
;;; certain SIMD record or scalar record.

(defstruct
    (reffer-record
     (:include record)
     (:copier nil)
     (:predicate reffer-record-p)
     (:constructor make-reffer-record
         (&key name mnemonic non-temporal-mnemonic
          &aux
            (value-record (find-value-record name))
            (load (intern (concatenate 'string (string name) "-LOAD")))
            (store (intern (concatenate 'string (string name) "-STORE")))
            (ref (intern (concatenate 'string (string name) "REF")))
            (row-major-ref (intern (concatenate 'string "ROW-MAJOR-" (string name) "REF")))
            (non-temporal-load (intern (concatenate 'string "NON-TEMPORAL-" (string name) "-LOAD")))
            (non-temporal-store (intern (concatenate 'string "NON-TEMPORAL-" (string name) "-STORE")))
            (non-temporal-ref (intern (concatenate 'string "NON-TEMPORAL-" (string name) "REF")))
            (non-temporal-row-major-ref (intern (concatenate 'string "NON-TEMPORAL-ROW-MAJOR-" (string name) "REF")))
            (supported-p
             (and *supported-p*
                  (value-record-supported-p value-record))))))
  ;; The load/store mnemonic of this reffer.
  (mnemonic nil :type symbol :read-only t)
  ;; The non-temporal load/store mnemonic of this reffer.
  (non-temporal-mnemonic nil :type symbol :read-only t)
  ;; The value that is being loaded or stored by the reffer.
  (value-record nil :type value-record :read-only t)
  ;; The name of the VOP for loading a value.
  (load nil :type name :read-only t)
  ;; The name of the VOP for storing a value.
  (store nil :type name :read-only t)
  ;; The name of the instruction for storing a value.
  (ref nil :type name :read-only t)
  ;; The name of the instruction for storing a value using a row-major index.
  (row-major-ref nil :type name :read-only t)
  ;; The name of the VOP for loading a value using a non-temporal load.
  (non-temporal-load nil :type name :read-only t)
  ;; The name of the VOP for storing a value using a non-temporal store.
  (non-temporal-store nil :type name :read-only t)
  ;; The name of the instruction for loading a value using a non-temporal load.
  (non-temporal-ref nil :type name :read-only t)
  ;; The name of the instruction for loading a value using a non-temporal
  ;; load and a row-major index.
  (non-temporal-row-major-ref nil :type name :read-only t))
