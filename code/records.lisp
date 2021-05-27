(in-package #:sb-simd)

;;; Most of this library is automatically generated from a set of tables.
;;; Each table is a hash table mapping from names to records.  This file
;;; defines the various kinds of records.

;; A variable that can be bound to false to mark all records created in a
;; snippet of code as unsupported.
(defvar *supported-p* t)

(defstruct (record
            (:copier nil)
            (:constructor nil))
  "The abstract base class of all records."
  ;; The name of the entity described by this record.
  (name nil :type name :read-only t)
  ;; Whether the host supports the entity described by this record.
  (supported-p *supported-p* :type boolean :read-only t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Value Record
;;;
;;; A value record describes a specialized set of Common Lisp objects.
;;; Each value record consists of a Common Lisp type specifier, a
;;; corresponding primitive type specifier used by SBCL's VM, the number of
;;; bits required to represent all such objects, and a register in which
;;; such objects can be stored.

(defstruct (value-record
            (:include record)
            (:copier nil)
            (:predicate value-record-p))
  ;; The Common Lisp type of this value.
  (type nil :type type-specifier :read-only t)
  ;; The primitive type of this value as used by SBCL's VM.
  (primitive-type nil :type type-specifier :read-only t)
  ;; The minimum number of bits that are necessary to represent this value
  ;; in memory.
  (bits nil :type (unsigned-byte 16) :read-only t)
  ;; The name of the most specialized VM register that can hold this value.
  (register nil :type symbol))

(defstruct (scalar-record
            (:include value-record)
            (:copier nil)
            (:constructor make-scalar-record
                (&key name bits type primitive-type register
                 &aux (supported-p (and *supported-p* (sb-ext:valid-type-specifier-p type)))))))

(defstruct (simd-record
            (:include value-record)
            (:copier nil)
            (:constructor make-simd-record
                (&key name type primitive-type register scalar-record size
                   packer primitive-packer unpacker primitive-unpacker
                 &aux
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

(defstruct (instruction-record
            (:include record)
            (:copier nil)
            (:predicate instruction-record-p)
            (:constructor make-instruction-record
                (&key name mnemonic result-records argument-records
                   cost commutative pure encoding prefix
                 &aux
                   (supported-p
                    (and *supported-p*
                         (find-symbol (string mnemonic) sb-assem::*backend-instruction-set-package*)
                         (every #'value-record-supported-p result-records)
                         (every #'value-record-supported-p argument-records))))))
  ;; The mnemonic of this instruction.
  (mnemonic (required-argument :mnemonic) :type symbol :read-only t)
  ;; A list of value records - one for each result.
  (result-records (required-argument :result-records) :type list :read-only t)
  ;; A list of value records - one for each argument.
  (argument-records (required-argument :argument-records) :type list :read-only t)
  ;; The cost of executing that instruction.  May be a rough estimate.
  (cost 1 :type unsigned-byte :read-only t)
  ;; Whether this instruction satisfies (INST a b) = (INST b a).
  (commutative nil :type boolean :read-only t)
  ;; Whether the instructions has side-effects.
  (pure t :type boolean :read-only t)
  ;; How the instruction is turned into a VOP and into a function.
  (encoding :standard :type (member :standard :sse :load :store) :read-only t)
  ;; A keyword that is included as the first argument to the mnemonic.
  (prefix nil :type (or null keyword) :read-only t))
