(in-package #:sb-simd)

;;; Most of this library is automatically generated from a set of tables.
;;; This file contains these tables.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Value Records

;;; This is the base class for both scalar records and SIMD records.
(defstruct (value-record
            (:copier nil)
            (:predicate value-record-p))
  ;; The name of the scalar.  This library uses its own naming convention
  ;; in which all names are symbols.
  (name nil :type symbol :read-only t)
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
;;; Scalar Records

(defstruct (scalar-record
            (:include value-record)
            (:copier nil)
            (:predicate scalar-record-p)))

(defparameter *scalar-records*
  (loop
    for (name bits type primitive-type register)
      in '(( u1  1 (unsigned-byte  1) (unsigned-byte  1) sb-vm::unsigned-reg)
           ( u2  2 (unsigned-byte  2) (unsigned-byte  2) sb-vm::unsigned-reg)
           ( u4  4 (unsigned-byte  4) (unsigned-byte  4) sb-vm::unsigned-reg)
           ( u8  8 (unsigned-byte  8) (unsigned-byte  8) sb-vm::unsigned-reg)
           (u16 16 (unsigned-byte 16) (unsigned-byte 16) sb-vm::unsigned-reg)
           (u32 32 (unsigned-byte 32) (unsigned-byte 32) sb-vm::unsigned-reg)
           (u64 64 (unsigned-byte 64) (unsigned-byte 64) sb-vm::unsigned-reg)
           ( s8  8 (signed-byte  8) (signed-byte  8) sb-vm::signed-reg)
           (s16 16 (signed-byte 16) (signed-byte 16) sb-vm::signed-reg)
           (s32 32 (signed-byte 32) (signed-byte 32) sb-vm::signed-reg)
           (s64 64 (signed-byte 64) (signed-byte 64) sb-vm::signed-reg)
           (f32 32 single-float single-float sb-vm::single-reg)
           (f64 64 double-float double-float sb-vm::double-reg)
           (c64 64 (complex single-float) sb-kernel::complex-single-float sb-vm::complex-single-reg)
           (c128 128 (complex double-float) sb-kernel::complex-double-float sb-vm::complex-double-reg))
    collect
    (make-scalar-record
     :name name
     :bits bits
     :type type
     :primitive-type primitive-type
     :register register)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SIMD Records

(defstruct (simd-record
            (:include value-record)
            (:copier nil)
            (:predicate simd-record-p))
  ;; The scalar record that describes the individual elements of this SIMD
  ;; pack.
  (scalar-record nil :type scalar-record :read-only t)
  ;; The number of individual elements of this SIMD pack.
  (size nil :type unsigned-byte :read-only t)
  (packer nil :type symbol :read-only t)
  (primitive-packer nil :type symbol :read-only t)
  (unpacker nil :type symbol :read-only t)
  (primitive-unpacker nil :type symbol :read-only t))

(defparameter *simd-records*
  (loop
    for (name scalar-record-name size bits type primitive-type register packer primitive-packer unpacker primitive-unpacker)
      in '((u64.2 u64 2 128 (sb-ext:simd-pack (unsigned-byte 64)) sb-kernel:simd-pack-int sb-vm::int-sse-reg make-u64.2 sb-ext:%make-simd-pack-ub64 u64.2-values sb-ext:%simd-pack-ub64s)
           (f32.4 f32 4 128 (sb-ext:simd-pack single-float) sb-kernel:simd-pack-single sb-vm::single-sse-reg make-f32.4 sb-ext:%make-simd-pack-single f32.4-values sb-ext:%simd-pack-singles)
           (f64.2 f64 2 128 (sb-ext:simd-pack double-float) sb-kernel:simd-pack-double sb-vm::double-sse-reg make-f64.2 sb-ext:%make-simd-pack-double f64.2-values sb-ext:%simd-pack-doubles)
           (u64.4 u64 4 256 (sb-ext:simd-pack-256 (unsigned-byte 64)) sb-kernel:simd-pack-256-int sb-vm::int-avx2-reg make-u64.4 sb-ext:%make-simd-pack-256-ub64 u64.4-values sb-ext:%simd-pack-256-ub64s)
           (f32.8 f32 8 256 (sb-ext:simd-pack-256 single-float) sb-kernel:simd-pack-256-single sb-vm::single-avx2-reg make-f32.8 sb-ext:%make-simd-pack-256-single f32.8-values sb-ext:%simd-pack-256-singles)
           (f64.4 f64 4 256 (sb-ext:simd-pack-256 double-float) sb-kernel:simd-pack-256-double sb-vm::double-avx2-reg make-f64.4 sb-ext:%make-simd-pack-256-double f64.4-values sb-ext:%simd-pack-256-doubles))
    for scalar-record = (find scalar-record-name *scalar-records* :key #'scalar-record-name)
    when (sb-ext:valid-type-specifier-p type)
      collect
      (make-simd-record
       :name name
       :scalar-record scalar-record
       :size size
       :bits bits
       :type type
       :primitive-type primitive-type
       :register register
       :packer packer
       :primitive-packer primitive-packer
       :unpacker unpacker
       :primitive-unpacker primitive-unpacker)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Value Records

(defparameter *value-records*
  (append *scalar-records* *simd-records*))

(let ((table (make-hash-table)))
  (loop for value-record in *value-records* do
    (setf (gethash (value-record-name value-record) table)
          value-record))
  (defun find-value-record-by-name (name)
    (or (gethash name table)
        (error "There is no value record with the name ~S."
               name)))
  (defun value-record-name-p (name)
    (nth-value 1 (gethash name table))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction Records

(bitfield:define-bitfield instruction-record-bits
  (cost (unsigned-byte 8) :initform 1)
  (foldable boolean :initform t)
  (flushable boolean :initform t)
  (unsafely-flushable boolean :initform t)
  (movable boolean :initform t)
  (commutative boolean :initform nil)
  (first-arg-stores-result boolean :initform nil))

;; Each instruction record is used to generate a function/VOP pair such
;; that the instruction can be used from regular Common Lisp code.
(defstruct (instruction-record
            (:copier nil)
            (:predicate instruction-record-p))
  ;; The name of the instruction.
  (name nil :type symbol :read-only t)
  ;; The mnemonic of this instruction.
  (mnemonic nil :type symbol :read-only t)
  ;; A list of value records - one for each result.
  (result-records nil)
  ;; A list of value records - one for each argument.
  (argument-records nil)
  ;; Additional instruction properties, encoded as a bitfield.
  (bits nil :type instruction-record-bits :read-only t))

(define-inline instruction-record-cost (instruction-record)
  (instruction-record-bits-cost
   (instruction-record-bits instruction-record)))

(define-inline instruction-record-foldable (instruction-record)
  (instruction-record-bits-foldable
   (instruction-record-bits instruction-record)))

(define-inline instruction-record-flushable (instruction-record)
  (instruction-record-bits-flushable
   (instruction-record-bits instruction-record)))

(define-inline instruction-record-unsafely-flushable (instruction-record)
  (instruction-record-bits-unsafely-flushable
   (instruction-record-bits instruction-record)))

(define-inline instruction-record-movable (instruction-record)
  (instruction-record-bits-movable
   (instruction-record-bits instruction-record)))

(define-inline instruction-record-commutative (instruction-record)
  (instruction-record-bits-commutative
   (instruction-record-bits instruction-record)))

(define-inline instruction-record-first-arg-stores-result (instruction-record)
  (instruction-record-bits-first-arg-stores-result
   (instruction-record-bits instruction-record)))

(defparameter *instruction-records*
  (loop
    for (name mnemonic result-records argument-records . attributes)
      in '(;; Casts
           (f32.4-from-f64.4 vcvtpd2ps (f32.4) (f64.4) :cost 5)
           (f64.4-from-f32.4 vcvtps2pd (f64.4) (f32.4) :cost 5)
           ;; SSE arithmetic operations
           (f64.2-two-arg-+ addpd (f64.2) (f64.2 f64.2) :cost 2 :first-arg-stores-result t :commutative t)
           (f64.2-two-arg-- subpd (f64.2) (f64.2 f64.2) :cost 2 :first-arg-stores-result t)
           (f64.2-two-arg-* mulpd (f64.2) (f64.2 f64.2) :cost 2 :first-arg-stores-result t :commutative t)
           (f64.2-two-arg-/ divpd (f64.2) (f64.2 f64.2) :cost 8 :first-arg-stores-result t)
           (f32.4-two-arg-+ addps (f32.4) (f32.4 f32.4) :cost 2 :first-arg-stores-result t :commutative t)
           (f32.4-two-arg-- addps (f32.4) (f32.4 f32.4) :cost 2 :first-arg-stores-result t)
           (f32.4-two-arg-* addps (f32.4) (f32.4 f32.4) :cost 2 :first-arg-stores-result t :commutative t)
           (f32.4-two-arg-/ addps (f32.4) (f32.4 f32.4) :cost 8 :first-arg-stores-result t)
           ;; AVX2 arithmetic operations
           (f64.4-two-arg-+ vaddpd (f64.4) (f64.4 f64.4) :cost 2 :commutative t)
           (f64.4-two-arg-- vsubpd (f64.4) (f64.4 f64.4) :cost 2)
           (f64.4-two-arg-* vmulpd (f64.4) (f64.4 f64.4) :cost 2 :commutative t)
           (f64.4-two-arg-/ vdivpd (f64.4) (f64.4 f64.4) :cost 8)
           (f32.8-two-arg-+ vaddps (f32.8) (f32.8 f32.8) :cost 2 :commutative t)
           (f32.8-two-arg-- vaddps (f32.8) (f32.8 f32.8) :cost 2)
           (f32.8-two-arg-* vaddps (f32.8) (f32.8 f32.8) :cost 2 :commutative t)
           (f32.8-two-arg-/ vaddps (f32.8) (f32.8 f32.8) :cost 8))
    when (find-symbol (string mnemonic) sb-assem::*backend-instruction-set-package*)
      collect
      (make-instruction-record
       :name name
       :mnemonic mnemonic
       :result-records (mapcar #'find-value-record-by-name result-records)
       :argument-records (mapcar #'find-value-record-by-name argument-records)
       :bits (apply #'make-instruction-record-bits attributes))))

(let ((table (make-hash-table)))
  (loop for instruction-record in *instruction-records* do
    (setf (gethash (instruction-record-name instruction-record) table)
          instruction-record))
  (defun find-instruction-record-by-name (name)
    (or (gethash name table)
        (error "There is no instruction record with the name ~S."
               name)))
  (defun instruction-record-name-p (name)
    (nth-value 1 (gethash name table))))
