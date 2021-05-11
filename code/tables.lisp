(in-package #:sb-simd)

;;; Most of this library is automatically generated from a set of tables.
;;; This file contains these tables.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Value Types
;;;
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

;;; A hash table, mapping from value record names to value records.
(declaim (hash-table *value-records*))
(defparameter *value-records* (make-hash-table))

(define-condition no-such-value-record (error)
  ((%name :initarg :name :reader no-such-value-record-name))
  (:report (lambda (c s)
             (format s "There is no value record with the name ~S."
                     (no-such-value-record-name c)))))

(defun find-value-record-by-name (name)
  (or (gethash name *value-records*)
      (error 'no-such-value-record :name name)))

(defun value-record-name-p (name)
  (nth-value 1 (gethash name *value-records*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Scalar Types

(defstruct (scalar-record
            (:include value-record)
            (:copier nil)
            (:predicate scalar-record-p)))

(loop
  for (name bits type                   primitive-type                  register) in
  '((    u1    1 (unsigned-byte  1)     (unsigned-byte  1)              sb-vm::unsigned-reg)
    (    u2    2 (unsigned-byte  2)     (unsigned-byte  2)              sb-vm::unsigned-reg)
    (    u4    4 (unsigned-byte  4)     (unsigned-byte  4)              sb-vm::unsigned-reg)
    (    u8    8 (unsigned-byte  8)     (unsigned-byte  8)              sb-vm::unsigned-reg)
    (   u16   16 (unsigned-byte 16)     (unsigned-byte 16)              sb-vm::unsigned-reg)
    (   u32   32 (unsigned-byte 32)     (unsigned-byte 32)              sb-vm::unsigned-reg)
    (   u64   64 (unsigned-byte 64)     (unsigned-byte 64)              sb-vm::unsigned-reg)
    (    s8    8 (signed-byte  8)       (signed-byte  8)                sb-vm::signed-reg)
    (   s16   16 (signed-byte 16)       (signed-byte 16)                sb-vm::signed-reg)
    (   s32   32 (signed-byte 32)       (signed-byte 32)                sb-vm::signed-reg)
    (   s64   64 (signed-byte 64)       (signed-byte 64)                sb-vm::signed-reg)
    (   f32   32 single-float           single-float                    sb-vm::single-reg)
    (   f64   64 double-float           double-float                    sb-vm::double-reg)
    (   c64   64 (complex single-float) sb-kernel::complex-single-float sb-vm::complex-single-reg)
    (  c128  128 (complex double-float) sb-kernel::complex-double-float sb-vm::complex-double-reg))
  when (sb-ext:valid-type-specifier-p type)
    do (setf (gethash name *value-records*)
             (make-scalar-record
              :name name
              :bits bits
              :type type
              :primitive-type primitive-type
              :register register)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SIMD Types

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

(loop
  for (name  scalar-record-name size bits type                                      primitive-type                 register               packer     primitive-packer                  unpacker     primitive-unpacker) in
  '((  u32.4                u32    4  128 (sb-ext:simd-pack (unsigned-byte 32))     sb-kernel:simd-pack-int        sb-vm::int-sse-reg     make-u32.4 sb-ext:%make-simd-pack-ub32       u32.4-values sb-ext:%simd-pack-ub32s)
    (  u64.2                u64    2  128 (sb-ext:simd-pack (unsigned-byte 64))     sb-kernel:simd-pack-int        sb-vm::int-sse-reg     make-u64.2 sb-ext:%make-simd-pack-ub64       u64.2-values sb-ext:%simd-pack-ub64s)
    (  f32.4                f32    4  128 (sb-ext:simd-pack single-float)           sb-kernel:simd-pack-single     sb-vm::single-sse-reg  make-f32.4 sb-ext:%make-simd-pack-single     f32.4-values sb-ext:%simd-pack-singles)
    (  f64.2                f64    2  128 (sb-ext:simd-pack double-float)           sb-kernel:simd-pack-double     sb-vm::double-sse-reg  make-f64.2 sb-ext:%make-simd-pack-double     f64.2-values sb-ext:%simd-pack-doubles)
    (  u32.8                u32    8  256 (sb-ext:simd-pack-256 (unsigned-byte 32)) sb-kernel:simd-pack-256-int    sb-vm::int-avx2-reg    make-u32.8 sb-ext:%make-simd-pack-256-ub32   u32.8-values sb-ext:%simd-pack-256-ub32s)
    (  u64.4                u64    4  256 (sb-ext:simd-pack-256 (unsigned-byte 64)) sb-kernel:simd-pack-256-int    sb-vm::int-avx2-reg    make-u64.4 sb-ext:%make-simd-pack-256-ub64   u64.4-values sb-ext:%simd-pack-256-ub64s)
    (  f32.8                f32    8  256 (sb-ext:simd-pack-256 single-float)       sb-kernel:simd-pack-256-single sb-vm::single-avx2-reg make-f32.8 sb-ext:%make-simd-pack-256-single f32.8-values sb-ext:%simd-pack-256-singles)
    (  f64.4                f64    4  256 (sb-ext:simd-pack-256 double-float)       sb-kernel:simd-pack-256-double sb-vm::double-avx2-reg make-f64.4 sb-ext:%make-simd-pack-256-double f64.4-values sb-ext:%simd-pack-256-doubles))
  for scalar-record = (gethash scalar-record-name *value-records*)
  when (sb-ext:valid-type-specifier-p type)
    do (setf (gethash name *value-records*)
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
;;; Instructions

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
  ;; A function that turns result symbols and argument symbols into an
  ;; instruction emitting form that can be used as the :GENERATOR argument
  ;; of a VOP.
  (emitter nil :type function :read-only t)
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun default-emitter (mnemonic dst &rest args)
    `(sb-assem:inst ,mnemonic ,dst ,@args))

  (defun cmp-emitter (condition)
    (lambda (mnemonic dst a b)
      `(sb-assem:inst ,mnemonic ,condition ,dst ,a ,b))))

(defmacro define-instruction-table (table-name &body rows)
  `(progn
     (declaim (hash-table ,table-name))
     (defparameter ,table-name (make-hash-table :test #'eq))
     (macrolet ((define-instruction (name mnemonic result-records argument-records emitter &rest attributes)
                  (check-type name symbol)
                  (check-type mnemonic symbol)
                  (check-type result-records list)
                  (check-type argument-records list)
                  (assert (find-symbol (string mnemonic) sb-assem::*backend-instruction-set-package*))
                  `(setf (gethash ',name ,',table-name)
                         (make-instruction-record
                          :name ',name
                          :mnemonic ',mnemonic
                          :result-records (mapcar #'find-value-record-by-name ',result-records)
                          :argument-records (mapcar #'find-value-record-by-name ',argument-records)
                          :emitter ,emitter
                          :bits (make-instruction-record-bits ,@attributes)))))
       ,@(loop for row in rows collect `(define-instruction ,@row)))))

(define-instruction-table *sse-instructions*
  (two-arg-f32.4-and     andps      (f32.4)  (f32.4 f32.4)  #'default-emitter :cost 1 :first-arg-stores-result t :commutative t)
  (two-arg-f32.4-or      orps       (f32.4)  (f32.4 f32.4)  #'default-emitter :cost 1 :first-arg-stores-result t :commutative t)
  (two-arg-f32.4-xor     xorps      (f32.4)  (f32.4 f32.4)  #'default-emitter :cost 1 :first-arg-stores-result t :commutative t)
  (two-arg-f32.4-andnot  andnps     (f32.4)  (f32.4 f32.4)  #'default-emitter :cost 1 :first-arg-stores-result t)
  (two-arg-f32.4-max     maxps      (f32.4)  (f32.4 f32.4)  #'default-emitter :cost 3 :first-arg-stores-result t)
  (two-arg-f32.4-min     minps      (f32.4)  (f32.4 f32.4)  #'default-emitter :cost 3 :first-arg-stores-result t)
  (two-arg-f32.4+        addps      (f32.4)  (f32.4 f32.4)  #'default-emitter :cost 2 :first-arg-stores-result t :commutative t)
  (two-arg-f32.4-        subps      (f32.4)  (f32.4 f32.4)  #'default-emitter :cost 2 :first-arg-stores-result t)
  (two-arg-f32.4*        mulps      (f32.4)  (f32.4 f32.4)  #'default-emitter :cost 2 :first-arg-stores-result t :commutative t)
  (two-arg-f32.4/        divps      (f32.4)  (f32.4 f32.4)  #'default-emitter :cost 8 :first-arg-stores-result t)
  )

(define-instruction-table *sse2-instructions*
  (two-arg-f64.2-and     andpd      (f64.2)  (f64.2 f64.2)  #'default-emitter :cost 1 :first-arg-stores-result t :commutative t)
  (two-arg-f64.2-or      orpd       (f64.2)  (f64.2 f64.2)  #'default-emitter :cost 1 :first-arg-stores-result t :commutative t)
  (two-arg-f64.2-xor     xorpd      (f64.2)  (f64.2 f64.2)  #'default-emitter :cost 1 :first-arg-stores-result t :commutative t)
  (two-arg-f64.2-andnot  andnpd     (f64.2)  (f64.2 f64.2)  #'default-emitter :cost 1 :first-arg-stores-result t)
  (two-arg-f64.2-max     maxpd      (f64.2)  (f64.2 f64.2)  #'default-emitter :cost 3 :first-arg-stores-result t)
  (two-arg-f64.2-min     minpd      (f64.2)  (f64.2 f64.2)  #'default-emitter :cost 3 :first-arg-stores-result t)
  (two-arg-u64.2+        paddq      (u64.2)  (u64.2 u64.2)  #'default-emitter :cost 2 :first-arg-stores-result t :commutative t)
  (two-arg-u64.2-        psubq      (u64.2)  (u64.2 u64.2)  #'default-emitter :cost 2 :first-arg-stores-result t)
  (two-arg-u32.4+        paddd      (u32.4)  (u32.4 u32.4)  #'default-emitter :cost 2 :first-arg-stores-result t :commutative t)
  (two-arg-u32.4-        psubd      (u32.4)  (u32.4 u32.4)  #'default-emitter :cost 2 :first-arg-stores-result t)
  (two-arg-f64.2+        addpd      (f64.2)  (f64.2 f64.2)  #'default-emitter :cost 2 :first-arg-stores-result t :commutative t)
  (two-arg-f64.2-        subpd      (f64.2)  (f64.2 f64.2)  #'default-emitter :cost 2 :first-arg-stores-result t)
  (two-arg-f64.2*        mulpd      (f64.2)  (f64.2 f64.2)  #'default-emitter :cost 2 :first-arg-stores-result t :commutative t)
  (two-arg-f64.2/        divpd      (f64.2)  (f64.2 f64.2)  #'default-emitter :cost 8 :first-arg-stores-result t)
  )

(define-instruction-table *sse3-instructions*
  )

(define-instruction-table *ssse3-instructions*
  (f32.4-hdup            movshdup   (f32.4)  (f32.4)        #'default-emitter :cost 1)
  (f32.4-ldup            movsldup   (f32.4)  (f32.4)        #'default-emitter :cost 1)
  (f64.2-ddup            movddup    (f64.2)  (f64.2)        #'default-emitter :cost 1)
  )

(define-instruction-table *sse4.1-instructions*
  )

(define-instruction-table *sse4.2-instructions*
  )

(define-instruction-table *avx-instructions*
  (f32.4-from-f64.4      vcvtpd2ps  (f32.4)  (f64.4)        #'default-emitter :cost 5)
  (f64.4-from-f32.4      vcvtps2pd  (f64.4)  (f32.4)        #'default-emitter :cost 5)
  (two-arg-f64.4+        vaddpd     (f64.4)  (f64.4 f64.4)  #'default-emitter :cost 2 :commutative t)
  (two-arg-f64.4-        vsubpd     (f64.4)  (f64.4 f64.4)  #'default-emitter :cost 2)
  (two-arg-f64.4*        vmulpd     (f64.4)  (f64.4 f64.4)  #'default-emitter :cost 2 :commutative t)
  (two-arg-f64.4/        vdivpd     (f64.4)  (f64.4 f64.4)  #'default-emitter :cost 8)
  (two-arg-f32.8+        vaddps     (f32.8)  (f32.8 f32.8)  #'default-emitter :cost 2 :commutative t)
  (two-arg-f32.8-        vsubps     (f32.8)  (f32.8 f32.8)  #'default-emitter :cost 2)
  (two-arg-f32.8*        vmulps     (f32.8)  (f32.8 f32.8)  #'default-emitter :cost 2 :commutative t)
  (two-arg-f32.8/        vdivps     (f32.8)  (f32.8 f32.8)  #'default-emitter :cost 8)
  (two-arg-f32.8=        vcmpps     (u64.4)  (f32.8 f32.8)  (cmp-emitter :eq) :cost 4 :commutative t)
  (two-arg-f32.8<        vcmpps     (u64.4)  (f32.8 f32.8)  (cmp-emitter :lt) :cost 4)
  (two-arg-f32.8<=       vcmpps     (u64.4)  (f32.8 f32.8)  (cmp-emitter :le) :cost 4)
  (two-arg-f32.8/=       vcmpps     (u64.4)  (f32.8 f32.8)  (cmp-emitter :neq) :cost 4 :commutative t)
  (two-arg-f32.8>        vcmpps     (u64.4)  (f32.8 f32.8)  (cmp-emitter :gt) :cost 4)
  (two-arg-f32.8>=       vcmpps     (u64.4)  (f32.8 f32.8)  (cmp-emitter :ge) :cost 4)
  )

(define-instruction-table *avx2-instructions*
  (two-arg-u64.4+        vpaddq     (u64.4)  (u64.4 u64.4)  #'default-emitter :cost 2 :commutative t)
  (two-arg-u64.4-        vpsubq     (u64.4)  (u64.4 u64.4)  #'default-emitter :cost 2)
  (two-arg-u64.4*        vpmuludq   (u64.4)  (u64.4 u64.4)  #'default-emitter :cost 2 :commutative t)
  (two-arg-u32.8+        vpaddd     (u32.8)  (u32.8 u32.8)  #'default-emitter :cost 2 :commutative t)
  (two-arg-u32.8-        vpsubd     (u32.8)  (u32.8 u32.8)  #'default-emitter :cost 2)
  (two-arg-u32.8*        vpmulld    (u32.8)  (u32.8 u32.8)  #'default-emitter :cost 2 :commutative t)
  (two-arg-f32.4+        vaddps     (f32.4)  (f32.4 f32.4)  #'default-emitter :cost 2 :commutative t)
  (two-arg-f32.4-        vsubps     (f32.4)  (f32.4 f32.4)  #'default-emitter :cost 2)
  (two-arg-f32.4*        vmulps     (f32.4)  (f32.4 f32.4)  #'default-emitter :cost 2 :commutative t)
  (two-arg-f32.4/        vdivps     (f32.4)  (f32.4 f32.4)  #'default-emitter :cost 8)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Instruction Table

(declaim (hash-table *instructions*))
(defparameter *instructions* (make-hash-table))

(defun find-instruction-record-by-name (name)
  (or (gethash name *instructions*)
      (error "There is no instruction record with the name ~S."
             name)))

(defun instruction-record-name-p (name)
  (nth-value 1 (gethash name *instructions*)))

(flet ((copy-instructions (table)
         (maphash
          (lambda (key record)
            (setf (gethash key *instructions*) record))
          table)))
  ;; The order in which instructions are copied is important.  We start
  ;; with the SSE instructions and then move to AVX instructions.  This
  ;; way, AVX instructions of the same name shadow the SSE equivalents, and
  ;; users of this library avoid costly transitions from AVX to SSE.
  (when +sse+
    (copy-instructions *sse-instructions*))
  (when +sse2+
    (copy-instructions *sse2-instructions*))
  (when +sse3+
    (copy-instructions *sse3-instructions*))
  (when +ssse3+
    (copy-instructions *ssse3-instructions*))
  (when +sse4.1+
    (copy-instructions *sse4.1-instructions*))
  (when +sse4.2+
    (copy-instructions *sse4.2-instructions*))
  (when +avx+
    (copy-instructions *avx-instructions*))
  (when +avx2+
    (copy-instructions *avx2-instructions*)))
