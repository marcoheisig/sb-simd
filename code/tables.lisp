(in-package #:sb-simd)

;;; A hash table, mapping from value record names to value records.
(declaim (hash-table *value-records*))
(defparameter *value-records* (make-hash-table))

(defun find-value-record (name)
  (or (gethash name *value-records*)
      (error "There is no value record with the name ~S."
             name)))

(defun value-record-name-p (name)
  (nth-value 1 (gethash name *value-records*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Scalar Types

(defmacro define-scalar-record (name bits type primitive-type register)
  `(setf (gethash ',name *value-records*)
         (make-scalar-record
          :name ',name
          :bits ,bits
          :type ',type
          :primitive-type ',primitive-type
          :register ',register)))

(defmacro define-scalar-records (supported-p &body rows)
  `(let ((*supported-p* ,supported-p))
     ,@(loop for row in rows collect `(define-scalar-record ,@row))))

(define-scalar-records t
  (u1    1   (unsigned-byte  1)      sb-vm::unsigned-num              sb-vm::unsigned-reg)
  (u2    2   (unsigned-byte  2)      sb-vm::unsigned-num              sb-vm::unsigned-reg)
  (u4    4   (unsigned-byte  4)      sb-vm::unsigned-num              sb-vm::unsigned-reg)
  (u8    8   (unsigned-byte  8)      sb-vm::unsigned-num              sb-vm::unsigned-reg)
  (u16   16  (unsigned-byte 16)      sb-vm::unsigned-num              sb-vm::unsigned-reg)
  (u32   32  (unsigned-byte 32)      sb-vm::unsigned-num              sb-vm::unsigned-reg)
  (u64   64  (unsigned-byte 64)      sb-vm::unsigned-num              sb-vm::unsigned-reg)
  (s8    8   (signed-byte  8)        sb-vm::signed-num                sb-vm::signed-reg)
  (s16   16  (signed-byte 16)        sb-vm::signed-num                sb-vm::signed-reg)
  (s32   32  (signed-byte 32)        sb-vm::signed-num                sb-vm::signed-reg)
  (s64   64  (signed-byte 64)        sb-vm::signed-num                sb-vm::signed-reg)
  (f32   32  single-float            single-float                     sb-vm::single-reg)
  (f64   64  double-float            double-float                     sb-vm::double-reg)
  (c64   64  (complex single-float)  sb-vm::complex-single-float  sb-vm::complex-single-reg)
  (c128  128 (complex double-float)  sb-vm::complex-double-float  sb-vm::complex-double-reg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SIMD Types

(defmacro define-simd-record (name scalar-record-name size type primitive-type register packer primitive-packer unpacker primitive-unpacker)
  `(setf (gethash ',name *value-records*)
         (make-simd-record
          :name ',name
          :scalar-record-name ',scalar-record-name
          :size ',size
          :type ',type
          :primitive-type ',primitive-type
          :register ',register
          :packer ',packer
          :primitive-packer ',primitive-packer
          :unpacker ',unpacker
          :primitive-unpacker ',primitive-unpacker)))

(defmacro define-simd-records (supported-p &body rows)
  `(let ((*supported-p* ,supported-p))
     ,@(loop for row in rows collect `(define-simd-record ,@row))))

(define-simd-records +sse+
  (u32.4  u32  4  (sb-ext:simd-pack (unsigned-byte 32))      sb-kernel:simd-pack-int         sb-vm::int-sse-reg      make-u32.4  sb-ext:%make-simd-pack-ub32        u32.4-values  sb-ext:%simd-pack-ub32s)
  (u64.2  u64  2  (sb-ext:simd-pack (unsigned-byte 64))      sb-kernel:simd-pack-int         sb-vm::int-sse-reg      make-u64.2  sb-ext:%make-simd-pack-ub64        u64.2-values  sb-ext:%simd-pack-ub64s)
  (f32.4  f32  4  (sb-ext:simd-pack single-float)            sb-kernel:simd-pack-single      sb-vm::single-sse-reg   make-f32.4  sb-ext:%make-simd-pack-single      f32.4-values  sb-ext:%simd-pack-singles)
  (f64.2  f64  2  (sb-ext:simd-pack double-float)            sb-kernel:simd-pack-double      sb-vm::double-sse-reg   make-f64.2  sb-ext:%make-simd-pack-double      f64.2-values  sb-ext:%simd-pack-doubles))

(define-simd-records +avx+
  (u32.8  u32  8  (sb-ext:simd-pack-256 (unsigned-byte 32))  sb-kernel:simd-pack-256-int     sb-vm::int-avx2-reg     make-u32.8  sb-ext:%make-simd-pack-256-ub32    u32.8-values  sb-ext:%simd-pack-256-ub32s)
  (u64.4  u64  4  (sb-ext:simd-pack-256 (unsigned-byte 64))  sb-kernel:simd-pack-256-int     sb-vm::int-avx2-reg     make-u64.4  sb-ext:%make-simd-pack-256-ub64    u64.4-values  sb-ext:%simd-pack-256-ub64s)
  (f32.8  f32  8  (sb-ext:simd-pack-256 single-float)        sb-kernel:simd-pack-256-single  sb-vm::single-avx2-reg  make-f32.8  sb-ext:%make-simd-pack-256-single  f32.8-values  sb-ext:%simd-pack-256-singles)
  (f64.4  f64  4  (sb-ext:simd-pack-256 double-float)        sb-kernel:simd-pack-256-double  sb-vm::double-avx2-reg  make-f64.4  sb-ext:%make-simd-pack-256-double  f64.4-values  sb-ext:%simd-pack-256-doubles))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instructions

(declaim (hash-table *instruction-records*))
(defparameter *instruction-records* (make-hash-table :test #'eq))

(defun register-instruction-record (instruction-record)
  (let ((key (instruction-record-name instruction-record)))
    (symbol-macrolet ((value (gethash key *instruction-records*)))
      ;; Only overwrite existing instruction records of the same name if
      ;; the new one is also supported.
      (when (or (not value)
                (instruction-record-supported-p instruction-record))
        (setf value instruction-record)))))

(defmacro define-instruction-record (name mnemonic result-records argument-records &rest attributes)
  `(register-instruction-record
    (make-instruction-record
     :name ',name
     :mnemonic ',mnemonic
     :result-record-names ',result-records
     :argument-record-names ',argument-records
     ,@attributes)))

(defmacro define-instruction-records (supported-p &body rows)
  `(let ((*supported-p* ,supported-p))
     ,@(loop for row in rows collect `(define-instruction-record ,@row))))

(defun find-instruction-record (name)
  (or (gethash name *instruction-records*)
      (error "There is no instruction record with the name ~S."
             name)))

(defun instruction-record-name-p (name)
  (nth-value 1 (gethash name *instruction-records*)))

;; The order in which instructions are copied is important.  We start with
;; the SSE instructions and then move to AVX instructions.  This way, AVX
;; instructions of the same name shadow the SSE equivalents.  This, in
;; turn, minimizes the number of costly transitions from AVX to SSE.

(define-instruction-records +sse+
  ;; f32.4
  (two-arg-f32.4-and     andps      (f32.4)  (f32.4 f32.4) :cost 1 :first-arg-stores-result t :commutative t)
  (two-arg-f32.4-or      orps       (f32.4)  (f32.4 f32.4) :cost 1 :first-arg-stores-result t :commutative t)
  (two-arg-f32.4-xor     xorps      (f32.4)  (f32.4 f32.4) :cost 1 :first-arg-stores-result t :commutative t)
  (two-arg-f32.4-andnot  andnps     (f32.4)  (f32.4 f32.4) :cost 1 :first-arg-stores-result t)
  (two-arg-f32.4-max     maxps      (f32.4)  (f32.4 f32.4) :cost 3 :first-arg-stores-result t :commutative t)
  (two-arg-f32.4-min     minps      (f32.4)  (f32.4 f32.4) :cost 3 :first-arg-stores-result t :commutative t)
  (two-arg-f32.4+        addps      (f32.4)  (f32.4 f32.4) :cost 2 :first-arg-stores-result t :commutative t)
  (two-arg-f32.4-        subps      (f32.4)  (f32.4 f32.4) :cost 2 :first-arg-stores-result t)
  (two-arg-f32.4*        mulps      (f32.4)  (f32.4 f32.4) :cost 2 :first-arg-stores-result t :commutative t)
  (two-arg-f32.4/        divps      (f32.4)  (f32.4 f32.4) :cost 8 :first-arg-stores-result t)
  (two-arg-f32.4=        cmpps      (u32.4)  (f32.4 f32.4) :cost 4 :first-arg-stores-result t :emitter (cmp-emitter :eq) :commutative t)
  (two-arg-f32.4/=       cmpps      (u32.4)  (f32.4 f32.4) :cost 4 :first-arg-stores-result t :emitter (cmp-emitter :neq) :commutative t)
  (two-arg-f32.4<        cmpps      (u32.4)  (f32.4 f32.4) :cost 4 :first-arg-stores-result t :emitter (cmp-emitter :lt))
  (two-arg-f32.4<=       cmpps      (u32.4)  (f32.4 f32.4) :cost 4 :first-arg-stores-result t :emitter (cmp-emitter :le))
  (two-arg-f32.4>        cmpps      (u32.4)  (f32.4 f32.4) :cost 4 :first-arg-stores-result t :emitter (cmp-emitter :nle))
  (two-arg-f32.4>=       cmpps      (u32.4)  (f32.4 f32.4) :cost 4 :first-arg-stores-result t :emitter (cmp-emitter :nlt))
  (f32.4-reciprocal      rcpps      (f32.4)  (f32.4)       :cost 5)
  (f32.4-rsqrt           rsqrtps    (f32.4)  (f32.4)       :cost 5)
  (f32.4-sqrt            sqrtps     (f32.4)  (f32.4)       :cost 15)
  ;; u32.4
  (u32.4-from-f32.4      cvtps2pi   (u32.4)  (f32.4)       :cost 5))

(define-instruction-records +sse2+
  ;; f64.2
  (two-arg-f64.2-and     andpd      (f64.2)  (f64.2 f64.2) :cost 1 :first-arg-stores-result t :commutative t)
  (two-arg-f64.2-or      orpd       (f64.2)  (f64.2 f64.2) :cost 1 :first-arg-stores-result t :commutative t)
  (two-arg-f64.2-xor     xorpd      (f64.2)  (f64.2 f64.2) :cost 1 :first-arg-stores-result t :commutative t)
  (two-arg-f64.2-andnot  andnpd     (f64.2)  (f64.2 f64.2) :cost 1 :first-arg-stores-result t)
  (two-arg-f64.2-max     maxpd      (f64.2)  (f64.2 f64.2) :cost 3 :first-arg-stores-result t :commutative t)
  (two-arg-f64.2-min     minpd      (f64.2)  (f64.2 f64.2) :cost 3 :first-arg-stores-result t :commutative t)
  (two-arg-f64.2+        addpd      (f64.2)  (f64.2 f64.2) :cost 2 :first-arg-stores-result t :commutative t)
  (two-arg-f64.2-        subpd      (f64.2)  (f64.2 f64.2) :cost 2 :first-arg-stores-result t)
  (two-arg-f64.2*        mulpd      (f64.2)  (f64.2 f64.2) :cost 2 :first-arg-stores-result t :commutative t)
  (two-arg-f64.2/        divpd      (f64.2)  (f64.2 f64.2) :cost 8 :first-arg-stores-result t)
  (two-arg-f64.2=        cmppd      (u64.2)  (f64.2 f64.2) :cost 4 :first-arg-stores-result t :emitter (cmp-emitter :eq) :commutative t)
  (two-arg-f64.2/=       cmppd      (u64.2)  (f64.2 f64.2) :cost 4 :first-arg-stores-result t :emitter (cmp-emitter :neq) :commutative t)
  (two-arg-f64.2<        cmppd      (u64.2)  (f64.2 f64.2) :cost 4 :first-arg-stores-result t :emitter (cmp-emitter :lt))
  (two-arg-f64.2<=       cmppd      (u64.2)  (f64.2 f64.2) :cost 4 :first-arg-stores-result t :emitter (cmp-emitter :le))
  (two-arg-f64.2>        cmppd      (u64.2)  (f64.2 f64.2) :cost 4 :first-arg-stores-result t :emitter (cmp-emitter :nle))
  (two-arg-f64.2>=       cmppd      (u64.2)  (f64.2 f64.2) :cost 4 :first-arg-stores-result t :emitter (cmp-emitter :nlt))
  (f64.2-sqrt            sqrtpd     (f64.2)  (f64.2)       :cost 20)
  ;; u32.4
  (two-arg-u32.4-and     pand       (u32.4)  (u32.4 u32.4) :cost 1 :first-arg-stores-result t :commutative t)
  (two-arg-u32.4-or      por        (u32.4)  (u32.4 u32.4) :cost 1 :first-arg-stores-result t :commutative t)
  (two-arg-u32.4-xor     pxor       (u32.4)  (u32.4 u32.4) :cost 1 :first-arg-stores-result t :commutative t)
  (two-arg-u32.4-andnot  pandn      (u32.4)  (u32.4 u32.4) :cost 1 :first-arg-stores-result t)
  (two-arg-u32.4+        paddd      (u32.4)  (u32.4 u32.4) :cost 2 :first-arg-stores-result t :commutative t)
  (two-arg-u32.4-        psubd      (u32.4)  (u32.4 u32.4) :cost 2 :first-arg-stores-result t)
  (u32.4-shiftl          pslld      (u32.4)  (u32.4 u32.4) :cost 1 :first-arg-stores-result t)
  (u32.4-shiftr          psrld      (u32.4)  (u32.4 u32.4) :cost 1 :first-arg-stores-result t)
  ;; u64.2
  (two-arg-u64.2-and     pand       (u64.2)  (u64.2 u64.2) :cost 1 :first-arg-stores-result t :commutative t)
  (two-arg-u64.2-or      por        (u64.2)  (u64.2 u64.2) :cost 1 :first-arg-stores-result t :commutative t)
  (two-arg-u64.2-xor     pxor       (u64.2)  (u64.2 u64.2) :cost 1 :first-arg-stores-result t :commutative t)
  (two-arg-u64.2-andnot  pand       (u64.2)  (u64.2 u64.2) :cost 1 :first-arg-stores-result t)
  (two-arg-u64.2+        paddq      (u64.2)  (u64.2 u64.2) :cost 2 :first-arg-stores-result t :commutative t)
  (two-arg-u64.2-        psubq      (u64.2)  (u64.2 u64.2) :cost 2 :first-arg-stores-result t)
  (u64.2-shiftl          psllq      (u64.2)  (u64.2 u64.2) :cost 1 :first-arg-stores-result t)
  (u64.2-shiftr          psrlq      (u64.2)  (u64.2 u64.2) :cost 1 :first-arg-stores-result t))

(define-instruction-records +sse3+
  (f32.4-hdup            movshdup   (f32.4)  (f32.4)       :cost 1)
  (f32.4-ldup            movsldup   (f32.4)  (f32.4)       :cost 1)
  (f64.2-broadcast       movddup    (f64.2)  (f64.2)       :cost 1))

(define-instruction-records +ssse3+
  (u32.4-hadd            phaddd     (u32.4)  (u32.4 u32.4) :cost 3 :first-arg-stores-result t)
  (u32.4-hsub            phsubd     (u32.4)  (u32.4 u32.4) :cost 3 :first-arg-stores-result t))

(define-instruction-records +sse4.1+
  (two-arg-u32.4*        mullo      (u32.4)  (u32.4 u32.4) :cost 9 :first-arg-stores-result t :commutative t)
  (two-arg-u64.2=        pcmpeqq    (u64.2)  (u64.2 u64.2) :cost 1 :first-arg-stores-result t :commutative t))

(define-instruction-records +sse4.2+
  (two-arg-u64.2>        pcmpgtq    (u64.2)  (u64.2 u64.2) :cost 3 :first-arg-stores-result t))

(define-instruction-records +avx+
  ;; f32.4
  (f32.4-from-f64.4      vcvtpd2ps  (f32.4)  (f64.4)       :cost 5)
  (two-arg-f32.4-and     vandps     (f32.4)  (f32.4 f32.4) :cost 1 :commutative t)
  (two-arg-f32.4-or      vorps      (f32.4)  (f32.4 f32.4) :cost 1 :commutative t)
  (two-arg-f32.4-xor     vxorps     (f32.4)  (f32.4 f32.4) :cost 1 :commutative t)
  (two-arg-f32.4-andnot  vandnps    (f32.4)  (f32.4 f32.4) :cost 1)
  (two-arg-f32.4-max     vmaxps     (f32.4)  (f32.4 f32.4) :cost 3 :commutative t)
  (two-arg-f32.4-min     vminps     (f32.4)  (f32.4 f32.4) :cost 3 :commutative t)
  (two-arg-f32.4=        vcmpps     (u32.4)  (f32.4 f32.4) :cost 4 :emitter (cmp-emitter :eq) :commutative t)
  (two-arg-f32.4/=       vcmpps     (u32.4)  (f32.4 f32.4) :cost 4 :emitter (cmp-emitter :neq) :commutative t)
  (two-arg-f32.4<        vcmpps     (u32.4)  (f32.4 f32.4) :cost 4 :emitter (cmp-emitter :lt))
  (two-arg-f32.4<=       vcmpps     (u32.4)  (f32.4 f32.4) :cost 4 :emitter (cmp-emitter :le))
  (two-arg-f32.4>        vcmpps     (u32.4)  (f32.4 f32.4) :cost 4 :emitter (cmp-emitter :gt))
  (two-arg-f32.4>=       vcmpps     (u32.4)  (f32.4 f32.4) :cost 4 :emitter (cmp-emitter :ge))
  (f32.4-hadd            vhaddps    (f32.4)  (f32.8 f32.4) :cost 6)
  (f32.4-hsub            vhsubps    (f32.4)  (f32.8 f32.4) :cost 6)
  (f32.4-reciprocal      vrcpps     (f32.4)  (f32.4)       :cost 5)
  (f32.4-rsqrt           vrsqrt     (f32.4)  (f32.4)       :cost 5)
  (f32.4-sqrt            vsqrt      (f32.4)  (f32.4)       :cost 15)
  (f32.4-unpackhi        vunpckhps  (f32.4)  (f32.4 f32.4) :cost 1)
  (f32.4-unpacklo        vunpcklps  (f32.4)  (f32.4 f32.4) :cost 1)
  ;; f64.2
  (two-arg-f64.2-and     vandpd     (f64.2)  (f64.4 f64.2) :cost 1 :commutative t)
  (two-arg-f64.2-or      vorpd      (f64.2)  (f64.4 f64.2) :cost 1 :commutative t)
  (two-arg-f64.2-xor     vxorpd     (f64.2)  (f64.4 f64.2) :cost 1 :commutative t)
  (two-arg-f64.2-andnot  vandnpd    (f64.2)  (f64.4 f64.2) :cost 1)
  (two-arg-f64.2-max     vmaxpd     (f64.2)  (f64.4 f64.2) :cost 3 :commutative t)
  (two-arg-f64.2-min     vminpd     (f64.2)  (f64.4 f64.2) :cost 3 :commutative t)
  (two-arg-f64.2+        vaddpd     (f64.2)  (f64.2 f64.2) :cost 2 :commutative t)
  (two-arg-f64.2-        vsubpd     (f64.2)  (f64.2 f64.2) :cost 2)
  (two-arg-f64.2*        vmulpd     (f64.2)  (f64.2 f64.2) :cost 2 :commutative t)
  (two-arg-f64.2/        vdivpd     (f64.2)  (f64.2 f64.2) :cost 8)
  (two-arg-f64.2=        vcmppd     (u64.2)  (f64.2 f64.2) :cost 4 :emitter (cmp-emitter :eq) :commutative t)
  (two-arg-f64.2/=       vcmppd     (u64.2)  (f64.2 f64.2) :cost 4 :emitter (cmp-emitter :neq) :commutative t)
  (two-arg-f64.2<        vcmppd     (u64.2)  (f64.2 f64.2) :cost 4 :emitter (cmp-emitter :lt))
  (two-arg-f64.2<=       vcmppd     (u64.2)  (f64.2 f64.2) :cost 4 :emitter (cmp-emitter :le))
  (two-arg-f64.2>        vcmppd     (u64.2)  (f64.2 f64.2) :cost 4 :emitter (cmp-emitter :gt))
  (two-arg-f64.2>=       vcmppd     (u64.2)  (f64.2 f64.2) :cost 4 :emitter (cmp-emitter :ge))
  (f64.2-hadd            vhaddpd    (f64.2)  (f64.2 f64.2) :cost 6)
  (f64.2-hsub            vhsubpd    (f64.2)  (f64.2 f64.2) :cost 6)
  (f64.2-sqrt            vsqrtpd    (f64.2)  (f64.2)       :cost 20)
  (f64.2-unpackhi        vunpckhpd  (f64.2)  (f64.2 f64.2) :cost 1)
  (f64.2-unpacklo        vunpcklpd  (f64.2)  (f64.2 f64.2) :cost 1)
  ;; f32.8
  (f32.8-from-u32.8      vcvtdq2ps  (f32.8)  (u32.8)       :cost 5)
  (two-arg-f32.8-and     vandps     (f32.8)  (f32.8 f32.8) :cost 1 :commutative t)
  (two-arg-f32.8-or      vorps      (f32.8)  (f32.8 f32.8) :cost 1 :commutative t)
  (two-arg-f32.8-xor     vxorps     (f32.8)  (f32.8 f32.8) :cost 1 :commutative t)
  (two-arg-f32.8-andnot  vandnps    (f32.8)  (f32.8 f32.8) :cost 1)
  (two-arg-f32.8-max     vmaxps     (f32.8)  (f32.8 f32.8) :cost 3 :commutative t)
  (two-arg-f32.8-min     vminps     (f32.8)  (f32.8 f32.8) :cost 3 :commutative t)
  (two-arg-f32.8+        vaddps     (f32.8)  (f32.8 f32.8) :cost 2 :commutative t)
  (two-arg-f32.8-        vsubps     (f32.8)  (f32.8 f32.8) :cost 2)
  (two-arg-f32.8*        vmulps     (f32.8)  (f32.8 f32.8) :cost 2 :commutative t)
  (two-arg-f32.8/        vdivps     (f32.8)  (f32.8 f32.8) :cost 8)
  (two-arg-f32.8=        vcmpps     (u32.8)  (f32.8 f32.8) :cost 4 :emitter (cmp-emitter :eq) :commutative t)
  (two-arg-f32.8/=       vcmpps     (u32.8)  (f32.8 f32.8) :cost 4 :emitter (cmp-emitter :neq) :commutative t)
  (two-arg-f32.8<        vcmpps     (u32.8)  (f32.8 f32.8) :cost 4 :emitter (cmp-emitter :lt))
  (two-arg-f32.8<=       vcmpps     (u32.8)  (f32.8 f32.8) :cost 4 :emitter (cmp-emitter :le))
  (two-arg-f32.8>        vcmpps     (u32.8)  (f32.8 f32.8) :cost 4 :emitter (cmp-emitter :gt))
  (two-arg-f32.8>=       vcmpps     (u32.8)  (f32.8 f32.8) :cost 4 :emitter (cmp-emitter :ge))
  (f32.8-hadd            vhaddps    (f32.8)  (f32.8 f32.8) :cost 6)
  (f32.8-hsub            vhsubps    (f32.8)  (f32.8 f32.8) :cost 6)
  (f32.8-reciprocal      vrcpps     (f32.8)  (f32.8)       :cost 5)
  (f32.8-rsqrt           vrsqrt     (f32.8)  (f32.8)       :cost 5)
  (f32.8-sqrt            vsqrt      (f32.8)  (f32.8)       :cost 15)
  (f32.8-unpackhi        vunpckhps  (f32.8)  (f32.8 f32.8) :cost 1)
  (f32.8-unpacklo        vunpcklps  (f32.8)  (f32.8 f32.8) :cost 1)
  ;; f64.4
  (f64.4-from-f32.4      vcvtps2pd  (f64.4)  (f32.4)       :cost 5)
  (f64.4-from-u32.4      vcvtdq2pd  (f64.4)  (u32.4)       :cost 5)
  (two-arg-f64.4-and     vandpd     (f64.4)  (f64.4 f64.4) :cost 1 :commutative t)
  (two-arg-f64.4-or      vorpd      (f64.4)  (f64.4 f64.4) :cost 1 :commutative t)
  (two-arg-f64.4-xor     vxorpd     (f64.4)  (f64.4 f64.4) :cost 1 :commutative t)
  (two-arg-f64.4-andnot  vandnpd    (f64.4)  (f64.4 f64.4) :cost 1)
  (two-arg-f64.4-max     vmaxpd     (f64.4)  (f64.4 f64.4) :cost 3 :commutative t)
  (two-arg-f64.4-min     vminpd     (f64.4)  (f64.4 f64.4) :cost 3 :commutative t)
  (two-arg-f64.4+        vaddpd     (f64.4)  (f64.4 f64.4) :cost 2 :commutative t)
  (two-arg-f64.4-        vsubpd     (f64.4)  (f64.4 f64.4) :cost 2)
  (two-arg-f64.4*        vmulpd     (f64.4)  (f64.4 f64.4) :cost 2 :commutative t)
  (two-arg-f64.4/        vdivpd     (f64.4)  (f64.4 f64.4) :cost 8)
  (two-arg-f64.4=        vcmppd     (u64.4)  (f64.4 f64.4) :cost 4 :emitter (cmp-emitter :eq) :commutative t)
  (two-arg-f64.4/=       vcmppd     (u64.4)  (f64.4 f64.4) :cost 4 :emitter (cmp-emitter :neq) :commutative t)
  (two-arg-f64.4<        vcmppd     (u64.4)  (f64.4 f64.4) :cost 4 :emitter (cmp-emitter :lt))
  (two-arg-f64.4<=       vcmppd     (u64.4)  (f64.4 f64.4) :cost 4 :emitter (cmp-emitter :le))
  (two-arg-f64.4>        vcmppd     (u64.4)  (f64.4 f64.4) :cost 4 :emitter (cmp-emitter :gt))
  (two-arg-f64.4>=       vcmppd     (u64.4)  (f64.4 f64.4) :cost 4 :emitter (cmp-emitter :ge))
  (f64.4-hadd            vhaddpd    (f64.4)  (f64.4 f64.4) :cost 6)
  (f64.4-hsub            vhsubpd    (f64.4)  (f64.4 f64.4) :cost 6)
  (f64.4-sqrt            vsqrtpd    (f64.4)  (f64.4)       :cost 20)
  (f64.4-unpackhi        vunpckhpd  (f64.4)  (f64.4 f64.4) :cost 1)
  (f64.4-unpacklo        vunpcklpd  (f64.4)  (f64.4 f64.4) :cost 1)
  ;; u32.4
  (u32.4-from-f64.4      vcvpd2dq   (u32.4)  (f64.4)       :cost 6)
  (two-arg-u32.4-and     vpand      (u32.4)  (u32.4 u32.4) :cost 1 :commutative t)
  (two-arg-u32.4-or      vpor       (u32.4)  (u32.4 u32.4) :cost 1 :commutative t)
  (two-arg-u32.4-xor     vpxor      (u32.4)  (u32.4 u32.4) :cost 1 :commutative t)
  (two-arg-u32.4-andnot  vpandn     (u32.4)  (u32.4 u32.4) :cost 1)
  (two-arg-u32.4+        vpaddd     (u32.4)  (u32.4 u32.4) :cost 2 :commutative t)
  (two-arg-u32.4-        vpsubd     (u32.4)  (u32.4 u32.4) :cost 2)
  (u32.4-shiftl          vpslld     (u32.4)  (u32.4 u32.4) :cost 1)
  (u32.4-shiftr          vpsrld     (u32.4)  (u32.4 u32.4) :cost 1)
  ;; u64.2
  (two-arg-u64.2-and     vpand      (u64.2)  (u64.2 u64.2) :cost 1 :commutative t)
  (two-arg-u64.2-or      vpor       (u64.2)  (u64.2 u64.2) :cost 1 :commutative t)
  (two-arg-u64.2-xor     vpxor      (u64.2)  (u64.2 u64.2) :cost 1 :commutative t)
  (two-arg-u64.2-andnot  vpand      (u64.2)  (u64.2 u64.2) :cost 1)
  (two-arg-u64.2+        vpaddq     (u64.2)  (u64.2 u64.2) :cost 2 :commutative t)
  (two-arg-u64.2-        vpsubq     (u64.2)  (u64.2 u64.2) :cost 2)
  (u64.2-shiftl          vpsllq     (u64.2)  (u64.2 u64.2) :cost 1)
  (u64.2-shiftr          vpsrlq     (u64.2)  (u64.2 u64.2) :cost 1)
  ;; u32.8
  (u32.8-from-f32.8      vcvtps2dq  (u32.8)  (f32.8)       :cost 4)
  (two-arg-u32.8-and     vandps     (u32.8)  (u32.8 u32.8) :cost 1 :commutative t)
  (two-arg-u32.8-or      vorps      (u32.8)  (u32.8 u32.8) :cost 1 :commutative t)
  (two-arg-u32.8-xor     vxorps     (u32.8)  (u32.8 u32.8) :cost 1 :commutative t)
  (two-arg-u32.8-andnot  vandnps    (u32.8)  (u32.8 u32.8) :cost 1)
  ;; u64.4
  (two-arg-u64.4-and     vandpd     (u64.4)  (u64.4 u64.4) :cost 1 :commutative t)
  (two-arg-u64.4-or      vorpd      (u64.4)  (u64.4 u64.4) :cost 1 :commutative t)
  (two-arg-u64.4-xor     vxorpd     (u64.4)  (u64.4 u64.4) :cost 1 :commutative t)
  (two-arg-u64.4-andnot  vandnpd    (u64.4)  (u64.4 u64.4) :cost 1))

(define-instruction-records +avx2+
  ;; f32.4
  (f32.4-broadcast       vbroadcastss (f32.4)  (f32.4)       :cost 1)
  ;; f64.2
  (f64.2-broadcast       movddup      (f64.2)  (f64.2)       :cost 1)
  ;; f32.8
  (f32.8-broadcast       vbroadcastss (f32.8)  (f32.4)       :cost 1)
  ;; f64.4
  (f64.4-broadcast       vbroadcastpd (f64.4)  (f64.2)       :cost 1)
  ;; u32.4
  (two-arg-u64.4+        vpaddq       (u64.4)  (u64.4 u64.4) :cost 2 :commutative t)
  (two-arg-u64.4-        vpsubq       (u64.4)  (u64.4 u64.4) :cost 2)
  (two-arg-u64.4*        vpmuludq     (u64.4)  (u64.4 u64.4) :cost 2 :commutative t)
  (two-arg-u32.4=        vpcmpeqd     (u32.4)  (u32.4 u32.4) :cost 1 :commutative t)
  (two-arg-u32.4>        vpcmpgtd     (u32.4)  (u32.4 u32.4) :cost 1)
  (u32.4-shiftl          vpsllvd      (u32.4)  (u32.4 u32.4) :cost 1)
  (u32.4-shiftr          vpsrlvd      (u32.4)  (u32.4 u32.4) :cost 1)
  (u32.4-unpackhi        vpunpckhdq   (u32.4)  (u32.4 u32.4) :cost 1)
  (u32.4-unpacklo        vpunpckldq   (u32.4)  (u32.4 u32.4) :cost 1)
  (u32.4-broadcast       vpbroadcastd (u32.4)  (u32.4)       :cost 1)
  ;; u64.2
  (two-arg-u64.4+        vpaddq       (u64.2)  (u64.2 u64.2) :cost 1 :commutative t)
  (two-arg-u64.4-        vpsubq       (u64.2)  (u64.2 u64.2) :cost 1)
  (two-arg-u64.2=        vpcmpeqq     (u64.2)  (u64.2 u64.2) :cost 1 :commutative t)
  (two-arg-u64.2>        vpcmpgtq     (u64.2)  (u64.2 u64.2) :cost 1)
  (u64.2-shiftl          vpsllvq      (u64.2)  (u64.2 u64.2) :cost 1)
  (u64.2-shiftr          vpsrlvq      (u64.2)  (u64.2 u64.2) :cost 1)
  (u64.2-unpackhi        vpunpckhqdq  (u64.2)  (u64.2 u64.2) :cost 1)
  (u64.2-unpacklo        vpunpcklqdq  (u64.2)  (u64.2 u64.2) :cost 1)
  (u64.2-broadcast       vpbroadcastq (u64.2)  (u64.2)       :cost 1)
  ;; u32.8
  (two-arg-u32.8+        vpaddd       (u32.8)  (u32.8 u32.8) :cost 2 :commutative t)
  (two-arg-u32.8-        vpsubd       (u32.8)  (u32.8 u32.8) :cost 2)
  (two-arg-u32.8*        vpmulld      (u32.8)  (u32.8 u32.8) :cost 2 :commutative t)
  (two-arg-u32.8=        vpcmpeqd     (u32.8)  (u32.8 u32.8) :cost 1 :commutative t)
  (two-arg-u32.8>        vpcmpgtd     (u32.8)  (u32.8 u32.8) :cost 1)
  (u32.8-shiftl          vpsllvd      (u32.8)  (u32.8 u32.8) :cost 1)
  (u32.8-shiftr          vpsrlvd      (u32.8)  (u32.8 u32.8) :cost 1)
  (u32.8-unpackhi        vpunpckhdq   (u32.8)  (u32.8 u32.8) :cost 1)
  (u32.8-unpacklo        vpunpckldq   (u32.8)  (u32.8 u32.8) :cost 1)
  (u32.8-broadcast       vpbroadcastd (u32.8)  (u32.4)       :cost 1)
  ;; u64.4
  (two-arg-u64.4+        vpaddq       (u64.4)  (u64.4 u64.4) :cost 1 :commutative t)
  (two-arg-u64.4-        vpsubq       (u64.4)  (u64.4 u64.4) :cost 1)
  (two-arg-u64.4=        vpcmpeqq     (u64.4)  (u64.4 u64.4) :cost 1 :commutative t)
  (two-arg-u64.4>        vpcmpgtq     (u64.4)  (u64.4 u64.4) :cost 1)
  (u64.4-shiftl          vpsllvq      (u64.4)  (u64.4 u64.4) :cost 1)
  (u64.4-shiftr          vpsrlvq      (u64.4)  (u64.4 u64.4) :cost 1)
  (u64.4-unpackhi        vpunpckhqdq  (u64.4)  (u64.4 u64.4) :cost 1)
  (u64.4-unpacklo        vpunpcklqdq  (u64.4)  (u64.4 u64.4) :cost 1)
  (u64.4-broadcast       vpbroadcastq (u64.4)  (u64.2)       :cost 1))
