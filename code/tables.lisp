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
  (u1    1   (unsigned-byte  1)      (unsigned-byte  1)               sb-vm::unsigned-reg)
  (u2    2   (unsigned-byte  2)      (unsigned-byte  2)               sb-vm::unsigned-reg)
  (u4    4   (unsigned-byte  4)      (unsigned-byte  4)               sb-vm::unsigned-reg)
  (u8    8   (unsigned-byte  8)      (unsigned-byte  8)               sb-vm::unsigned-reg)
  (u16   16  (unsigned-byte 16)      (unsigned-byte 16)               sb-vm::unsigned-reg)
  (u32   32  (unsigned-byte 32)      (unsigned-byte 32)               sb-vm::unsigned-reg)
  (u64   64  (unsigned-byte 64)      (unsigned-byte 64)               sb-vm::unsigned-reg)
  (s8    8   (signed-byte  8)        (signed-byte  8)                 sb-vm::signed-reg)
  (s16   16  (signed-byte 16)        (signed-byte 16)                 sb-vm::signed-reg)
  (s32   32  (signed-byte 32)        (signed-byte 32)                 sb-vm::signed-reg)
  (s64   64  (signed-byte 64)        (signed-byte 64)                 sb-vm::signed-reg)
  (f32   32  single-float            single-float                     sb-vm::single-reg)
  (f64   64  double-float            double-float                     sb-vm::double-reg)
  (c64   64  (complex single-float)  sb-kernel::complex-single-float  sb-vm::complex-single-reg)
  (c128  128 (complex double-float)  sb-kernel::complex-double-float  sb-vm::complex-double-reg))

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

(defmacro define-instruction-record (name mnemonic result-records argument-records emitter &rest attributes)
  `(setf (gethash ',name *instruction-records*)
         (make-instruction-record
          :name ',name
          :mnemonic ',mnemonic
          :result-record-names ',result-records
          :argument-record-names ',argument-records
          :emitter ,emitter
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

(define-instruction-records +sse2+
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

(define-instruction-records +sse3+
  )

(define-instruction-records +ssse3+
  (f32.4-hdup            movshdup   (f32.4)  (f32.4)        #'default-emitter :cost 1)
  (f32.4-ldup            movsldup   (f32.4)  (f32.4)        #'default-emitter :cost 1)
  (f64.2-ddup            movddup    (f64.2)  (f64.2)        #'default-emitter :cost 1)
  )

(define-instruction-records +sse4.1+
  )

(define-instruction-records +sse4.2+
  )

(define-instruction-records +avx+
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

(define-instruction-records +avx2+
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
