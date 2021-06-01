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

(defmacro define-scalar-record
    (name bits type primitive-type &optional (register 'sb-vm::descriptor-reg))
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
  (index   64  index                                     sb-vm::tagged-num            sb-vm::any-reg)
  (u1      1   (unsigned-byte  1)                        sb-vm::unsigned-num          sb-vm::unsigned-reg)
  (u2      2   (unsigned-byte  2)                        sb-vm::unsigned-num          sb-vm::unsigned-reg)
  (u4      4   (unsigned-byte  4)                        sb-vm::unsigned-num          sb-vm::unsigned-reg)
  (u8      8   (unsigned-byte  8)                        sb-vm::unsigned-num          sb-vm::unsigned-reg)
  (u16     16  (unsigned-byte 16)                        sb-vm::unsigned-num          sb-vm::unsigned-reg)
  (u32     32  (unsigned-byte 32)                        sb-vm::unsigned-num          sb-vm::unsigned-reg)
  (u64     64  (unsigned-byte 64)                        sb-vm::unsigned-num          sb-vm::unsigned-reg)
  (s8      8   (signed-byte  8)                          sb-vm::signed-num            sb-vm::signed-reg)
  (s16     16  (signed-byte 16)                          sb-vm::signed-num            sb-vm::signed-reg)
  (s32     32  (signed-byte 32)                          sb-vm::signed-num            sb-vm::signed-reg)
  (s64     64  (signed-byte 64)                          sb-vm::signed-num            sb-vm::signed-reg)
  (f32     32  single-float                              sb-vm::single-float          sb-vm::single-reg)
  (f64     64  double-float                              sb-vm::double-float          sb-vm::double-reg)
  (c64     64  (complex single-float)                    sb-vm::complex-single-float  sb-vm::complex-single-reg)
  (c128    128 (complex double-float)                    sb-vm::complex-double-float  sb-vm::complex-double-reg)
  (u1vec   64  (simple-array (unsigned-byte 1) (*))      sb-vm::simple-bit-array)
  (u2vec   64  (simple-array (unsigned-byte 2) (*))      sb-vm::simple-array-unsigned-byte-2)
  (u4vec   64  (simple-array (unsigned-byte 4) (*))      sb-vm::simple-array-unsigned-byte-4)
  (u8vec   64  (simple-array (unsigned-byte 8) (*))      sb-vm::simple-array-unsigned-byte-8)
  (u16vec  64  (simple-array (unsigned-byte 16) (*))     sb-vm::simple-array-unsigned-byte-16)
  (u32vec  64  (simple-array (unsigned-byte 32) (*))     sb-vm::simple-array-unsigned-byte-32)
  (u64vec  64  (simple-array (unsigned-byte 64) (*))     sb-vm::simple-array-unsigned-byte-64)
  (s8vec   64  (simple-array (signed-byte 8) (*))        sb-vm::simple-array-signed-byte-8)
  (s16vec  64  (simple-array (signed-byte 16) (*))       sb-vm::simple-array-signed-byte-16)
  (s32vec  64  (simple-array (signed-byte 32) (*))       sb-vm::simple-array-signed-byte-32)
  (s64vec  64  (simple-array (signed-byte 64) (*))       sb-vm::simple-array-signed-byte-64)
  (f32vec  64  (simple-array single-float (*))           sb-vm::simple-array-single-float)
  (f64vec  64  (simple-array double-float (*))           sb-vm::simple-array-double-float)
  (c64vec  64  (simple-array (complex single-float) (*)) sb-vm::simple-array-complex-single-float)
  (c128vec 64  (simple-array (complex double-float) (*)) sb-vm::simple-array-complex-double-float))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SIMD Types

(defmacro define-simd-record (name scalar-record-name size type primitive-type register packer primitive-packer unpacker primitive-unpacker)
  `(setf (gethash ',name *value-records*)
         (make-simd-record
          :name ',name
          :scalar-record (find-value-record ',scalar-record-name)
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
  (u8.16  u8  16  (sb-ext:simd-pack (unsigned-byte 8))       sb-kernel:simd-pack-int         sb-vm::int-sse-reg      make-u8.16  sb-vm::%make-simd-pack-ub8         u8.16-values  sb-vm::%simd-pack-ub8s)
  (u16.8  u16  8  (sb-ext:simd-pack (unsigned-byte 16))      sb-kernel:simd-pack-int         sb-vm::int-sse-reg      make-u16.8  sb-vm::%make-simd-pack-ub16        u16.8-values  sb-vm::%simd-pack-ub16s)
  (u32.4  u32  4  (sb-ext:simd-pack (unsigned-byte 32))      sb-kernel:simd-pack-int         sb-vm::int-sse-reg      make-u32.4  sb-ext:%make-simd-pack-ub32        u32.4-values  sb-ext:%simd-pack-ub32s)
  (u64.2  u64  2  (sb-ext:simd-pack (unsigned-byte 64))      sb-kernel:simd-pack-int         sb-vm::int-sse-reg      make-u64.2  sb-ext:%make-simd-pack-ub64        u64.2-values  sb-ext:%simd-pack-ub64s)
  (s8.16  s8  16  (sb-ext:simd-pack (signed-byte 8))         sb-kernel:simd-pack-int         sb-vm::int-sse-reg      make-s8.16  sb-vm::%make-simd-pack-sb8         s8.16-values  sb-vm::%simd-pack-sb8s)
  (s16.8  s16  8  (sb-ext:simd-pack (signed-byte 16))        sb-kernel:simd-pack-int         sb-vm::int-sse-reg      make-s16.8  sb-vm::%make-simd-pack-sb16        s16.8-values  sb-vm::%simd-pack-sb16s)
  (s32.4  s32  4  (sb-ext:simd-pack (signed-byte 32))        sb-kernel:simd-pack-int         sb-vm::int-sse-reg      make-s32.4  sb-vm::%make-simd-pack-sb32        s32.4-values  sb-vm::%simd-pack-sb32s)
  (s64.2  s64  2  (sb-ext:simd-pack (signed-byte 64))        sb-kernel:simd-pack-int         sb-vm::int-sse-reg      make-s64.2  sb-vm::%make-simd-pack-sb64        s64.2-values  sb-vm::%simd-pack-sb64s)
  (f32.4  f32  4  (sb-ext:simd-pack single-float)            sb-kernel:simd-pack-single      sb-vm::single-sse-reg   make-f32.4  sb-ext:%make-simd-pack-single      f32.4-values  sb-ext:%simd-pack-singles)
  (f64.2  f64  2  (sb-ext:simd-pack double-float)            sb-kernel:simd-pack-double      sb-vm::double-sse-reg   make-f64.2  sb-ext:%make-simd-pack-double      f64.2-values  sb-ext:%simd-pack-doubles))

(define-simd-records +avx+
  (u32.8  u32  8  (sb-ext:simd-pack-256 (unsigned-byte 32))  sb-kernel:simd-pack-256-int     sb-vm::int-avx2-reg     make-u32.8  sb-ext:%make-simd-pack-256-ub32    u32.8-values  sb-ext:%simd-pack-256-ub32s)
  (u64.4  u64  4  (sb-ext:simd-pack-256 (unsigned-byte 64))  sb-kernel:simd-pack-256-int     sb-vm::int-avx2-reg     make-u64.4  sb-ext:%make-simd-pack-256-ub64    u64.4-values  sb-ext:%simd-pack-256-ub64s)
  ;(s8.32  s8  32  (sb-ext:simd-pack-256 (signed-byte 8))     sb-kernel:simd-pack-256-int     sb-vm::int-avx2-reg     make-s8.32  sb-vm::%make-simd-pack-256-sb8     s8.32-values  sb-vm::%simd-pack-256-sb8s)
  (s16.16 s16 16  (sb-ext:simd-pack-256 (signed-byte 16))    sb-kernel:simd-pack-256-int     sb-vm::int-avx2-reg     make-s16.16 sb-vm::%make-simd-pack-256-sb16    s16.16-values sb-vm::%simd-pack-256-sb16s)
  (s32.8  s32  8  (sb-ext:simd-pack-256 (signed-byte 32))    sb-kernel:simd-pack-256-int     sb-vm::int-avx2-reg     make-s32.8  sb-vm::%make-simd-pack-256-sb32    s32.8-values  sb-vm::%simd-pack-256-sb32s)
  (s64.4  s64  4  (sb-ext:simd-pack-256 (signed-byte 64))    sb-kernel:simd-pack-256-int     sb-vm::int-avx2-reg     make-s64.4  sb-vm::%make-simd-pack-256-sb64    s64.4-values  sb-vm::%simd-pack-256-sb64s)
  (f32.8  f32  8  (sb-ext:simd-pack-256 single-float)        sb-kernel:simd-pack-256-single  sb-vm::single-avx2-reg  make-f32.8  sb-ext:%make-simd-pack-256-single  f32.8-values  sb-ext:%simd-pack-256-singles)
  (f64.4  f64  4  (sb-ext:simd-pack-256 double-float)        sb-kernel:simd-pack-256-double  sb-vm::double-avx2-reg  make-f64.4  sb-ext:%make-simd-pack-256-double  f64.4-values  sb-ext:%simd-pack-256-doubles))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instructions

(declaim (hash-table *instruction-records*))
(defparameter *instruction-records* (make-hash-table))

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
     :result-records (mapcar #'find-value-record ',result-records)
     :argument-records (mapcar #'find-value-record ',argument-records)
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
;; older instruction sets and proceed to the newer ones.  The result is
;; that if multiple instruction sets define an instruction of the same
;; name, the one from the newer instruction set takes precedence.  The most
;; important practical consequence is that SSE instructions are shadowed by
;; their AVX equivalents, which avoids costly context switches.  Also, some
;; AVX instruction can be shadowed by more specific AVX2 instructions.

(define-instruction-records +sse+
  ;; f32.4
  (two-arg-f32.4-and     andps      (f32.4)  (f32.4 f32.4)  :cost 1 :encoding :sse :commutative t)
  (two-arg-f32.4-or      orps       (f32.4)  (f32.4 f32.4)  :cost 1 :encoding :sse :commutative t)
  (two-arg-f32.4-xor     xorps      (f32.4)  (f32.4 f32.4)  :cost 1 :encoding :sse :commutative t)
  (two-arg-f32.4-andnot  andnps     (f32.4)  (f32.4 f32.4)  :cost 1 :encoding :sse)
  (two-arg-f32.4-max     maxps      (f32.4)  (f32.4 f32.4)  :cost 3 :encoding :sse :commutative t)
  (two-arg-f32.4-min     minps      (f32.4)  (f32.4 f32.4)  :cost 3 :encoding :sse :commutative t)
  (two-arg-f32.4+        addps      (f32.4)  (f32.4 f32.4)  :cost 2 :encoding :sse :commutative t)
  (two-arg-f32.4-        subps      (f32.4)  (f32.4 f32.4)  :cost 2 :encoding :sse)
  (two-arg-f32.4*        mulps      (f32.4)  (f32.4 f32.4)  :cost 2 :encoding :sse :commutative t)
  (two-arg-f32.4/        divps      (f32.4)  (f32.4 f32.4)  :cost 8 :encoding :sse)
  (two-arg-f32.4=        cmpps      (u32.4)  (f32.4 f32.4)  :cost 4 :encoding :sse :prefix :eq :commutative t)
  (two-arg-f32.4/=       cmpps      (u32.4)  (f32.4 f32.4)  :cost 4 :encoding :sse :prefix :neq :commutative t)
  (two-arg-f32.4<        cmpps      (u32.4)  (f32.4 f32.4)  :cost 4 :encoding :sse :prefix :lt)
  (two-arg-f32.4<=       cmpps      (u32.4)  (f32.4 f32.4)  :cost 4 :encoding :sse :prefix :le)
  (two-arg-f32.4>        cmpps      (u32.4)  (f32.4 f32.4)  :cost 4 :encoding :sse :prefix :nle)
  (two-arg-f32.4>=       cmpps      (u32.4)  (f32.4 f32.4)  :cost 4 :encoding :sse :prefix :nlt)
  (f32.4-reciprocal      rcpps      (f32.4)  (f32.4)        :cost 5)
  (f32.4-rsqrt           rsqrtps    (f32.4)  (f32.4)        :cost 5)
  (f32.4-sqrt            sqrtps     (f32.4)  (f32.4)        :cost 15)
  ;; u32.4
  (u32.4-from-f32.4      cvtps2pi   (u32.4)  (f32.4)        :cost 5))

(define-instruction-records +sse2+
  ;; f64.2
  (two-arg-f64.2-and     andpd      (f64.2)  (f64.2 f64.2)  :cost 1 :encoding :sse :commutative t)
  (two-arg-f64.2-or      orpd       (f64.2)  (f64.2 f64.2)  :cost 1 :encoding :sse :commutative t)
  (two-arg-f64.2-xor     xorpd      (f64.2)  (f64.2 f64.2)  :cost 1 :encoding :sse :commutative t)
  (two-arg-f64.2-andnot  andnpd     (f64.2)  (f64.2 f64.2)  :cost 1 :encoding :sse)
  (two-arg-f64.2-max     maxpd      (f64.2)  (f64.2 f64.2)  :cost 3 :encoding :sse :commutative t)
  (two-arg-f64.2-min     minpd      (f64.2)  (f64.2 f64.2)  :cost 3 :encoding :sse :commutative t)
  (two-arg-f64.2+        addpd      (f64.2)  (f64.2 f64.2)  :cost 2 :encoding :sse :commutative t)
  (two-arg-f64.2-        subpd      (f64.2)  (f64.2 f64.2)  :cost 2 :encoding :sse)
  (two-arg-f64.2*        mulpd      (f64.2)  (f64.2 f64.2)  :cost 2 :encoding :sse :commutative t)
  (two-arg-f64.2/        divpd      (f64.2)  (f64.2 f64.2)  :cost 8 :encoding :sse)
  (two-arg-f64.2=        cmppd      (u64.2)  (f64.2 f64.2)  :cost 4 :encoding :sse :prefix :eq :commutative t)
  (two-arg-f64.2/=       cmppd      (u64.2)  (f64.2 f64.2)  :cost 4 :encoding :sse :prefix :neq :commutative t)
  (two-arg-f64.2<        cmppd      (u64.2)  (f64.2 f64.2)  :cost 4 :encoding :sse :prefix :lt)
  (two-arg-f64.2<=       cmppd      (u64.2)  (f64.2 f64.2)  :cost 4 :encoding :sse :prefix :le)
  (two-arg-f64.2>        cmppd      (u64.2)  (f64.2 f64.2)  :cost 4 :encoding :sse :prefix :nle)
  (two-arg-f64.2>=       cmppd      (u64.2)  (f64.2 f64.2)  :cost 4 :encoding :sse :prefix :nlt)
  (f64.2-sqrt            sqrtpd     (f64.2)  (f64.2)        :cost 20)
  ;; u8.16
  (two-arg-u8.16+        paddb      (u8.16)  (u8.16 u8.16)  :cost 2 :encoding :sse :commutative t)
  (two-arg-u8.16-        psubb      (u8.16)  (u8.16 u8.16)  :cost 2 :encoding :sse)
  ;; u16.8
  (two-arg-u16.8+        paddw      (u16.8)  (u16.8 u16.8)  :cost 2 :encoding :sse :commutative t)
  (two-arg-u16.8-        psubw      (u16.8)  (u16.8 u16.8)  :cost 2 :encoding :sse)
  ;; u32.4
  (two-arg-u32.4-and     pand       (u32.4)  (u32.4 u32.4)  :cost 1 :encoding :sse :commutative t)
  (two-arg-u32.4-or      por        (u32.4)  (u32.4 u32.4)  :cost 1 :encoding :sse :commutative t)
  (two-arg-u32.4-xor     pxor       (u32.4)  (u32.4 u32.4)  :cost 1 :encoding :sse :commutative t)
  (two-arg-u32.4-andnot  pandn      (u32.4)  (u32.4 u32.4)  :cost 1 :encoding :sse)
  (two-arg-u32.4+        paddd      (u32.4)  (u32.4 u32.4)  :cost 2 :encoding :sse :commutative t)
  (two-arg-u32.4-        psubd      (u32.4)  (u32.4 u32.4)  :cost 2 :encoding :sse)
  (u32.4-shiftl          pslld      (u32.4)  (u32.4 u32.4)  :cost 1 :encoding :sse)
  (u32.4-shiftr          psrld      (u32.4)  (u32.4 u32.4)  :cost 1 :encoding :sse)
  ;; u64.2
  (two-arg-u64.2-and     pand       (u64.2)  (u64.2 u64.2)  :cost 1 :encoding :sse :commutative t)
  (two-arg-u64.2-or      por        (u64.2)  (u64.2 u64.2)  :cost 1 :encoding :sse :commutative t)
  (two-arg-u64.2-xor     pxor       (u64.2)  (u64.2 u64.2)  :cost 1 :encoding :sse :commutative t)
  (two-arg-u64.2-andnot  pand       (u64.2)  (u64.2 u64.2)  :cost 1 :encoding :sse)
  (two-arg-u64.2+        paddq      (u64.2)  (u64.2 u64.2)  :cost 2 :encoding :sse :commutative t)
  (two-arg-u64.2-        psubq      (u64.2)  (u64.2 u64.2)  :cost 2 :encoding :sse)
  (u64.2-shiftl          psllq      (u64.2)  (u64.2 u64.2)  :cost 1 :encoding :sse)
  (u64.2-shiftr          psrlq      (u64.2)  (u64.2 u64.2)  :cost 1 :encoding :sse)
  ;; s32.4
  (two-arg-s32.4+        paddd      (s32.4)  (s32.4 s32.4)  :cost 2 :encoding :sse :commutative t)
  (two-arg-s32.4-        psubd      (s32.4)  (s32.4 s32.4)  :cost 2 :encoding :sse)
  ;; s64.2
  (two-arg-s64.2+        paddq      (s64.2)  (s64.2 s64.2)  :cost 2 :encoding :sse :commutative t)
  (two-arg-s64.2-        psubq      (s64.2)  (s64.2 s64.2)  :cost 2 :encoding :sse))

(define-instruction-records +sse3+
  (f32.4-hdup            movshdup   (f32.4)  (f32.4)        :cost 1)
  (f32.4-ldup            movsldup   (f32.4)  (f32.4)        :cost 1)
  (f64.2-broadcast       movddup    (f64.2)  (f64.2)        :cost 1))

(define-instruction-records +ssse3+
  (u32.4-hadd            phaddd     (u32.4)  (u32.4 u32.4)  :cost 3 :encoding :sse)
  (u32.4-hsub            phsubd     (u32.4)  (u32.4 u32.4)  :cost 3 :encoding :sse))

(define-instruction-records +sse4.1+
  (two-arg-u32.4*        mullo      (u32.4)  (u32.4 u32.4)  :cost 9 :encoding :sse :commutative t)
  (two-arg-u64.2=        pcmpeqq    (u64.2)  (u64.2 u64.2)  :cost 1 :encoding :sse :commutative t)
  (two-arg-s32.4*        pmulld     (s32.4)  (s32.4 s32.4)  :cost 2 :encoding :sse :commutative t))

(define-instruction-records +sse4.2+
  (two-arg-u64.2>        pcmpgtq    (u64.2)  (u64.2 u64.2)  :cost 3 :encoding :sse))

(define-instruction-records +avx+
  ;; f32.4
  (f32.4-from-f64.4      vcvtpd2ps  (f32.4)  (f64.4)        :cost 5)
  (two-arg-f32.4-and     vandps     (f32.4)  (f32.4 f32.4)  :cost 1 :commutative t)
  (two-arg-f32.4-or      vorps      (f32.4)  (f32.4 f32.4)  :cost 1 :commutative t)
  (two-arg-f32.4-xor     vxorps     (f32.4)  (f32.4 f32.4)  :cost 1 :commutative t)
  (two-arg-f32.4-andnot  vandnps    (f32.4)  (f32.4 f32.4)  :cost 1)
  (two-arg-f32.4-max     vmaxps     (f32.4)  (f32.4 f32.4)  :cost 3 :commutative t)
  (two-arg-f32.4-min     vminps     (f32.4)  (f32.4 f32.4)  :cost 3 :commutative t)
  (two-arg-f32.4=        vcmpps     (u32.4)  (f32.4 f32.4)  :cost 4 :prefix :eq :commutative t)
  (two-arg-f32.4/=       vcmpps     (u32.4)  (f32.4 f32.4)  :cost 4 :prefix :neq :commutative t)
  (two-arg-f32.4<        vcmpps     (u32.4)  (f32.4 f32.4)  :cost 4 :prefix :lt)
  (two-arg-f32.4<=       vcmpps     (u32.4)  (f32.4 f32.4)  :cost 4 :prefix :le)
  (two-arg-f32.4>        vcmpps     (u32.4)  (f32.4 f32.4)  :cost 4 :prefix :gt)
  (two-arg-f32.4>=       vcmpps     (u32.4)  (f32.4 f32.4)  :cost 4 :prefix :ge)
  (f32.4-hadd            vhaddps    (f32.4)  (f32.8 f32.4)  :cost 6)
  (f32.4-hsub            vhsubps    (f32.4)  (f32.8 f32.4)  :cost 6)
  (f32.4-reciprocal      vrcpps     (f32.4)  (f32.4)        :cost 5)
  (f32.4-rsqrt           vrsqrt     (f32.4)  (f32.4)        :cost 5)
  (f32.4-sqrt            vsqrt      (f32.4)  (f32.4)        :cost 15)
  (f32.4-unpackhi        vunpckhps  (f32.4)  (f32.4 f32.4)  :cost 1)
  (f32.4-unpacklo        vunpcklps  (f32.4)  (f32.4 f32.4)  :cost 1)
  (f32.4-broadcast       vbroadcastss (f32.4) (f32)         :cost 1)
  (f32.4-load            vmovups    (f32.4)  (f32vec index) :cost 7 :encoding :load)
  (f32.4-store           vmovups    (f32.4)  (f32.4 f32vec index) :cost 7 :encoding :store)
  (f32.4-ntstore         vmovntps   (f32.4)  (f32.4 f32vec index) :cost 5 :encoding :store)
  ;; f64.2
  (two-arg-f64.2-and     vandpd     (f64.2)  (f64.4 f64.2)  :cost 1 :commutative t)
  (two-arg-f64.2-or      vorpd      (f64.2)  (f64.4 f64.2)  :cost 1 :commutative t)
  (two-arg-f64.2-xor     vxorpd     (f64.2)  (f64.4 f64.2)  :cost 1 :commutative t)
  (two-arg-f64.2-andnot  vandnpd    (f64.2)  (f64.4 f64.2)  :cost 1)
  (two-arg-f64.2-max     vmaxpd     (f64.2)  (f64.4 f64.2)  :cost 3 :commutative t)
  (two-arg-f64.2-min     vminpd     (f64.2)  (f64.4 f64.2)  :cost 3 :commutative t)
  (two-arg-f64.2+        vaddpd     (f64.2)  (f64.2 f64.2)  :cost 2 :commutative t)
  (two-arg-f64.2-        vsubpd     (f64.2)  (f64.2 f64.2)  :cost 2)
  (two-arg-f64.2*        vmulpd     (f64.2)  (f64.2 f64.2)  :cost 2 :commutative t)
  (two-arg-f64.2/        vdivpd     (f64.2)  (f64.2 f64.2)  :cost 8)
  (two-arg-f64.2=        vcmppd     (u64.2)  (f64.2 f64.2)  :cost 4 :prefix :eq :commutative t)
  (two-arg-f64.2/=       vcmppd     (u64.2)  (f64.2 f64.2)  :cost 4 :prefix :neq :commutative t)
  (two-arg-f64.2<        vcmppd     (u64.2)  (f64.2 f64.2)  :cost 4 :prefix :lt)
  (two-arg-f64.2<=       vcmppd     (u64.2)  (f64.2 f64.2)  :cost 4 :prefix :le)
  (two-arg-f64.2>        vcmppd     (u64.2)  (f64.2 f64.2)  :cost 4 :prefix :gt)
  (two-arg-f64.2>=       vcmppd     (u64.2)  (f64.2 f64.2)  :cost 4 :prefix :ge)
  (f64.2-hadd            vhaddpd    (f64.2)  (f64.2 f64.2)  :cost 6)
  (f64.2-hsub            vhsubpd    (f64.2)  (f64.2 f64.2)  :cost 6)
  (f64.2-sqrt            vsqrtpd    (f64.2)  (f64.2)        :cost 20)
  (f64.2-unpackhi        vunpckhpd  (f64.2)  (f64.2 f64.2)  :cost 1)
  (f64.2-unpacklo        vunpcklpd  (f64.2)  (f64.2 f64.2)  :cost 1)
  (f64.2-load            vmovupd    (f64.2)  (f64vec index) :cost 7 :encoding :load)
  (f64.2-store           vmovupd    (f64.2)  (f64.2 f64vec index) :cost 7 :encoding :store)
  (f64.2-ntstore         vmovntpd   (f64.2)  (f64.2 f64vec index) :cost 5 :encoding :store)
  ;; f32.8
  (f32.8-from-u32.8      vcvtdq2ps  (f32.8)  (u32.8)        :cost 5)
  (two-arg-f32.8-and     vandps     (f32.8)  (f32.8 f32.8)  :cost 1 :commutative t)
  (two-arg-f32.8-or      vorps      (f32.8)  (f32.8 f32.8)  :cost 1 :commutative t)
  (two-arg-f32.8-xor     vxorps     (f32.8)  (f32.8 f32.8)  :cost 1 :commutative t)
  (two-arg-f32.8-andnot  vandnps    (f32.8)  (f32.8 f32.8)  :cost 1)
  (two-arg-f32.8-max     vmaxps     (f32.8)  (f32.8 f32.8)  :cost 3 :commutative t)
  (two-arg-f32.8-min     vminps     (f32.8)  (f32.8 f32.8)  :cost 3 :commutative t)
  (two-arg-f32.8+        vaddps     (f32.8)  (f32.8 f32.8)  :cost 2 :commutative t)
  (two-arg-f32.8-        vsubps     (f32.8)  (f32.8 f32.8)  :cost 2)
  (two-arg-f32.8*        vmulps     (f32.8)  (f32.8 f32.8)  :cost 2 :commutative t)
  (two-arg-f32.8/        vdivps     (f32.8)  (f32.8 f32.8)  :cost 8)
  (two-arg-f32.8=        vcmpps     (u32.8)  (f32.8 f32.8)  :cost 4 :prefix :eq :commutative t)
  (two-arg-f32.8/=       vcmpps     (u32.8)  (f32.8 f32.8)  :cost 4 :prefix :neq :commutative t)
  (two-arg-f32.8<        vcmpps     (u32.8)  (f32.8 f32.8)  :cost 4 :prefix :lt)
  (two-arg-f32.8<=       vcmpps     (u32.8)  (f32.8 f32.8)  :cost 4 :prefix :le)
  (two-arg-f32.8>        vcmpps     (u32.8)  (f32.8 f32.8)  :cost 4 :prefix :gt)
  (two-arg-f32.8>=       vcmpps     (u32.8)  (f32.8 f32.8)  :cost 4 :prefix :ge)
  (f32.8-hadd            vhaddps    (f32.8)  (f32.8 f32.8)  :cost 6)
  (f32.8-hsub            vhsubps    (f32.8)  (f32.8 f32.8)  :cost 6)
  (f32.8-reciprocal      vrcpps     (f32.8)  (f32.8)        :cost 5)
  (f32.8-rsqrt           vrsqrt     (f32.8)  (f32.8)        :cost 5)
  (f32.8-sqrt            vsqrt      (f32.8)  (f32.8)        :cost 15)
  (f32.8-unpackhi        vunpckhps  (f32.8)  (f32.8 f32.8)  :cost 1)
  (f32.8-unpacklo        vunpcklps  (f32.8)  (f32.8 f32.8)  :cost 1)
  (f32.8-broadcast       vbroadcastss (f32.8) (f32)         :cost 1)
  (f32.8-load            vmovups    (f32.8)  (f32vec index) :cost 7 :encoding :load)
  (f32.8-store           vmovups    (f32.8)  (f32.8 f32vec index) :cost 7 :encoding :store)
  (f32.8-ntstore         vmovntps   (f32.8)  (f32.8 f32vec index) :cost 5 :encoding :store)
  ;; f64.4
  (f64.4-from-f32.4      vcvtps2pd  (f64.4)  (f32.4)        :cost 5)
  (f64.4-from-u32.4      vcvtdq2pd  (f64.4)  (u32.4)        :cost 5)
  (two-arg-f64.4-and     vandpd     (f64.4)  (f64.4 f64.4)  :cost 1 :commutative t)
  (two-arg-f64.4-or      vorpd      (f64.4)  (f64.4 f64.4)  :cost 1 :commutative t)
  (two-arg-f64.4-xor     vxorpd     (f64.4)  (f64.4 f64.4)  :cost 1 :commutative t)
  (two-arg-f64.4-andnot  vandnpd    (f64.4)  (f64.4 f64.4)  :cost 1)
  (two-arg-f64.4-max     vmaxpd     (f64.4)  (f64.4 f64.4)  :cost 3 :commutative t)
  (two-arg-f64.4-min     vminpd     (f64.4)  (f64.4 f64.4)  :cost 3 :commutative t)
  (two-arg-f64.4+        vaddpd     (f64.4)  (f64.4 f64.4)  :cost 2 :commutative t)
  (two-arg-f64.4-        vsubpd     (f64.4)  (f64.4 f64.4)  :cost 2)
  (two-arg-f64.4*        vmulpd     (f64.4)  (f64.4 f64.4)  :cost 2 :commutative t)
  (two-arg-f64.4/        vdivpd     (f64.4)  (f64.4 f64.4)  :cost 8)
  (two-arg-f64.4=        vcmppd     (u64.4)  (f64.4 f64.4)  :cost 4 :prefix :eq :commutative t)
  (two-arg-f64.4/=       vcmppd     (u64.4)  (f64.4 f64.4)  :cost 4 :prefix :neq :commutative t)
  (two-arg-f64.4<        vcmppd     (u64.4)  (f64.4 f64.4)  :cost 4 :prefix :lt)
  (two-arg-f64.4<=       vcmppd     (u64.4)  (f64.4 f64.4)  :cost 4 :prefix :le)
  (two-arg-f64.4>        vcmppd     (u64.4)  (f64.4 f64.4)  :cost 4 :prefix :gt)
  (two-arg-f64.4>=       vcmppd     (u64.4)  (f64.4 f64.4)  :cost 4 :prefix :ge)
  (f64.4-hadd            vhaddpd    (f64.4)  (f64.4 f64.4)  :cost 6)
  (f64.4-hsub            vhsubpd    (f64.4)  (f64.4 f64.4)  :cost 6)
  (f64.4-sqrt            vsqrtpd    (f64.4)  (f64.4)        :cost 20)
  (f64.4-unpackhi        vunpckhpd  (f64.4)  (f64.4 f64.4)  :cost 1)
  (f64.4-unpacklo        vunpcklpd  (f64.4)  (f64.4 f64.4)  :cost 1)
  (f64.4-broadcast       vbroadcastsd (f64.4) (f64)         :cost 1)
  (f64.4-load            vmovupd    (f64.4)  (f64vec index) :cost 7 :encoding :load)
  (f64.4-store           vmovupd    (f64.4)  (f64.4 f64vec index) :cost 7 :encoding :store)
  (f64.4-ntstore         vmovntpd   (f64.4)  (f64.4 f64vec index) :cost 5 :encoding :store)
  ;; u32.4
  (u32.4-from-f64.4      vcvpd2dq   (u32.4)  (f64.4)        :cost 6)
  (two-arg-u32.4-and     vpand      (u32.4)  (u32.4 u32.4)  :cost 1 :commutative t)
  (two-arg-u32.4-or      vpor       (u32.4)  (u32.4 u32.4)  :cost 1 :commutative t)
  (two-arg-u32.4-xor     vpxor      (u32.4)  (u32.4 u32.4)  :cost 1 :commutative t)
  (two-arg-u32.4-andnot  vpandn     (u32.4)  (u32.4 u32.4)  :cost 1)
  (two-arg-u32.4+        vpaddd     (u32.4)  (u32.4 u32.4)  :cost 2 :commutative t)
  (two-arg-u32.4-        vpsubd     (u32.4)  (u32.4 u32.4)  :cost 2)
  (u32.4-shiftl          vpslld     (u32.4)  (u32.4 u32.4)  :cost 1)
  (u32.4-shiftr          vpsrld     (u32.4)  (u32.4 u32.4)  :cost 1)
  (u32.4-load            vmovdqu    (u32.4)  (u32vec index) :cost 7 :encoding :load)
  (u32.4-store           vmovdqu    (u32.4)  (u32.4 u32vec index) :cost 7 :encoding :store)
  (u32.4-ntstore         vmovntdq   (u32.4)  (u32.4 u32vec index) :cost 5 :encoding :store)
  ;; u64.2
  (two-arg-u64.2-and     vpand      (u64.2)  (u64.2 u64.2)  :cost 1 :commutative t)
  (two-arg-u64.2-or      vpor       (u64.2)  (u64.2 u64.2)  :cost 1 :commutative t)
  (two-arg-u64.2-xor     vpxor      (u64.2)  (u64.2 u64.2)  :cost 1 :commutative t)
  (two-arg-u64.2-andnot  vpand      (u64.2)  (u64.2 u64.2)  :cost 1)
  (two-arg-u64.2+        vpaddq     (u64.2)  (u64.2 u64.2)  :cost 2 :commutative t)
  (two-arg-u64.2-        vpsubq     (u64.2)  (u64.2 u64.2)  :cost 2)
  (u64.2-shiftl          vpsllq     (u64.2)  (u64.2 u64.2)  :cost 1)
  (u64.2-shiftr          vpsrlq     (u64.2)  (u64.2 u64.2)  :cost 1)
  (u64.2-load            vmovdqu    (u64.2)  (u64vec index) :cost 7 :encoding :load)
  (u64.2-store           vmovdqu    (u64.2)  (u64.2 u64vec index) :cost 7 :encoding :store)
  (u64.2-ntstore         vmovntdq   (u64.2)  (u64.2 u64vec index) :cost 5 :encoding :store)
  ;; u32.8
  (u32.8-from-f32.8      vcvtps2dq  (u32.8)  (f32.8)        :cost 4)
  (two-arg-u32.8-and     vandps     (u32.8)  (u32.8 u32.8)  :cost 1 :commutative t)
  (two-arg-u32.8-or      vorps      (u32.8)  (u32.8 u32.8)  :cost 1 :commutative t)
  (two-arg-u32.8-xor     vxorps     (u32.8)  (u32.8 u32.8)  :cost 1 :commutative t)
  (two-arg-u32.8-andnot  vandnps    (u32.8)  (u32.8 u32.8)  :cost 1)
  (u32.8-load            vmovdqu    (u32.8)  (u32vec index) :cost 7 :encoding :load)
  (u32.8-store           vmovdqu    (u32.8)  (u32.8 u32vec index) :cost 7 :encoding :store)
  (u32.8-ntstore         vmovntdq   (u32.8)  (u32.8 u32vec index) :cost 5 :encoding :store)
  ;; u64.4
  (two-arg-u64.4-and     vandpd     (u64.4)  (u64.4 u64.4)  :cost 1 :commutative t)
  (two-arg-u64.4-or      vorpd      (u64.4)  (u64.4 u64.4)  :cost 1 :commutative t)
  (two-arg-u64.4-xor     vxorpd     (u64.4)  (u64.4 u64.4)  :cost 1 :commutative t)
  (two-arg-u64.4-andnot  vandnpd    (u64.4)  (u64.4 u64.4)  :cost 1)
  (u64.4-load            vmovdqu    (u64.4)  (u64vec index) :cost 7 :encoding :load)
  (u64.4-store           vmovdqu    (u64.4)  (u64.4 u64vec index) :cost 7 :encoding :store)
  (u64.4-ntstore         vmovntdq   (u64.4)  (u64.4 u64vec index) :cost 5 :encoding :store)
  ;; s8.16
  (two-arg-s8.16+        vpaddw     (s8.16)  (s8.16 s8.16)  :cost 2 :commutative t)
  (two-arg-s8.16-        vpsubw     (s8.16)  (s8.16 s8.16)  :cost 2)
  (two-arg-s8.16*        vpmullw    (s8.16)  (s8.16 s8.16)  :cost 2 :commutative t)
  ;; s16.8
  (two-arg-s16.8+        vpaddw     (s16.8)  (s16.8 s16.8)  :cost 2 :commutative t)
  (two-arg-s16.8-        vpsubw     (s16.8)  (s16.8 s16.8)  :cost 2)
  (two-arg-s16.8*        vpmullw    (s16.8)  (s16.8 s16.8)  :cost 2 :commutative t)
  ;; s32.4
  (two-arg-s32.4+        vpaddd     (s32.4)  (s32.4 s32.4)  :cost 2 :commutative t)
  (two-arg-s32.4-        vpsubd     (s32.4)  (s32.4 s32.4)  :cost 2)
  (two-arg-s32.4*        vpmulld    (s32.4)  (s32.4 s32.4)  :cost 2 :commutative t)
  ;; s64.2
  (two-arg-s64.2+        vpaddq     (s64.2)  (s64.2 s64.2)  :cost 2 :commutative t)
  (two-arg-s64.2-        vpsubq     (s64.2)  (s64.2 s64.2)  :cost 2))

(define-instruction-records +avx2+
  ;; f32.4
  (f32.4-broadcast       vbroadcastss (f32.4)  (f32.4)       :cost 1)
  (f32.4-ntload          vmovntdqa    (f32.4)  (f32vec index) :cost 7 :encoding :load)
  ;; f64.2
  (f64.2-broadcast       movddup      (f64.2)  (f64.2)       :cost 1)
  (f64.2-ntload          vmovntdqa    (f64.2)  (f64vec index) :cost 7 :encoding :load)
  ;; f32.8
  (f32.8-broadcast       vbroadcastss (f32.8)  (f32.4)       :cost 1)
  (f32.8-ntload          vmovntdqa    (f32.8)  (f32vec index) :cost 7 :encoding :load)
  ;; f64.4
  (f64.4-broadcast       vbroadcastsd (f64.4)  (f64.2)       :cost 1)
  (f64.4-ntload          vmovntdqa    (f64.4)  (f64vec index) :cost 7 :encoding :load)
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
  (u32.4-ntload          vmovntdqa    (u32.4)  (u32vec index) :cost 7 :encoding :load)
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
  (u64.2-ntload          vmovntdqa    (u64.2)  (u64vec index) :cost 7 :encoding :load)
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
  (u32.8-ntload          vmovntdqa    (u32.8)  (u32vec index) :cost 7 :encoding :load)
  ;; u64.4
  (two-arg-u64.4+        vpaddq       (u64.4)  (u64.4 u64.4) :cost 1 :commutative t)
  (two-arg-u64.4-        vpsubq       (u64.4)  (u64.4 u64.4) :cost 1)
  (two-arg-u64.4=        vpcmpeqq     (u64.4)  (u64.4 u64.4) :cost 1 :commutative t)
  (two-arg-u64.4>        vpcmpgtq     (u64.4)  (u64.4 u64.4) :cost 1)
  (u64.4-shiftl          vpsllvq      (u64.4)  (u64.4 u64.4) :cost 1)
  (u64.4-shiftr          vpsrlvq      (u64.4)  (u64.4 u64.4) :cost 1)
  (u64.4-unpackhi        vpunpckhqdq  (u64.4)  (u64.4 u64.4) :cost 1)
  (u64.4-unpacklo        vpunpcklqdq  (u64.4)  (u64.4 u64.4) :cost 1)
  (u64.4-broadcast       vpbroadcastq (u64.4)  (u64.2)       :cost 1)
  (u64.4-ntload          vmovntdqa    (u64.4)  (u64vec index) :cost 7 :encoding :load)
  ;; s8.32
  ;(two-arg-s8.32+        vpaddb       (s8.32) (s8.32 s8.32)  :cost 2 :commutative t)
  ;(two-arg-s8.32-        vpsubb       (s8.32) (s8.32 s8.32)  :cost 2)
  ;(two-arg-s8.32*        vpmullb      (s8.32) (s8.32 s8.32)  :cost 2 :commutative t)
  ;; s16.16
  (two-arg-s16.16+       vpaddw       (s16.16) (s16.16 s16.16)  :cost 2 :commutative t)
  (two-arg-s16.16-       vpsubw       (s16.16) (s16.16 s16.16)  :cost 2)
  (two-arg-s16.16*       vpmullw      (s16.16) (s16.16 s16.16)  :cost 2 :commutative t)
  ;; s32.8
  (two-arg-s32.8+        vpaddd       (s32.8)  (s32.8 s32.8) :cost 2 :commutative t)
  (two-arg-s32.8-        vpsubd       (s32.8)  (s32.8 s32.8) :cost 2)
  (two-arg-s32.8*        vpmulld      (s32.8)  (s32.8 s32.8) :cost 2 :commutative t)
  ;; s64.4
  (two-arg-s64.4+        vpaddq       (s64.4)  (s64.4 s64.4) :cost 1 :commutative t)
  (two-arg-s64.4-        vpsubq       (s64.4)  (s64.4 s64.4) :cost 1))
