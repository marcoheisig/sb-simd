(in-package #:sb-simd)

(defconstant +sse+ #+x86-64 t #-x86-64 nil)

(defconstant +sse2+ #+x86-64 t #-x86-64 nil)

;; TODO Not all amd64 CPUs support SSE3 and above.
(defconstant +sse3+ #+x86-64 t #-x86-64 nil)

(defconstant +ssse3+ #+x86-64 t #-x86-64 nil)

(defconstant +sse4.1+ #+x86-64 t #-x86-64 nil)

(defconstant +sse4.2+ #+x86-64 t #-x86-64 nil)

(defconstant +avx+ (plusp (sb-alien:extern-alien "avx_supported" sb-alien:int)))

(defconstant +avx2+ (plusp (sb-alien:extern-alien "avx2_supported" sb-alien:int)))

(defconstant +fma+ (not (null (find-symbol "VFMADD231PD" sb-assem::*backend-instruction-set-package*))))
