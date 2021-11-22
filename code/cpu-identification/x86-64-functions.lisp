(in-package #:sb-simd-internals)

(define-inline cpuid (eax &optional (ecx 0))
  (declare (type (unsigned-byte 32) eax ecx))
  (%cpuid eax ecx))

(defun sse-supported-p ()
  (and (>= (cpuid 0) 1)
       (logbitp 25 (nth-value 3 (cpuid 1)))))

(defun sse2-supported-p ()
  (and (>= (cpuid 0) 1)
       (logbitp 26 (nth-value 3 (cpuid 1)))))

(defun sse3-supported-p ()
  (and (>= (cpuid 0) 1)
       (logbitp 0 (nth-value 2 (cpuid 1)))))

(defun ssse3-supported-p ()
  (and (>= (cpuid 0) 1)
       (logbitp 9 (nth-value 2 (cpuid 1)))))

(defun sse4.1-supported-p ()
  (and (>= (cpuid 0) 1)
       (logbitp 19 (nth-value 2 (cpuid 1)))))

(defun sse4.2-supported-p ()
  (and (>= (cpuid 0) 1)
       (logbitp 20 (nth-value 2 (cpuid 1)))))

(defun avx-supported-p ()
  (and (>= (cpuid 0) 1)
       (logbitp 28 (nth-value 2 (cpuid 1)))))

(defun avx2-supported-p ()
  (and (>= (cpuid 0) 7)
       (logbitp 5 (nth-value 1 (cpuid 7)))))

(defun fma3-supported-p ()
  (and (>= (cpuid 0) 1)
       (logbitp 12 (nth-value 2 (cpuid 1)))))
