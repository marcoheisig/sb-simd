(in-package #:sb-vm)

;; SSE
#-sb-xc-host
(progn
  (declaim (inline %make-simd-pack-ub16 %make-simd-pack-ub8
                   %simd-pack-ub16s %simd-pack-ub8s))
  (defun %make-simd-pack-ub16 (p0 p1 p2 p3 p4 p5 p6 p7)
    (declare (type (unsigned-byte 16) p0 p1 p2 p3 p4 p5 p6 p7))
    (%make-simd-pack-ub32 (logior p0 (ash p1 16))
                          (logior p2 (ash p3 16))
                          (logior p4 (ash p5 16))
                          (logior p6 (ash p7 16))))

  (defun %simd-pack-ub16s (pack)
    (declare (type simd-pack pack))
    (multiple-value-bind (p0 p1 p2 p3)
        (%simd-pack-ub32s pack)
      (values (ldb (byte 16 0) p0)
              (ash p0 -16)
              (ldb (byte 16 0) p1)
              (ash p1 -16)
              (ldb (byte 16 0) p2)
              (ash p2 -16)
              (ldb (byte 16 0) p3)
              (ash p3 -16))))

  (defun %make-simd-pack-ub8 (p0 p1 p2 p3 p4 p5 p6 p7
                              p8 p9 p10 p11 p12 p13 p14 p15)
    (declare (type (unsigned-byte 8) p0 p1 p2 p3 p4 p5 p6 p7
                   p8 p9 p10 p11 p12 p13 p14 p15))
    (%make-simd-pack-ub16 (logior p0 (ash p1 8))
                          (logior p2 (ash p3 8))
                          (logior p4 (ash p5 8))
                          (logior p6 (ash p7 8))
                          (logior p8 (ash p9 8))
                          (logior p10 (ash p11 8))
                          (logior p12 (ash p13 8))
                          (logior p14 (ash p15 8))))
  
   (defun %simd-pack-ub8s (pack)
    (declare (type simd-pack pack))
    (multiple-value-bind (p0 p1 p2 p3 p4 p5 p6 p7 p8)
        (%simd-pack-ub16s pack)
      (values (ldb (byte 8 0) p0) (ash p0 -8)
              (ldb (byte 8 0) p1) (ash p1 -8)
              (ldb (byte 8 0) p2) (ash p2 -8)
              (ldb (byte 8 0) p3) (ash p3 -8)
              (ldb (byte 8 0) p4) (ash p4 -8)
              (ldb (byte 8 0) p5) (ash p5 -8)
              (ldb (byte 8 0) p6) (ash p6 -8)
              (ldb (byte 8 0) p7) (ash p7 -8)))))

;; AVX
#-sb-xc-host
(progn
  ;; (declaim (inline %make-simd-pack-256-sb64 %make-simd-pack-256-sb32
  ;;                  %simd-pack-256-sb32s %simd-pack-256-sb64s))

)
