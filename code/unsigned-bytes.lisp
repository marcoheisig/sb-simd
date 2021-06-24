(in-package #:sb-ext)

;; SSE
#-sb-xc-host
(progn
  (declaim (inline %make-simd-pack-ub16 %make-simd-pack-ub8
                   %simd-pack-ub16s %simd-pack-ub8s))
  (defun %make-simd-pack-ub16 (p0 p1 p2 p3 p4 p5 p6 p7)
    (declare (type (unsigned-byte 16) p0 p1 p2 p3 p4 p5 p6 p7))
    (%make-simd-pack-ub32
     (logior p0 (ash p1 16))
     (logior p2 (ash p3 16))
     (logior p4 (ash p5 16))
     (logior p6 (ash p7 16))))
  (export '%make-simd-pack-ub16)

  (defun %simd-pack-ub16s (pack)
    (declare (type simd-pack pack))
    (multiple-value-bind (p0 p1 p2 p3)
        (%simd-pack-ub32s pack)
      (values
       (ldb (byte 16 0) p0) (ash p0 -16)
       (ldb (byte 16 0) p1) (ash p1 -16)
       (ldb (byte 16 0) p2) (ash p2 -16)
       (ldb (byte 16 0) p3) (ash p3 -16))))
  (export '%simd-pack-ub16s)

  (defun %make-simd-pack-ub8 (p0 p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15)
    (declare (type (unsigned-byte 8) p0 p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15))
    (%make-simd-pack-ub16
     (logior p0  (ash p1  8))
     (logior p2  (ash p3  8))
     (logior p4  (ash p5  8))
     (logior p6  (ash p7  8))
     (logior p8  (ash p9  8))
     (logior p10 (ash p11 8))
     (logior p12 (ash p13 8))
     (logior p14 (ash p15 8))))
  (export '%make-simd-pack-ub8)

  (defun %simd-pack-ub8s (pack)
    (declare (type simd-pack pack))
    (multiple-value-bind (p0 p1 p2 p3 p4 p5 p6 p7)
        (%simd-pack-ub16s pack)
      (values (ldb (byte 8 0) p0) (ash p0 -8)
              (ldb (byte 8 0) p1) (ash p1 -8)
              (ldb (byte 8 0) p2) (ash p2 -8)
              (ldb (byte 8 0) p3) (ash p3 -8)
              (ldb (byte 8 0) p4) (ash p4 -8)
              (ldb (byte 8 0) p5) (ash p5 -8)
              (ldb (byte 8 0) p6) (ash p6 -8)
              (ldb (byte 8 0) p7) (ash p7 -8))))
  (export '%simd-pack-ub8s))

;; AVX
#-sb-xc-host
(progn
  (declaim (inline %make-simd-pack-256-ub16 %make-simd-pack-256-ub8
                   %simd-pack-256-sb16s %simd-pack-256-sb8s))
  (defun %make-simd-pack-256-ub16 (p0 p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15)
    (declare (type (unsigned-byte 16) p0 p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15))
    (%make-simd-pack-256-ub32
     (logior p0  (ash p1  16))
     (logior p2  (ash p3  16))
     (logior p4  (ash p5  16))
     (logior p6  (ash p7  16))
     (logior p8  (ash p9  16))
     (logior p10 (ash p11 16))
     (logior p12 (ash p13 16))
     (logior p14 (ash p15 16))))
  (export '%make-simd-pack-256-ub16)

  (defun %simd-pack-256-ub16s (pack)
    (declare (type simd-pack-256 pack))
    (multiple-value-bind (p0 p1 p2 p3 p4 p5 p6 p7)
        (%simd-pack-256-ub32s pack)
      (values
       (ldb (byte 16 0) p0) (ash p0 -16)
       (ldb (byte 16 0) p1) (ash p1 -16)
       (ldb (byte 16 0) p2) (ash p2 -16)
       (ldb (byte 16 0) p3) (ash p3 -16)
       (ldb (byte 16 0) p4) (ash p4 -16)
       (ldb (byte 16 0) p5) (ash p5 -16)
       (ldb (byte 16 0) p6) (ash p6 -16)
       (ldb (byte 16 0) p7) (ash p7 -16))))
  (export '%simd-pack-256-ub16s)

  (defun %make-simd-pack-256-ub8 (p0 p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13
                                  p14 p15 p16 p17 p18 p19 p20 p21 p22 p23 p24 p25 p26 p27 p28 p29 p30 p31)
    (declare (type (unsigned-byte 8) p0 p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12
                   p13 p14 p15 p16 p17 p18 p19 p20 p21 p22 p23 p24 p25 p26 p27 p28 p29 p30 p31))
    (%make-simd-pack-256-ub16
     (logior p0  (ash p1  8)) (logior p2  (ash p3  8)) (logior p4  (ash p5  8))
     (logior p6  (ash p7  8)) (logior p8  (ash p9  8)) (logior p10 (ash p11 8))
     (logior p12 (ash p13 8)) (logior p14 (ash p15 8)) (logior p16 (ash p17 8))
     (logior p18 (ash p19 8)) (logior p20 (ash p21 8)) (logior p22 (ash p23 8))
     (logior p24 (ash p25 8)) (logior p26 (ash p27 8)) (logior p28 (ash p29 8))
     (logior p30 (ash p31 8))))
  (export '%make-simd-pack-256-ub8)

  (defun %simd-pack-256-ub8s (pack)
    (declare (type simd-pack-256 pack))
    (multiple-value-bind (p0 p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15)
        (%simd-pack-256-ub16s pack)
      (values
       (ldb (byte 8 0) p0)  (ash p0  -8)
       (ldb (byte 8 0) p1)  (ash p1  -8)
       (ldb (byte 8 0) p2)  (ash p2  -8)
       (ldb (byte 8 0) p3)  (ash p3  -8)
       (ldb (byte 8 0) p4)  (ash p4  -8)
       (ldb (byte 8 0) p5)  (ash p5  -8)
       (ldb (byte 8 0) p6)  (ash p6  -8)
       (ldb (byte 8 0) p7)  (ash p7  -8)
       (ldb (byte 8 0) p8)  (ash p8  -8)
       (ldb (byte 8 0) p9)  (ash p9  -8)
       (ldb (byte 8 0) p10) (ash p10 -8)
       (ldb (byte 8 0) p11) (ash p11 -8)
       (ldb (byte 8 0) p12) (ash p12 -8)
       (ldb (byte 8 0) p13) (ash p13 -8)
       (ldb (byte 8 0) p14) (ash p14 -8)
       (ldb (byte 8 0) p15) (ash p15 -8))))
  (export '%simd-pack-256-ub8s))
