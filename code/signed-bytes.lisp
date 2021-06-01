(in-package #:sb-vm)

;; SSE
#-sb-xc-host
(progn
  (declaim (inline %make-simd-pack-sb64 %make-simd-pack-sb32
                   %make-simd-pack-sb16 %make-simd-pack-sb8
                   %simd-pack-sb64s %simd-pack-sb32s
                   %simd-pack-sb16s %simd-pack-sb8s))
  (defun %make-simd-pack-sb64 (p0 p1)
    (declare (type (signed-byte 64) p0 p1))
    (%make-simd-pack-ub64 (ldb (byte 64 0) p0)
                          (ldb (byte 64 0) p1)))

  (defun %simd-pack-sb64s (pack)
    (declare (type simd-pack pack))
    (values (sb-c::mask-signed-field 64 (%simd-pack-low pack))
            (sb-c::mask-signed-field 64 (%simd-pack-high pack))))
  
  (defun %make-simd-pack-sb32 (p0 p1 p2 p3)
    (declare (type (signed-byte 32) p0 p1 p2 p3))
    (%make-simd-pack-ub64 (logior (ldb (byte 32 0) p0)
                                  (ash (ldb (byte 32 0) p1) 32))
                          (logior (ldb (byte 32 0) p2)
                                  (ash (ldb (byte 32 0) p3) 32))))

  (defun %simd-pack-sb32s (pack)
    (declare (type simd-pack pack))
    (let ((lo (%simd-pack-low pack))
          (hi (%simd-pack-high pack)))
      (values (sb-c::mask-signed-field 32 (ldb (byte 32 0) lo))
              (sb-c::mask-signed-field 32 (ash lo -32))
              (sb-c::mask-signed-field 32 (ldb (byte 32 0) hi))
              (sb-c::mask-signed-field 32 (ash hi -32)))))

  (defun %make-simd-pack-sb16 (p0 p1 p2 p3 p4 p5 p6 p7)
    (declare (type (signed-byte 16) p0 p1 p2 p3 p4 p5 p6 p7))
    (%make-simd-pack-ub32 (logior (ldb (byte 16 0) p0)
                                  (ash (ldb (byte 16 0) p1) 16))
                          (logior (ldb (byte 16 0) p2)
                                  (ash (ldb (byte 16 0) p3) 16))
                          (logior (ldb (byte 16 0) p4)
                                  (ash (ldb (byte 16 0) p5) 16))
                          (logior (ldb (byte 16 0) p6)
                                  (ash (ldb (byte 16 0) p7) 16))))

  (defun %simd-pack-sb16s (pack)
    (declare (type simd-pack pack))
    (multiple-value-bind (p0 p1 p2 p3)
        (%simd-pack-ub32s pack)
      (values (sb-c::mask-signed-field 16 (ldb (byte 16 0) p0))
              (sb-c::mask-signed-field 16 (ash p0 -16))
              (sb-c::mask-signed-field 16 (ldb (byte 16 0) p1))
              (sb-c::mask-signed-field 16 (ash p1 -16))
              (sb-c::mask-signed-field 16 (ldb (byte 16 0) p2))
              (sb-c::mask-signed-field 16 (ash p2 -16))
              (sb-c::mask-signed-field 16 (ldb (byte 16 0) p3))
              (sb-c::mask-signed-field 16 (ash p3 -16)))))
  
  (defun %make-simd-pack-sb8 (p0 p1 p2 p3 p4 p5 p6 p7
                               p8 p9 p10 p11 p12 p13 p14 p15)
    (declare (type (signed-byte 8) p0 p1 p2 p3 p4 p5 p6 p7
                   p8 p9 p10 p11 p12 p13 p14 p15))
    (%make-simd-pack-ub16 (logior (ldb (byte 8 0) p0)
                                  (ash (ldb (byte 8 0) p1) 8))
                          (logior (ldb (byte 8 0) p2)
                                  (ash (ldb (byte 8 0) p3) 8))
                          (logior (ldb (byte 8 0) p4)
                                  (ash (ldb (byte 8 0) p5) 8))
                          (logior (ldb (byte 8 0) p6)
                                  (ash (ldb (byte 8 0) p7) 8))
                          (logior (ldb (byte 8 0) p8)
                                  (ash (ldb (byte 8 0) p9) 8))
                          (logior (ldb (byte 8 0) p10)
                                  (ash (ldb (byte 8 0) p11) 8))
                          (logior (ldb (byte 8 0) p12)
                                  (ash (ldb (byte 8 0) p13) 8))
                          (logior (ldb (byte 8 0) p14)
                                  (ash (ldb (byte 8 0) p15) 8))))

  (defun %simd-pack-sb8s (pack)
    (declare (type simd-pack pack))
    (multiple-value-bind (p0 p1 p2 p3 p4 p5 p6 p7)
        (%simd-pack-ub16s pack)
      (values (sb-c::mask-signed-field 8 (ldb (byte 8 0) p0))
              (sb-c::mask-signed-field 8 (ash p0 -8))
              (sb-c::mask-signed-field 8 (ldb (byte 8 0) p1))
              (sb-c::mask-signed-field 8 (ash p1 -8))
              (sb-c::mask-signed-field 8 (ldb (byte 8 0) p2))
              (sb-c::mask-signed-field 8 (ash p2 -8))
              (sb-c::mask-signed-field 8 (ldb (byte 8 0) p3))
              (sb-c::mask-signed-field 8 (ash p3 -8))
              (sb-c::mask-signed-field 8 (ldb (byte 8 0) p4))
              (sb-c::mask-signed-field 8 (ash p4 -8))
              (sb-c::mask-signed-field 8 (ldb (byte 8 0) p5))
              (sb-c::mask-signed-field 8 (ash p5 -8))
              (sb-c::mask-signed-field 8 (ldb (byte 8 0) p6))
              (sb-c::mask-signed-field 8 (ash p6 -8))
              (sb-c::mask-signed-field 8 (ldb (byte 8 0) p7))
              (sb-c::mask-signed-field 8 (ash p7 -8))))))

;; AVX
#-sb-xc-host
(progn
  (declaim (inline %make-simd-pack-256-sb64 %make-simd-pack-256-sb32
                   %make-simd-pack-256-sb16
                   %simd-pack-256-sb64s %simd-pack-256-sb32s
                   %simd-pack-256-sb16s %simd-pack-256-sb8s))
  (defun %make-simd-256-pack-sb64 (p0 p1 p2 p3)
    (declare (type (signed-byte 64) p0 p1 p2 p3))
    (%make-simd-pack-ub64 (ldb (byte 64 0) p0)
                          (ldb (byte 64 0) p1)
                          (ldb (byte 64 0) p2)
                          (ldb (byte 64 0) p3)))
  
  (defun %simd-pack-256-sb64s (pack)
    (declare (type simd-pack-256 pack))
    (values (sb-c::mask-signed-field 64 (%simd-pack-256-0 pack))
            (sb-c::mask-signed-field 64 (%simd-pack-256-1 pack))
            (sb-c::mask-signed-field 64 (%simd-pack-256-2 pack))
            (sb-c::mask-signed-field 64 (%simd-pack-256-3 pack))))
  
  (defun %make-simd-pack-256-sb32 (p0 p1 p2 p3 p4 p5 p6 p7)
    (declare (type (signed-byte 32) p0 p1 p2 p3 p4 p5 p6 p7))
    (%make-simd-pack-256-ub64 (logior (ldb (byte 32 0) p0)
                                      (ash (ldb (byte 32 0) p1) 32))
                              (logior (ldb (byte 32 0) p2)
                                      (ash (ldb (byte 32 0) p3) 32))
                              (logior (ldb (byte 32 0) p4)
                                      (ash (ldb (byte 32 0) p5) 32))
                              (logior (ldb (byte 32 0) p6)
                                      (ash (ldb (byte 32 0) p7) 32))))

  (defun %simd-pack-256-sb32s (pack)
    (declare (type simd-pack-256 pack))
    (let ((p0 (%simd-pack-256-0 pack))
          (p1 (%simd-pack-256-1 pack))
          (p2 (%simd-pack-256-2 pack))
          (p3 (%simd-pack-256-3 pack)))
      (values (sb-c::mask-signed-field 32 (ldb (byte 32 0) p0))
              (sb-c::mask-signed-field 32 (ash p0 -32))
              (sb-c::mask-signed-field 32 (ldb (byte 32 0) p1))
              (sb-c::mask-signed-field 32 (ash p1 -32))
              (sb-c::mask-signed-field 32 (ldb (byte 32 0) p2))
              (sb-c::mask-signed-field 32 (ash p2 -32))
              (sb-c::mask-signed-field 32 (ldb (byte 32 0) p3))
              (sb-c::mask-signed-field 32 (ash p3 -32)))))

  (defun %make-simd-pack-256-sb16 (p0 p1 p2 p3 p4 p5 p6 p7
                                   p8 p9 p10 p11 p12 p13 p14 p15)
    (declare (type (signed-byte 16) p0 p1 p2 p3 p4 p5 p6 p7
                   p8 p9 p10 p11 p12 p13 p14 p15))
    (%make-simd-pack-256-ub32 (logior (ldb (byte 16 0) p0)  (ash (ldb (byte 16 0) p1) 16))
                              (logior (ldb (byte 16 0) p2)  (ash (ldb (byte 16 0) p3) 16))
                              (logior (ldb (byte 16 0) p4)  (ash (ldb (byte 16 0) p5) 16))
                              (logior (ldb (byte 16 0) p6)  (ash (ldb (byte 16 0) p7) 16))
                              (logior (ldb (byte 16 0) p8)  (ash (ldb (byte 16 0) p9) 16))
                              (logior (ldb (byte 16 0) p10) (ash (ldb (byte 16 0) p11) 16))
                              (logior (ldb (byte 16 0) p12) (ash (ldb (byte 16 0) p13) 16))
                              (logior (ldb (byte 16 0) p14) (ash (ldb (byte 16 0) p15) 16))))
  
  (defun %simd-pack-256-sb16s (pack)
    (declare (type simd-pack-256 pack))
    (multiple-value-bind (p0 p1 p2 p3 p4 p5 p6 p7)
        (%simd-pack-256-sb32s pack)
      (values (sb-c::mask-signed-field 16 (ldb (byte 16 0) p0))
              (sb-c::mask-signed-field 16 (ash p0 -16))
              (sb-c::mask-signed-field 16 (ldb (byte 16 0) p1))
              (sb-c::mask-signed-field 16 (ash p1 -16))
              (sb-c::mask-signed-field 16 (ldb (byte 16 0) p2))
              (sb-c::mask-signed-field 16 (ash p2 -16))
              (sb-c::mask-signed-field 16 (ldb (byte 16 0) p3))
              (sb-c::mask-signed-field 16 (ash p3 -16))
              (sb-c::mask-signed-field 16 (ldb (byte 16 0) p4))
              (sb-c::mask-signed-field 16 (ash p4 -16))
              (sb-c::mask-signed-field 16 (ldb (byte 16 0) p5))
              (sb-c::mask-signed-field 16 (ash p5 -16))
              (sb-c::mask-signed-field 16 (ldb (byte 16 0) p6))
              (sb-c::mask-signed-field 16 (ash p6 -16))
              (sb-c::mask-signed-field 16 (ldb (byte 16 0) p7))
              (sb-c::mask-signed-field 16 (ash p7 -16)))))

  (defun %make-simd-pack-256-sb8 (p0 p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15
                                   p16 p17 p18 p19 p20 p21 p22 p23 p24 p25 p26 p27 p28 p29
                                   p30 p31)
      (declare (type (signed-byte 8) p0 p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15
                     p15 p17 p18 p19 p20 p21 p22 p23 p24 p25 p26 p27 p28 p29 p30 p31))
      (%make-simd-pack-256-ub16 (logior (ldb (byte 8 0) p0)  (ash (ldb (byte 8 0) p1) 8))
                                (logior (ldb (byte 8 0) p2)  (ash (ldb (byte 8 0) p3) 8))
                                (logior (ldb (byte 8 0) p4)  (ash (ldb (byte 8 0) p5) 8))
                                (logior (ldb (byte 8 0) p6)  (ash (ldb (byte 8 0) p7) 8))
                                (logior (ldb (byte 8 0) p8)  (ash (ldb (byte 8 0) p9) 8))
                                (logior (ldb (byte 8 0) p10) (ash (ldb (byte 8 0) p11) 8))
                                (logior (ldb (byte 8 0) p12) (ash (ldb (byte 8 0) p13) 8))
                                (logior (ldb (byte 8 0) p14) (ash (ldb (byte 8 0) p15) 8))
                                (logior (ldb (byte 8 0) p16) (ash (ldb (byte 8 0) p17) 8))
                                (logior (ldb (byte 8 0) p18) (ash (ldb (byte 8 0) p19) 8))
                                (logior (ldb (byte 8 0) p20) (ash (ldb (byte 8 0) p21) 8))
                                (logior (ldb (byte 8 0) p22) (ash (ldb (byte 8 0) p23) 8))
                                (logior (ldb (byte 8 0) p24) (ash (ldb (byte 8 0) p25) 8))
                                (logior (ldb (byte 8 0) p26) (ash (ldb (byte 8 0) p27) 8))
                                (logior (ldb (byte 8 0) p28) (ash (ldb (byte 8 0) p29) 8))
                                (logior (ldb (byte 8 0) p30) (ash (ldb (byte 8 0) p31) 8))))

  (defun %simd-pack-256-sb8s (pack)
    (declare (type simd-pack-256 pack))
    (multiple-value-bind (p0 p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15)
        (%simd-pack-256-sb16s pack)
      (values (sb-c::mask-signed-field 8 (ldb (byte 8 0) p0))
              (sb-c::mask-signed-field 8 (ash p0 -8))
              (sb-c::mask-signed-field 8 (ldb (byte 8 0) p1))
              (sb-c::mask-signed-field 8 (ash p1 -8))
              (sb-c::mask-signed-field 8 (ldb (byte 8 0) p2))
              (sb-c::mask-signed-field 8 (ash p2 -8))
              (sb-c::mask-signed-field 8 (ldb (byte 8 0) p3))
              (sb-c::mask-signed-field 8 (ash p3 -8))
              (sb-c::mask-signed-field 8 (ldb (byte 8 0) p4))
              (sb-c::mask-signed-field 8 (ash p4 -8))
              (sb-c::mask-signed-field 8 (ldb (byte 8 0) p5))
              (sb-c::mask-signed-field 8 (ash p5 -8))
              (sb-c::mask-signed-field 8 (ldb (byte 8 0) p6))
              (sb-c::mask-signed-field 8 (ash p6 -8))
              (sb-c::mask-signed-field 8 (ldb (byte 8 0) p7))
              (sb-c::mask-signed-field 8 (ash p7 -8))
              (sb-c::mask-signed-field 8 (ldb (byte 8 0) p8))
              (sb-c::mask-signed-field 8 (ash p8 -8))
              (sb-c::mask-signed-field 8 (ldb (byte 8 0) p9))
              (sb-c::mask-signed-field 8 (ash p9 -8))
              (sb-c::mask-signed-field 8 (ldb (byte 8 0) p10))
              (sb-c::mask-signed-field 8 (ash p10 -8))
              (sb-c::mask-signed-field 8 (ldb (byte 8 0) p11))
              (sb-c::mask-signed-field 8 (ash p11 -8))
              (sb-c::mask-signed-field 8 (ldb (byte 8 0) p12))
              (sb-c::mask-signed-field 8 (ash p12 -8))
              (sb-c::mask-signed-field 8 (ldb (byte 8 0) p13))
              (sb-c::mask-signed-field 8 (ash p13 -8))
              (sb-c::mask-signed-field 8 (ldb (byte 8 0) p14))
              (sb-c::mask-signed-field 8 (ash p14 -8))
              (sb-c::mask-signed-field 8 (ldb (byte 8 0) p15))
              (sb-c::mask-signed-field 8 (ash p15 -8))))))
