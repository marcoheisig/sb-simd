(in-package #:sb-vm)

;; SSE
#-sb-xc-host
(progn
  (declaim (inline %make-simd-pack-sb64 %make-simd-pack-sb32
                   %simd-pack-sb32s %simd-pack-sb64s))
  (defun %make-simd-pack-sb64 (w x)
    (declare (type (signed-byte 64) w x))
    (%make-simd-pack-ub64 (ldb (byte 64 0) w)
                          (ldb (byte 64 0) x)))
    
  (defun %make-simd-pack-sb32 (w x y z)
    (declare (type (signed-byte 32) w x y z))
    (%make-simd-pack-ub64 (logior (ldb (byte 32 0) w)
                                  (ash (ldb (byte 32 0) x) 32))
                          (logior (ldb (byte 32 0) y)
                                  (ash (ldb (byte 32 0) z) 32))))
  
  (defun %simd-pack-sb32s (pack)
    (declare (type simd-pack pack))
    (let ((lo (%simd-pack-low pack))
          (hi (%simd-pack-high pack)))
      (sb-ext:without-package-locks
        (values (sb-c::mask-signed-field 32 (ldb (byte 32 0) lo))
                (sb-c::mask-signed-field 32 (ash lo -32))
                (sb-c::mask-signed-field 32 (ldb (byte 32 0) hi))
                (sb-c::mask-signed-field 32 (ash hi -32))))))

  (defun %simd-pack-sb64s (pack)
    (declare (type simd-pack pack))
    (sb-ext:without-package-locks
      (values (sb-c::mask-signed-field 64 (%simd-pack-low pack))
              (sb-c::mask-signed-field 64 (%simd-pack-high pack))))))

;; AVX
#-sb-xc-host
(progn
  (declaim (inline %make-simd-pack-256-sb64 %make-simd-pack-256-sb32
                   %simd-pack-256-sb32s %simd-pack-256-sb64s))
  (defun %make-simd-256-pack-sb64 (p0 p1 p2 p3)
    (declare (type (signed-byte 64) p0 p1 p2 p3))
    (%make-simd-pack-ub64 (ldb (byte 64 0) p0)
                          (ldb (byte 64 0) p1)
                          (ldb (byte 64 0) p2)
                          (ldb (byte 64 0) p3)))
    
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
      (sb-ext:without-package-locks
        (values (sb-c::mask-signed-field 32 (ldb (byte 32 0) p0))
                (sb-c::mask-signed-field 32 (ash p0 -32))
                (sb-c::mask-signed-field 32 (ldb (byte 32 0) p1))
                (sb-c::mask-signed-field 32 (ash p1 -32))
                (sb-c::mask-signed-field 32 (ldb (byte 32 0) p2))
                (sb-c::mask-signed-field 32 (ash p2 -32))
                (sb-c::mask-signed-field 32 (ldb (byte 32 0) p3))
                (sb-c::mask-signed-field 32 (ash p3 -32))))))

  (defun %simd-pack-256-sb64s (pack)
    (declare (type simd-pack-256 pack))
    (sb-ext:without-package-locks
      (values (sb-c::mask-signed-field 64 (%simd-pack-256-0 pack))
              (sb-c::mask-signed-field 64 (%simd-pack-256-1 pack))
              (sb-c::mask-signed-field 64 (%simd-pack-256-2 pack))
              (sb-c::mask-signed-field 64 (%simd-pack-256-3 pack))))))
