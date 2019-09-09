(in-package #:sb-simd)

;;; Most of this library is automatically generated from a set of tables.
;;; This file contains these tables.

(deftype type-specifier ()
  '(or symbol cons))

(defparameter *alphabet* '(a b c d e f g h i j k l m n o p q r s t u v w x y z))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Scalar Records

(defstruct (scalar-record
            (:copier nil)
            (:predicate scalar-record-p)
            (:conc-name scalar-record-))
  (name nil :type symbol :read-only t)
  (type nil :type type-specifier :read-only t)
  (bits nil :type unsigned-byte :read-only t))

(defmethod make-load-form ((scalar-record scalar-record) &optional environment)
  (make-load-form-saving-slots scalar-record :environment environment))

(defparameter *scalar-records*
  (loop for (name type bits)
          in '((u1 (unsigned-byte 1) 1)
               (u2 (unsigned-byte 2) 2)
               (u4 (unsigned-byte 4) 4)
               (u8 (unsigned-byte 8) 8)
               (u16 (unsigned-byte 16) 16)
               (u32 (unsigned-byte 32) 32)
               (u64 (unsigned-byte 64) 64)
               (s8 (signed-byte 8) 8)
               (s16 (signed-byte 16) 16)
               (s32 (signed-byte 32) 32)
               (s64 (signed-byte 64) 64)
               (f32 single-float 32)
               (f64 double-float 64))
        collect
        (make-scalar-record
         :name name
         :type type
         :bits bits)))

(defun find-scalar-records (&key (name nil name-supplied-p)
                              (type nil type-supplied-p)
                              (bits nil bits-supplied-p))
  (let ((records *scalar-records*))
    (when name-supplied-p
      (setf records (remove name records :key #'scalar-record-name :test-not #'eq)))
    (when type-supplied-p
      (setf records (remove type records :key #'scalar-record-type :test-not #'alexandria:type=)))
    (when bits-supplied-p
      (setf records (remove bits records :key #'scalar-record-bits :test-not #'=)))
    records))

(defun find-scalar-record (&rest args)
  (let ((records (apply #'find-simd-records args)))
    (cond ((null records)
           (error "No scalar record found for the query ~S"
                  args))
          ((null (rest records))
           (first records))
          (t
           (error "More than one scalar record found for the query ~S:~%~{~S~%~}"
                  args records)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SIMD Records

(defstruct (simd-record
            (:copier nil)
            (:predicate simd-record-p)
            (:conc-name simd-record-))
  (name nil :type symbol :read-only t)
  (type nil :type type-specifier :read-only t)
  (element-type nil :type type-specifier :read-only t)
  (width nil :type unsigned-byte :read-only t)
  (bits nil :type unsigned-byte :read-only t)
  (primitive-type nil :type symbol :read-only t)
  (pack nil :type symbol :read-only t)
  (unpack nil :type symbol :read-only t))

(defmethod make-load-form ((simd-record simd-record) &optional environment)
  (make-load-form-saving-slots simd-record :environment environment))

(defparameter *simd-records*
  (loop for (name type element-type width bits primitive-type pack unpack)
          in
          (append
           #+sb-simd-pack
           '((u64.2 (sb-ext:simd-pack u64) u64 2 128 sb-kernel:simd-pack-int sb-ext:%make-simd-pack-ub64 sb-ext:%simd-pack-ub64s)
             (f32.4 (sb-ext:simd-pack f32) f32 4 128 sb-kernel:simd-pack-single sb-ext:%make-simd-pack-single sb-ext:%simd-pack-singles)
             (f64.2 (sb-ext:simd-pack f64) f64 2 128 sb-kernel:simd-pack-double sb-ext:%make-simd-pack-double sb-ext:%simd-pack-doubles))
           #+sb-simd-pack-256
           '((u64.4 (sb-ext:simd-pack-256 u64) u64 4 256 sb-kernel:simd-pack-256-int sb-ext:%make-simd-pack-256-ub64 sb-ext:%simd-pack-256-ub64s)
             (f32.8 (sb-ext:simd-pack-256 f32) f32 8 256 sb-kernel:simd-pack-256-single sb-ext:%make-simd-pack-256-single sb-ext:%simd-pack-256-singles)
             (f64.4 (sb-ext:simd-pack-256 f64) f64 4 256 sb-kernel:simd-pack-256-double sb-ext:%make-simd-pack-256-double sb-ext:%simd-pack-256-doubles)))
        collect
        (make-simd-record
         :name name
         :type type
         :element-type element-type
         :width width
         :bits bits
         :primitive-type primitive-type
         :pack pack
         :unpack unpack)))

(defun find-simd-records (&key (name nil name-supplied-p)
                            (type nil type-supplied-p)
                            (element-type nil element-type-supplied-p)
                            (width nil width-supplied-p)
                            (bits nil bits-supplied-p))
  (let ((records *simd-records*))
    (when name-supplied-p
      (setf records (remove name records :key #'simd-record-name :test-not #'eq)))
    (when type-supplied-p
      (setf records (remove type records :key #'simd-record-type :test-not #'alexandria:type=)))
    (when element-type-supplied-p
      (setf records (remove element-type records :key #'simd-record-element-type :test-not #'subtypep)))
    (when width-supplied-p
      (setf records (remove width records :key #'simd-record-width :test-not #'=)))
    (when bits-supplied-p
      (setf records (remove bits records :key #'simd-record-bits :test-not #'=)))
    records))

(defun find-simd-record (&rest args)
  (let ((records (apply #'find-simd-records args)))
    (cond ((null records)
           (error "No SIMD record found for the parameters ~S"
                  args))
          ((null (rest records))
           (first records))
          (t
           (error "More than one SIMD record found for the parameters ~S:~%~{~S~%~}"
                  args records)))))
