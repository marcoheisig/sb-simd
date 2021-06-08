(in-package #:sb-simd)

;;; A value record describes a specialized set of Common Lisp objects.
;;; Each value record consists of a Common Lisp type specifier, a
;;; corresponding primitive type specifier used by SBCL's VM, the number of
;;; bits required to represent all such objects, and a register in which
;;; such objects can be stored.

(defstruct (value-record
            (:copier nil))
  ;; The value's name.
  (name nil :type non-nil-symbol :read-only t)
  ;; The Common Lisp type of this value.
  (type nil :type type-specifier :read-only t)
  ;; The primitive type of this value as used by SBCL's VM.
  (primitive-type nil :type type-specifier :read-only t)
  ;; The minimum number of bits that are necessary to represent this value
  ;; in memory.
  (bits nil :type (unsigned-byte 16) :read-only t)
  ;; The name of the most specialized VM register that can hold this value.
  (register nil :type symbol))

;;; A hash table, mapping from value record names to value records.
(declaim (hash-table *value-records*))
(defparameter *value-records* (make-hash-table :test #'eq))

(defun find-value-record (name)
  (or (gethash name *value-records*)
      (error "There is no value record with the name ~S."
             name)))

(defun value-record-name-p (name)
  (nth-value 1 (gethash name *value-records*)))

;;; Scalar Record

(defstruct (scalar-record
            (:include value-record)
            (:copier nil)
            (:constructor make-scalar-record
                (&key name bits type primitive-type register))))

(defmacro define-scalar-record
    (name bits type primitive-type &optional (register '#:descriptor-reg))
  (let ((primitive-type (find-symbol (string primitive-type) "SB-VM"))
        (register (find-symbol (string register) "SB-VM")))
    `(setf (gethash ',name *value-records*)
           (make-scalar-record
            :name ',name
            :bits ,bits
            :type ',type
            :primitive-type ',primitive-type
            :register ',register))))

(defmacro define-scalar-records (&body rows)
  `(progn ,@(loop for row in rows collect `(define-scalar-record ,@row))))

;;; SIMD Record

(defstruct (simd-record
            (:include value-record)
            (:copier nil)
            (:constructor make-simd-record
                (&key name type primitive-type register scalar-record size
                   packer primitive-packer unpacker primitive-unpacker
                 &aux
                   (bits (* size (scalar-record-bits scalar-record))))))
  ;; The scalar record of the elements of this SIMD pack.
  (scalar-record nil :type scalar-record :read-only t)
  ;; The number of scalar elements in this SIMD pack.
  (size nil :type unsigned-byte :read-only t)
  ;; The name of the function for turning values into a SIMD pack.
  (packer nil :type symbol :read-only t)
  ;; The name of SBCL's primitive function for packing.
  (primitive-packer nil :type symbol :read-only t)
  ;; The name of the function for extracting the values of a SIMD pack.
  (unpacker nil :type symbol :read-only t)
  ;; The name of SBCL's primitive function for unpacking.
  (primitive-unpacker nil :type symbol :read-only t))

(defmacro define-simd-record (name scalar-record-name bits primitive-type register primitive-packer primitive-unpacker)
  (let ((packer (mksym "SB-SIMD" "MAKE-" name))
        (unpacker (mksym "SB-SIMD" name "-VALUES"))
        (scalar-record (gensym "SCALAR-RECORD"))
        (primitive-type
          (or (find-symbol (string primitive-type) "SB-VM")
              t))
        (register
          (or (find-symbol (string register) "SB-VM")
              'sb-vm::descriptor-reg))
        (primitive-packer
          (or (find-symbol (string primitive-packer) "SB-EXT")
              'missing-instruction))
        (primitive-unpacker
          (or (find-symbol (string primitive-unpacker) "SB-EXT")
              'missing-instruction))
        (simd-pack-type
          (let ((base-type
                  (ecase bits
                    (128 (find-symbol "SIMD-PACK" "SB-EXT"))
                    (256 (find-symbol "SIMD-PACK-256" "SB-EXT")))))
            (if (not base-type)
                't
                `(,base-type ,scalar-record-name)))))
    `(let ((,scalar-record (find-value-record ',scalar-record-name)))
       (setf (gethash ',name *value-records*)
             (make-simd-record
              :name ',name
              :scalar-record ,scalar-record
              :size (the unsigned-byte (/ ,bits (scalar-record-bits ,scalar-record)))
              :type ',simd-pack-type
              :primitive-type ',primitive-type
              :register ',register
              :packer ',packer
              :primitive-packer ',primitive-packer
              :unpacker ',unpacker
              :primitive-unpacker ',primitive-unpacker)))))

(defmacro define-simd-records (&body rows)
  `(progn ,@(loop for row in rows collect `(define-simd-record ,@row))))
