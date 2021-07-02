(in-package #:sb-simd)

;;; A value record describes a specialized set of Common Lisp objects.
;;; Each value record consists of a Common Lisp type specifier, a
;;; corresponding primitive type specifier used by SBCL's VM, the number of
;;; bits required to represent all such objects, and a list of storage
;;; classes in which such objects can be stored.

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
  ;; A list of storage classes where this value can be placed.
  (scs nil :type list))

;;; A hash table, mapping from value record names to value records.
(declaim (hash-table *value-records*))
(defparameter *value-records* (make-hash-table :test #'eq))

(defun find-value-record (name)
  (or (gethash name *value-records*)
      (error "There is no value record with the name ~S."
             name)))

(defun value-record-name-p (name)
  (nth-value 1 (gethash name *value-records*)))

;; Interns a string designator into the SB-VM package, while gracefully
;; handling the case where the symbol is not present.
(defun find-sc (sc)
  (or (find-symbol (string sc) "SB-VM")
      'sb-vm::descriptor-reg))

;; Interns a string designator into the SB-VM package, while also accepting
;; special compound primitive type designators.  The latter is mainly used
;; for primitive types like (:CONSTANT TYPE).
(defun find-primitive-type (x)
  (typecase x
    (symbol (find-symbol (string x) "SB-VM"))
    (cons x)
    (otherwise 't)))

;;; Scalar Record

(defstruct (scalar-record
            (:include value-record)
            (:copier nil)
            (:constructor make-scalar-record
                (&key name bits type primitive-type scs))))

(defmacro define-scalar-record
    (name bits type primitive-type &optional (scs '(#:descriptor-reg)))
  `(setf (gethash ',name *value-records*)
         (make-scalar-record
          :name ',name
          :bits ,bits
          :type ',type
          :primitive-type ',(find-primitive-type primitive-type)
          :scs ',(mapcar #'find-sc scs))))

(defmacro define-scalar-records (&body rows)
  `(progn ,@(loop for row in rows collect `(define-scalar-record ,@row))))

;;; SIMD Record

(defstruct (simd-record
            (:include value-record)
            (:copier nil)
            (:constructor make-simd-record
                (&key name type primitive-type scs scalar-record size
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

(defmacro define-simd-record (name scalar-record-name bits primitive-type scs)
  (let ((simd-pack-type
          (let ((base-type
                  (ecase bits
                    (128 (find-symbol "SIMD-PACK" "SB-EXT"))
                    (256 (find-symbol "SIMD-PACK-256" "SB-EXT")))))
            (cond ((not base-type) 't)
                  ((not scalar-record-name) base-type)
                  (t `(,base-type ,scalar-record-name))))))
    `(let ((.scalar-record. (find-value-record ',(or scalar-record-name 'u64))))
       (setf (gethash ',name *value-records*)
             (make-simd-record
              :name ',name
              :scalar-record .scalar-record.
              :size (the unsigned-byte (/ ,bits (scalar-record-bits .scalar-record.)))
              :type ',simd-pack-type
              :primitive-type ',(find-primitive-type primitive-type)
              :scs ',(mapcar #'find-sc scs))))))

(defmacro define-simd-records (&body rows)
  `(progn ,@(loop for row in rows collect `(define-simd-record ,@row))))
