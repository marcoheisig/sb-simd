(in-package #:sb-simd)

;;; For each SIMD type X, we need:
;;;
;;; - The functions X-{AREF,ROW-MAJOR-AREF} and (SETF X-{AREF,ROW-MAJOR-AREF}).
;;;
;;; - The functions X-DATA-VECTOR-{REF,SET} and X-HAIRY-DATA-VECTOR-{REF,SET}.
;;;
;;; - Type derivation optimizers and defknowns for all these functions.
;;;
;;; - The VOPSs data-vector-ref-with-offset[-c]/X and data-vector-set-with-offset[-c]/X

(macrolet ((defload (name result-type)
             `(progn
                (sb-c:defknown ,name ((array double-float) sb-c::index) ,result-type
                    (sb-c:foldable) :overwrite-fndb-silently t)
                (sb-c:defoptimizer (,name derive-type) ((array index))
                  (declare (ignore array index))
                  (sb-kernel:specifier-type ',result-type)))))
  (defload f64.4-row-major-aref f64.4)
  (defload f64.4-hairy-data-vector-ref f64.4)
  (defload f64.4-data-vector-ref f64.4))

(sb-c:deftransform f64.4-row-major-aref ((array index))
  `(f64.4-hairy-data-vector-ref
    (the (array double-float) array)
    index
    #+(or)
    (sb-c::check-bound array (- (array-total-size array) 3) index)))

(sb-c:deftransform f64.4-hairy-data-vector-ref ((array index) (simple-array t) *)
  `(multiple-value-bind (array index) (sb-c::%data-vector-and-index array index)
    (f64.4-data-vector-ref array index)))

(sb-c:deftransform f64.4-data-vector-ref ((array index) ((simple-array double-float) t))
  (let ((dims (sb-c::array-type-dimensions (sb-c::lvar-type array))))
    (when (or (atom dims) (= (length dims) 1))
      (sb-c::give-up-ir1-transform))
    (let ((total-size (if (member '* dims) '* (reduce #'* dims))))
      `(data-vector-ref (sb-c::truly-the (simple-array double-float (,total-size))
                                         (sb-c::%array-data array))
                        index))))

(sb-c:define-source-transform f64.4-data-vector-ref (array index)
  `(f64.4-data-vector-ref-with-offset ,array ,index 0))

(defun f64.4-row-major-aref (array index)
  (f64.4-row-major-aref array index))

(defun f64.4-hairy-data-vector-ref (array index)
  (etypecase array
    ((simple-array double-float)
     (f64.4-hairy-data-vector-ref array index))
    (t
     (sb-c::with-array-data ((vector array)
                             (index index)
                             (end) :force-inline t)
       (declare (ignore end))
       (f64.4-hairy-data-vector-ref vector index)))))

(macrolet ((defstore (name result-type)
             `(progn
                (sb-c:defknown ,name (array sb-c::index ,result-type) ,result-type
                    () :overwrite-fndb-silently t)
                (sb-c:defoptimizer (,name derive-type) ((array index value))
                  (declare (ignore array index))
                  (let ((simd-type (sb-kernel:specifier-type ',result-type))
                        (policy (sb-c::lexenv-policy
                                 (sb-c::node-lexenv
                                  (sb-c::lvar-dest value)))))
                    (sb-c::assert-lvar-type value simd-type policy)
                    simd-type)))))
  (defstore f64.4-set-row-major-aref f64.4)
  (defstore f64.4-hairy-data-vector-set f64.4)
  (defstore f64.4-data-vector-set f64.4))

(sb-c:deftransform f64.4-set-row-major-aref ((array index value))
  `(f64.4-hairy-data-vector-set
    (the (array double-float) array)
    (sb-c::check-bound array (- (array-total-size array) 3) index)
    value))

(defun f64.4-set-row-major-aref (array index value)
  (setf (row-major-aref array index) value))

(defsetf f64.4-row-major-aref f64.4-set-row-major-aref)

(defun f64.4-aref (array &rest subscripts)
  (declare (sb-int:truly-dynamic-extent subscripts)
           (type (array f64) array))
  (f64.4-row-major-aref array (apply #'array-row-major-index array subscripts)))

(defun (setf f64.4-aref) (value array &rest subscripts)
  (declare (sb-int:truly-dynamic-extent subscripts)
           (type (array f64) array))
  (setf (f64.4-row-major-aref array (apply #'array-row-major-index array subscripts))
        value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-modify-macro f64.4-incf (&optional (num (make-f64.4 1d0 1d0 1d0 1d0))) f64.4+)
  (define-modify-macro f64.2-incf (&optional (num (make-f64.2 1d0 1d0))) f64.2+)
  (define-modify-macro f32.4-incf (&optional (num (make-f32.4 1f0 1f0 1f0 1f0))) f32.4+)
  (define-modify-macro f32.8-incf (&optional (num (make-f32.8 1f0 1f0 1f0 1f0 1f0 1f0 1f0 1f0))) f32.8+)

  (declaim (ftype (function ((simple-array double-float (*))
							 (integer 0 #.most-positive-fixnum))
							f64.4) f64.4-ref))
  (define-inline f64.4-ref (v i)
	(declare (optimize (speed 3) (safety 0) (debug 0))
			 (type (simple-array double-float (*)) v)
			 (type (integer 0 #.most-positive-fixnum) i))
	(sb-vm::%f64.4-ref v i))

  (declaim (ftype (function (f64.4 (simple-array double-float (*))
			       (integer 0 #.most-positive-fixnum))
			  f64.4) (setf f64.4-ref)))
  (define-inline (setf f64.4-ref) (new-value v i)
    (declare (optimize (speed 3) (safety 0) (debug 0))
			 (type (simple-array double-float (*)) v)
			 (type (integer 0 #.most-positive-fixnum) i)
			 (type f64.4 new-value))
    (sb-vm::%f64.4-set v i new-value))

  (declaim (ftype (function ((simple-array double-float (*))
							 (integer 0 #.most-positive-fixnum))
							f64.2) f64.2-ref))
  (define-inline f64.2-ref (v i)
    (declare (optimize (speed 3) (safety 0) (debug 0))
			 (type (simple-array double-float (*)) v)
             (type (integer 0 #.most-positive-fixnum) i))
    (sb-vm::%f64.2-ref v i))

  (declaim (ftype (function (f64.2
							 (simple-array double-float (*))
							 (integer 0 #.most-positive-fixnum))
							f64.2) (setf f64.2-ref)))
  (define-inline (setf f64.2-ref) (new-value v i)
    (declare (optimize (speed 3) (safety 0) (debug 0))
			 (type (simple-array double-float (*)) v)
			 (type (integer 0 #.most-positive-fixnum) i)
             (type f64.2 new-value))
    (sb-vm::%f64.2-set v i new-value))

  (declaim (ftype (function ((simple-array single-float (*))
							 (integer 0 #.most-positive-fixnum))
							f32.4) f32.4-ref))
  (define-inline f32.4-ref (v i)
    (declare (optimize (speed 3) (safety 0) (debug 0))
			 (type (simple-array single-float (*)) v)
             (type (integer 0 #.most-positive-fixnum) i))
    (sb-vm::%f32.4-ref v i))

  (declaim (ftype (function (f32.4
							 (simple-array single-float (*))
							 (integer 0 #.most-positive-fixnum))
							f32.4) (setf f32.4-ref)))
  (define-inline (setf f32.4-ref) (new-value v i)
    (declare (optimize (speed 3) (safety 0) (debug 0))
			 (type (simple-array single-float (*)) v)
			 (type (integer 0 #.most-positive-fixnum) i)
             (type f32.4 new-value))
    (sb-vm::%f32.4-set v i new-value))

  (declaim (ftype (function () (integer 0 #.most-positive-fixnum)) vzeroupper))
  (define-inline vzeroupper ()
	(declare (optimize (speed 3) (safety 0) (debug 0)))
	(sb-vm::%vzeroupper)))
