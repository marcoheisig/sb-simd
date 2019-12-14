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
