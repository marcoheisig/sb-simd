(in-package #:sb-simd)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Auxiliary Functions and Macros

(defun index+ (&rest indices)
  (the index (apply #'+ indices)))

(define-compiler-macro index+ (&rest indices)
  `(sb-ext:truly-the
    index
    (+ ,@(loop for index in indices collect `(sb-ext:truly-the index ,index)))))

(defun index* (&rest indices)
  (the index (apply #'* indices)))

(define-compiler-macro index* (&rest indices)
  `(sb-ext:truly-the
    index
    (* ,@(loop for index in indices collect `(sb-ext:truly-the index ,index)))))

(declaim (notinline wrong-number-of-subscripts))
(defun wrong-number-of-subscripts (array number-of-subscripts)
  (error "Wrong number of subcripts, ~S, for an array of rank ~S."
         number-of-subscripts
         (array-rank array)))

(declaim (notinline invalid-subscript))
(defun invalid-subscript (subscript array axis limit)
  (declare (ignore array))
  (error "Invalid array subscript ~S for axis ~S, ~
          should be a non-negative integer below ~S."
         subscript axis limit))

;; This function doesn't have to be particularly fast, because all index
;; computations where the number of subscripts is known at compile-time are
;; expanded by compiler macros and WITH-ROW-MAJOR-SIMD-INDEX.  The only way
;; to reach this function is when an array indexing function is supplied as
;; the first argument to APPLY or FUNCALL.
(defun array-row-major-simd-index (array simd-width &rest subscripts)
  (let ((rank (array-rank array))
        (length (length subscripts)))
    (unless (= rank length)
      (wrong-number-of-subscripts array length))
    (let ((stride 1)
          (index 0))
      (declare (index stride index))
      (loop for axis from (1- rank) downto 0
            for subscript = (nth axis subscripts)
            for dimension = (array-dimension array axis)
            for width = simd-width then 1 do
              (unless (<= -1 subscript (- dimension width))
                (invalid-subscript subscript array axis (1+ (- dimension width))))
              (incf index (* stride subscript))
              (setf stride (* stride dimension)))
      index)))

(defmacro with-row-major-simd-index
    ((index array simd-width &rest indices) &body body &environment env)
  (check-type index symbol)
  (check-type array symbol)
  (check-type simd-width (integer 1))
  (dolist (index indices)
    (check-type index symbol))
  (let* ((length (length indices))
         (rank-binding `(,(gensym "RANK") (array-rank ,array)))
         (rank (first rank-binding))
         (dimension-bindings
           (loop for axis below length
                 collect `(,(gensym "DIMENSION") (array-dimension ,array ,axis))))
         (dimensions (mapcar #'first dimension-bindings))
         (stride-bindings
           (loop for axis from (- length 2) downto 0
                 for old-stride = nil then new-stride
                 for new-stride = (gensym "STRIDE")
                 for stride-binding = `(,new-stride ,(nth axis dimensions))
                   then `(,new-stride (index* ,(nth axis dimensions) ,old-stride))
                 collect stride-binding))
         (strides (reverse (mapcar #'first stride-bindings)))
         (index-form
           `(index+
             ,@(loop for stride in strides
                     for index in indices
                     collect `(index* ,stride ,index))
             ,(first (last indices)))))
    `(let (,rank-binding)
       (unless (= ,rank ,length)
         (wrong-number-of-subscripts ,array ,length))
       (let (,@dimension-bindings)
         (declare (ignorable ,@dimensions))
         ,@(when (sb-c:policy env (plusp sb-c::insert-array-bounds-checks))
             (loop for axis from 0
                   for dimension in dimensions
                   for index in indices
                   for limit = (if (= axis (1- length)) `(- ,dimension ,(1- simd-width)) dimension)
                   collect `(unless (< -1 ,index ,limit)
                              (invalid-subscript ,index ,array ,axis ,limit))))
         (let (,@stride-bindings)
           (let ((,index ,index-form))
             ,@body))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Array Load and Store Instructions

(defmacro define-load (ref row-major-ref load)
  (let* ((instruction-record (find-instruction-record load))
         (value-record (first (instruction-record-result-records instruction-record)))
         (scalar-record
           (etypecase value-record
             (scalar-record value-record)
             (simd-record (simd-record-scalar-record value-record))))
         (simd-width
           (etypecase value-record
             (scalar-record 1)
             (simd-record (simd-record-size value-record))))
         (element-type (scalar-record-name scalar-record)))
    (when (instruction-record-supported-p instruction-record)
      `(progn
         (export ',row-major-ref)
         (export ',ref)
         (define-inline ,row-major-ref (array index)
           (declare (type (array ,element-type) array)
                    (index index))
           (,load array index))
         (defun ,ref (array &rest indices)
           (declare (type (array ,element-type) array))
           (,load
            array
            (apply #'array-row-major-simd-index array ,simd-width indices)))
         (define-compiler-macro ,ref (array &rest indices)
           (let* ((index (gensym "INDEX"))
                  (array-binding `(,(gensym "ARRAY") ,array))
                  (index-bindings
                    (loop for index-form in indices
                          collect `(,(gensym "INDEX") ,index-form)))
                  (array (first array-binding))
                  (indices (mapcar #'first index-bindings)))
             `(let (,array-binding ,@index-bindings)
                (declare (type (array ,',element-type) ,array))
                (with-row-major-simd-index (,index ,array ,',simd-width ,@indices)
                  (,',load ,array ,index)))))))))

(defmacro define-store (ref row-major-ref store)
  (let* ((instruction-record (find-instruction-record store))
         (value-record (first (instruction-record-result-records instruction-record)))
         (scalar-record
           (etypecase value-record
             (scalar-record value-record)
             (simd-record (simd-record-scalar-record value-record))))
         (simd-width
           (etypecase value-record
             (scalar-record 1)
             (simd-record (simd-record-size value-record))))
         (element-type (scalar-record-name scalar-record)))
    (when (instruction-record-supported-p instruction-record)
      `(progn
         (export ',row-major-ref)
         (export ',ref)
         (define-inline (setf ,row-major-ref) (value array index)
           (declare (type (array ,element-type) array)
                    (index index))
           (,store value array index))
         (defun (setf ,ref) (value array &rest indices)
           (declare (type (array ,element-type) array))
           (,store
            value
            array
            (apply #'array-row-major-simd-index array ,simd-width indices)))
         (define-compiler-macro (setf ,ref) (value array &rest indices)
           (let* ((value-binding `(,(gensym "VALUE") ,value))
                  (array-binding `(,(gensym "ARRAY") ,array))
                  (index-bindings
                    (loop for index-form in indices
                          collect `(,(gensym "INDEX") ,index-form)))
                  (indices (mapcar #'first index-bindings))
                  (value (first value-binding))
                  (array (first array-binding))
                  (index (gensym "INDEX")))
             `(let (,value-binding ,array-binding ,@index-bindings)
                (declare (type (array ,',element-type) ,array))
                (with-row-major-simd-index (,index ,array ,',simd-width ,@indices)
                  (,',store ,value ,array ,index)))))))))

(define-load f32.4-aref f32.4-row-major-aref f32.4-load)
(define-load f64.2-aref f64.2-row-major-aref f64.2-load)
(define-load f32.8-aref f32.8-row-major-aref f32.8-load)
(define-load f64.4-aref f64.4-row-major-aref f64.4-load)
(define-load u32.4-aref u32.4-row-major-aref u32.4-load)
(define-load u64.2-aref u64.2-row-major-aref u64.2-load)
(define-load u32.8-aref u32.8-row-major-aref u32.8-load)
(define-load u64.4-aref u64.4-row-major-aref u64.4-load)

(define-store f32.4-aref f32.4-row-major-aref f32.4-store)
(define-store f64.2-aref f64.2-row-major-aref f64.2-store)
(define-store f32.8-aref f32.8-row-major-aref f32.8-store)
(define-store f64.4-aref f64.4-row-major-aref f64.4-store)
(define-store u32.4-aref u32.4-row-major-aref u32.4-store)
(define-store u64.2-aref u64.2-row-major-aref u64.2-store)
(define-store u32.8-aref u32.8-row-major-aref u32.8-store)
(define-store u64.4-aref u64.4-row-major-aref u64.4-store)
