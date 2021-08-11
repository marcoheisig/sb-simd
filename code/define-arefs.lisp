(in-package #:sb-simd-internals)

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
;; computations where the number of subscripts is known at compile time are
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

(defun setf-row-major-aref (v a i)
  (setf (row-major-aref a i) v))

(define-compiler-macro setf-row-major-aref (v a i)
  (let* ((v-binding `(,(gensym "VALUE") ,v))
         (v (first v-binding)))
    `(let (,v-binding)
       (setf (row-major-aref ,a ,i) ,v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Array Load and Store Instructions

(defmacro define-aref (load-record-name)
  (with-accessors ((load load-record-name)
                   (aref load-record-aref)
                   (row-major-aref load-record-row-major-aref)
                   (value-record load-record-value-record))
      (find-instruction-record load-record-name)
    (let* ((scalar-record
             (etypecase value-record
               (scalar-record value-record)
               (simd-record (simd-record-scalar-record value-record))))
           (simd-width
             (etypecase value-record
               (scalar-record 1)
               (simd-record (simd-record-size value-record))))
           (element-type (scalar-record-name scalar-record)))
      `(progn
         (define-inline ,row-major-aref (array index)
           (declare (type (array ,element-type) array)
                    (index index))
           (,load array index))
         (defun ,aref (array &rest indices)
           (declare (type (array ,element-type) array))
           (,load
            array
            (apply #'array-row-major-simd-index array ,simd-width indices)))
         (define-compiler-macro ,aref (array &rest indices)
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

(defmacro define-setf-aref (store-record-name)
  (with-accessors ((store store-record-name)
                   (aref store-record-aref)
                   (row-major-aref store-record-row-major-aref)
                   (value-record store-record-value-record))
      (find-instruction-record store-record-name)
    (let* ((value-type (value-record-name value-record))
           (scalar-record
             (etypecase value-record
               (scalar-record value-record)
               (simd-record (simd-record-scalar-record value-record))))
           (simd-width
             (etypecase value-record
               (scalar-record 1)
               (simd-record (simd-record-size value-record))))
           (element-type (scalar-record-name scalar-record)))
      `(progn
         (define-inline (setf ,row-major-aref) (value array index)
           (declare (type (array ,element-type) array)
                    (index index))
           (,store (,value-type value) array index))
         (defun (setf ,aref) (value array &rest indices)
           (declare (type (array ,element-type) array))
           (,store
            (,value-type value)
            array
            (apply #'array-row-major-simd-index array ,simd-width indices)))
         (define-compiler-macro (setf ,aref) (value array &rest indices)
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
                  (,',store (,',value-type ,value) ,array ,index)))))))))

(defmacro define-arefs ()
  `(progn
     ,@(loop for load-record in (filter-instruction-records #'load-record-p)
             for name = (load-record-name load-record)
             collect `(define-aref ,name))
     ,@(loop for store-record in (filter-instruction-records #'store-record-p)
             for name = (store-record-name store-record)
             collect `(define-setf-aref ,name))))

(define-arefs)

(in-package #:sb-simd-common)

;;; New we define reffers for scalars.  Those can be defined simply in
;;; terms of built-in Common Lisp functions.
(macrolet ((define-reffer (type aref row-major-aref)
             `(progn
                (define-inline ,row-major-aref (array index)
                  (declare (type (array ,type) array))
                  (row-major-aref array index))
                (define-inline (setf ,row-major-aref) (value array index)
                  (declare (type ,type value)
                           (type (array ,type) array))
                  (setf (row-major-aref array index) value))
                (defun ,aref (array &rest subscripts)
                  (declare (type (array ,type) array))
                  (apply #'aref array subscripts))
                (defun (setf ,aref) (value array &rest subscripts)
                  (declare (type ,type value)
                           (type (array ,type) array))
                  (setf (apply #'aref array subscripts) value))
                (define-compiler-macro ,aref (array &rest subscripts)
                  `(aref (the (array ,',type) ,array) ,@subscripts))
                (define-compiler-macro (setf ,aref) (value array &rest subscripts)
                  (let ((v (gensym "VALUE")))
                    `(let ((,v ,value))
                       (setf (aref (the (array ,',type) ,array) ,@subscripts)
                             ,v)))))))
  (define-reffer f32 f32-aref f32-row-major-aref)
  (define-reffer f64 f64-aref f64-row-major-aref)
  (define-reffer  u8  u8-aref  u8-row-major-aref)
  (define-reffer u16 u16-aref u16-row-major-aref)
  (define-reffer u32 u32-aref u32-row-major-aref)
  (define-reffer u64 u64-aref u64-row-major-aref)
  (define-reffer  s8  s8-aref  s8-row-major-aref)
  (define-reffer s16 s16-aref s16-row-major-aref)
  (define-reffer s32 s32-aref s32-row-major-aref)
  (define-reffer s64 s64-aref s64-row-major-aref))
