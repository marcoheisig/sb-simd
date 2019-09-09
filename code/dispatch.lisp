(in-package #:sb-simd)

(defmacro scalar-dispatch (scalars &body clauses)
  (compute-dispatch-expansion
   :inputs scalars
   :clauses clauses
   :type-wrapper #'values
   :simd-width (length scalars)
   :initial-state 'u64
   :states
   '((u64 (f64 f64) (u64 u64) (real f32))
     (f32 (f64 f64) (real f32))
     (f64 (real f64)))))

(defmacro array-dispatch (width arrays &body clauses)
  (compute-dispatch-expansion
   :inputs arrays
   :clauses clauses
   :type-wrapper (lambda (type width) (declare (ignore width)) `(array ,type))
   :simd-width width
   :initial-state 'u64
   :states
   '((u64 (f64 f64) (u64 u64) (real f32))
     (f32 (f64 f64) (real f32))
     (f64 (real f64)))))

(defmacro simd-dispatch (width simd-packs &body clauses)
  (compute-dispatch-expansion
   :inputs simd-packs
   :clauses clauses
   :type-wrapper (lambda (type width)
                   (loop for record in *simd-records*
                         when (and (= (simd-width record) width)
                                   (subtypep type (simd-element-type record)))
                           do (return (simd-name record))))
   :simd-width width
   :initial-state 'u64
   :states
   '((u64 (u64 u64) (f32 f32) (f64 f64))
     (f32 (u64 f32) (f32 f32) (f64 f64))
     (f64 (u64 f64) (f32 f64) (f64 f64)))))

(defun find-simd-type (element-type simd-width)
  (loop for record in *simd-records*
        when (and (= (simd-width record) simd-width)
                  (subtypep element-type (simd-element-type record)))
          do (return (simd-name record))))

(defun compute-dispatch-expansion
    (&key inputs clauses type-wrapper simd-width initial-state states)
  (let ((bindings (mapcar (lambda (input)
                            (check-type input symbol)
                            `(,(gensym "INPUT") ,input))
                          inputs)))
    (labels ((wrap-type (type)
               (funcall type-wrapper type simd-width))
             (expand-state (state variables)
               (destructuring-bind (type &rest transitions)
                   (find state states :key #'first)
                 (if (null variables)
                     (let ((clause (find type clauses :key #'first)))
                       (if (or (not clause)
                               (not (find-simd-type type simd-width)))
                           (values '(values) nil)
                           (values `(let ,(mapcar #'reverse bindings) ,@(rest clause)) t)))
                     (let ((clauses '()))
                       (loop for (from-types to-type) in (group-transitions transitions) do
                         (multiple-value-bind (form validp)
                             (expand-state to-type (rest variables))
                           (unless (not validp)
                             (let ((type (type-or (mapcar #'wrap-type from-types))))
                               (unless (subtypep type nil)
                                 (pushnew `(,type ,form) clauses :key #'first))))))
                       (if (null clauses)
                           (values '(values) nil)
                           (values `(etypecase ,(first variables) ,@(reverse clauses)) t)))))))
      `(let ,bindings
         ,(expand-state initial-state (mapcar #'first bindings))))))

(defun group-transitions (transitions)
  (let ((target-types (remove-duplicates (mapcar #'second transitions))))
    (loop for target-type in target-types
          collect
          (list
           (loop for (from to) in transitions
                 when (equal to target-type)
                   collect from)
           target-type))))

(defun type-or (types)
  (setf types (remove nil types))
  (setf types (remove-duplicates types :test #'equal))
  (cond ((null types) nil)
        ((null (rest types)) (first types))
        (t `(or ,@types))))
