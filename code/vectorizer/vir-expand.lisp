(in-package #:sb-simd-internals)

;;; Turn all the expressions that have been converted in the current
;;; vectorizer context into a vectorized, unrolled loop.

(defun vir-expand ()
  (let ((width (vir-max-simd-width *vectorizer-context*))
        (unroll *vir-unroll*)
        (var *vir-variable*)
        (body (vir-vectorize 1)))
    (multiple-value-bind (vec-body)
        (vir-vectorize width)
      (sb-int:with-unique-names (start end vend)
        `(let* ((,start ,*vir-start*)
                (,end ,*vir-end*)
                (,var ,start)
                (,vend (- ,end ,(* width unroll))))
           (declare (index ,var) (ignorable ,vend))
           #+(or)
           ,@(when optimize-unsafely
               '((declare (optimize (safety 0) (debug 0) (compilation-speed 0) (speed 3)))
                 (declare (optimize (sb-c::insert-array-bounds-checks 0)))))
           ;; The unrolled, vectorized loop.
           ,@(unless (= unroll width 1)
               `((loop
                   (unless (< ,var ,vend) (return))
                   ,@(loop repeat unroll
                           for offset from 0 by width
                           collect
                           `(let ((,var (+ ,var ,offset))) ,@vec-body))
                   (incf ,var ,(* width unroll)))))
           ;; The remainder loop.
           (loop
             (unless (< ,var ,end) (return))
             (locally ,@body)
             (incf ,var)))))))

(defun vir-vectorize (width)
  (break "TODO"))
