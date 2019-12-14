(in-package #:sb-simd)

(defun make-external-symbol (&rest strings)
  (let ((symbol (intern (apply #'concatenate 'string (mapcar #'string strings))
                        #.*package*)))
    (export symbol)
    symbol))

(defun make-internal-symbol (&rest strings)
  (let ((symbol (intern (apply #'concatenate 'string (mapcar #'string strings))
                        #.*package*)))
    symbol))

(defun constructor-name (record)
  (etypecase record
    (scalar-record
     (make-external-symbol 'make- (scalar-record-name record)))
    (simd-record
     (make-external-symbol 'make- (simd-record-name record)))))

(defun unpacker-name (simd-record)
  (make-external-symbol (simd-record-name simd-record) '-values))

(defun cast-name (goal-record &optional (input-record nil input-record-supplied-p))
  (if (not input-record-supplied-p)
      (make-external-symbol
       (simd-record-name goal-record))
      (make-external-symbol
       (simd-record-name goal-record)
       '-from-
       (simd-record-name input-record))))
