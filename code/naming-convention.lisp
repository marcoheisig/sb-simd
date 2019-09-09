(in-package #:sb-simd)

(defun make-external-symbol (&rest strings)
  (let ((symbol (intern (apply #'concatenate 'string (mapcar #'string strings))
                        #.*package*)))
    (export symbol)
    symbol))

(defun constructor-name (record)
  (make-external-symbol
   (etypecase record
     (scalar-record (scalar-record-name record))
     (simd-record (simd-record-name record)))))

(defun unpacker-name (simd-record)
  (make-external-symbol (simd-record-name simd-record) '-values))

(defun cast-name (goal-record input-record)
  (make-external-symbol
   (simd-record-name goal-record)
   '-from-
   (simd-record-name input-record)))
