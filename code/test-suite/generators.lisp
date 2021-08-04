(in-package #:sb-simd-test-suite)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *generator-alist*
    '((sb-simd-common:f32 random-f32)
      (sb-simd-common:f64 random-f64)
      (sb-simd-common:u8 random-u8)
      (sb-simd-common:u16 random-u16)
      (sb-simd-common:u32 random-u32)
      (sb-simd-common:u64 random-u64)
      (sb-simd-common:s8 random-s8)
      (sb-simd-common:s16 random-s16)
      (sb-simd-common:s32 random-s32)
      (sb-simd-common:s64 random-s64))))

(defun find-generator (type)
  (loop for (generator-type generator-name) in *generator-alist* do
    (when (eq type generator-type)
      (return-from find-generator generator-name)))
  (error "Found no generator for type ~S."
         type))

(macrolet ((define-generators ()
             `(progn
                ,@(loop for (type name) in *generator-alist*
                        collect
                        (let ((numbers (numbers-of-type type)))
                          `(defun ,name ()
                             (aref ,(coerce numbers `(simple-array ,type (*)))
                                   (random ,(length numbers)))))))))
  (define-generators))
