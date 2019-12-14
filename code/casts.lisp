(in-package #:sb-simd)

(macrolet ((define-simd-cast (goal-record input-records)
             (let* ((name (cast-name goal-record))
                    (cast-names
                      (loop for input-record in input-records
                            collect (cast-name goal-record input-record))))
               `(define-inline ,name (simd-pack)
                  (etypecase simd-pack
                    ,@(loop for input-record in input-records
                            for cast-name in cast-names
                            collect
                            (if (eq input-record goal-record)
                                `(,(simd-record-type input-record)
                                  simd-pack)
                                `(,(simd-record-type input-record)
                                  (,cast-name simd-pack))))))))
           (define-simd-casts ()
             (flet ((input-records (goal-record)
                      (loop for input-record in *simd-records*
                            ;; Ensure the SIMD types have the same width.
                            when (= (simd-record-width goal-record)
                                    (simd-record-width input-record))
                              ;; Ensure the SIMD types have a compatible element type.
                              when (ignore-errors
                                    (coerce (coerce 0 (simd-record-element-type input-record))
                                            (simd-record-element-type goal-record)))
                                collect input-record)))
               `(progn
                  ,@(loop for goal-record in *simd-records*
                          collect
                          `(define-simd-cast ,goal-record ,(input-records goal-record)))))))
  (define-simd-casts))
