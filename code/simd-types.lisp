(in-package #:sb-simd)

(macrolet ((define-simd-type (simd-record)
             (let ((name (make-external-symbol (simd-record-name simd-record)))
                   (type (simd-record-type simd-record)))
               `(progn (deftype ,name () ',type)
                       (export ',name))))
           (define-simd-types ()
             `(progn
                ,@(loop for simd-record in *simd-records*
                        collect
                        `(define-simd-type ,simd-record)))))
  (define-simd-types))

(macrolet ((define-simd-constructor (simd-record)
             (let ((name (constructor-name simd-record))
                   (args (subseq *alphabet* 0 (simd-record-width simd-record))))
               `(progn (define-inline ,name ,args
                         (,(simd-record-pack simd-record)
                          ,@(loop for arg in args
                                  collect
                                  `(coerce ,arg ',(simd-record-element-type simd-record)))))
                       (export ',name))))
           (define-simd-constructors ()
             `(progn
                ,@(loop for simd-record in *simd-records*
                        collect
                        `(define-simd-constructor ,simd-record)))))
  (define-simd-constructors))

(macrolet ((define-unpacker (simd-record)
             (let ((name (unpacker-name simd-record))
                   (type (simd-record-name simd-record)))
               `(progn (define-inline ,name (,type)
                         (declare (type ,type ,type))
                         (,(simd-record-unpack simd-record) ,type))
                       (export ',name))))

           (define-unpackers ()
             `(progn
                ,@(loop for simd-record in *simd-records*
                        collect
                        `(define-unpacker ,simd-record)))))
  (define-unpackers))

;;; TODO - The next two functions should also work for aliases of SIMD type
;;; specifiers.

(defmacro define-simd-width ()
  `(define-inline ,(make-external-symbol 'simd-width) (type-specifier)
     (ecase type-specifier
       ,@(loop for simd-record in *simd-records*
               collect
               `(,(simd-record-name simd-record)
                 ',(simd-record-width simd-record))))))

(define-simd-width)

(defmacro define-simd-element-type ()
  `(define-inline ,(make-external-symbol 'simd-element-type) (type-specifier)
     (ecase type-specifier
       ,@(loop for simd-record in *simd-records*
               collect
               `(,(simd-record-name simd-record)
                 ',(simd-record-element-type simd-record))))))

(define-simd-element-type)
