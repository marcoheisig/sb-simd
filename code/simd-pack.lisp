(in-package #:sb-simd)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Types

(defmacro define-simd-type (simd-record)
  (let ((name (simd-record-name simd-record))
        (type (simd-record-type simd-record)))
    `(deftype ,name () ',type)))

(defmacro define-simd-types ()
  `(progn
     ,@(loop for simd-record in *simd-records*
             collect
             `(define-simd-type ,simd-record))))

(define-simd-types)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Constructors

(defmacro define-simd-constructor (simd-record)
  (let ((args (subseq *alphabet* 0 (simd-record-width simd-record))))
    `(define-inline ,(constructor-name simd-record) ,args
       (,(simd-record-pack simd-record)
        ,@(loop for arg in args
                collect
                `(coerce ,arg ',(simd-record-element-type simd-record)))))))

(defmacro define-simd-constructors ()
  `(progn
     ,@(loop for simd-record in *simd-records*
             collect
             `(define-simd-constructor ,simd-record))))

(define-simd-constructors)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Unpacking

(defmacro define-unpacker (simd-record)
  (let ((type (simd-record-name simd-record)))
    `(define-inline ,(unpacker-name simd-record) (,type)
       (declare (type ,type ,type))
       (,(simd-record-unpack simd-record) ,type))))

(defmacro define-unpackers ()
  `(progn
     ,@(loop for simd-record in *simd-records*
             collect
             `(define-unpacker ,simd-record))))

(define-unpackers)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Casts

(defmacro define-simd-cast (goal-record input-record)
  (let ((input-type (simd-record-name input-record))
        (variables
          (subseq *alphabet* 0 (simd-record-width input-record))))
    `(define-inline ,(cast-name goal-record input-record) (,input-type)
       (declare (type ,input-type ,input-type))
       ,(if (eql goal-record input-record)
            `,input-type
            `(multiple-value-bind ,variables
                 (,(simd-record-unpack input-record) ,input-type)
               (,(simd-record-pack goal-record)
                ,@(loop for variable in variables
                        collect
                        `(coerce ,variable ',(simd-record-element-type goal-record)))))))))

(defmacro define-simd-casts ()
  (let ((forms '()))
    (dolist (goal *simd-records*)
      (dolist (input *simd-records*)
        (when (and (= (simd-record-width goal)
                      (simd-record-width input))
                   ;; Ensure the element types are compatible.
                   (ignore-errors
                    (coerce (coerce 0 (simd-record-element-type input))
                            (simd-record-element-type goal))))
          (push `(define-simd-cast ,goal ,input) forms))))
    `(progn ,@(reverse forms))))

(define-simd-casts)
