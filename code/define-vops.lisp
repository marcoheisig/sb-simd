(in-package #:sb-simd)

;;; Define VOPs for all the primitives, loads, and stores of each
;;; instruction set.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Primitive VOPs

(defmacro define-primitive-vop (primitive-record-name)
  (with-accessors ((name primitive-record-name)
                   (vop primitive-record-vop)
                   (mnemonic primitive-record-mnemonic)
                   (argument-records primitive-record-argument-records)
                   (result-records primitive-record-result-records)
                   (cost primitive-record-cost)
                   (pure primitive-record-pure)
                   (commutative primitive-record-commutative)
                   (prefix primitive-record-prefix)
                   (encoding primitive-record-encoding))
      (find-instruction-record primitive-record-name)
    (let* ((asyms (argument-symbols (length argument-records)))
           (rsyms (result-symbols (length result-records)))
           (defknown
               `(sb-c:defknown ,vop
                    (,@(mapcar #'value-record-name argument-records))
                    (values ,@(mapcar #'value-record-name result-records) &optional)
                    (,@(unless (eq encoding :none) '(sb-c:always-translatable))
                     ,@(when pure '(sb-c:foldable sb-c:flushable sb-c:movable)))
                  :overwrite-fndb-silently t))
           (args
             (loop for asym in asyms
                   for argument-record in argument-records
                   when (symbolp (value-record-primitive-type argument-record))
                     collect `(,asym :scs ,(value-record-scs argument-record))))
           (info
             (loop for asym in asyms
                   for argument-record in argument-records
                   unless (symbolp (value-record-primitive-type argument-record))
                     collect asym))
           (results
             (loop for rsym in rsyms
                   for result-record in result-records
                   collect `(,rsym :scs ,(value-record-scs result-record)))))
      (ecase encoding
        ((:none :custom)
         `(progn ,defknown))
        (:standard
         `(progn
            ,defknown
            (sb-c:define-vop (,vop)
              (:translate ,vop)
              (:policy :fast-safe)
              (:args ,@args)
              (:info ,@info)
              (:results ,@results)
              (:arg-types ,@(mapcar #'value-record-primitive-type argument-records))
              (:result-types ,@(mapcar #'value-record-primitive-type result-records))
              (:generator
               ,cost
               (sb-assem:inst ,mnemonic ,@(when prefix `(,prefix)) ,@rsyms ,@asyms)))))
        (:move
         (let ((src (first asyms))
               (dst (first rsyms)))
           `(progn
              ,defknown
              (sb-c:define-vop (,vop)
                (:translate ,vop)
                (:policy :fast-safe)
                (:args (,@(first args) :target ,dst) ,@(rest args))
                (:info ,@info)
                (:results ,@results)
                (:arg-types ,@(mapcar #'value-record-primitive-type argument-records))
                (:result-types ,@(mapcar #'value-record-primitive-type result-records))
                (:generator
                 ,cost
                 (unless (sb-c:location= ,dst ,src)
                   (sb-assem:inst ,mnemonic ,@(when prefix `(,prefix)) ,@rsyms ,@asyms)))))))
        (:sse
         (let ((x (first asyms))
               (y (second asyms))
               (rest (rest (rest asyms)))
               (r (first rsyms)))
           `(progn
              ,defknown
              (sb-c:define-vop (,vop)
                (:translate ,vop)
                (:policy :fast-safe)
                (:args (,@(first args) :target ,r) ,@(rest args))
                (:temporary (:sc ,(first (value-record-scs (first argument-records)))) tmp)
                (:info ,@info)
                (:results ,@results)
                (:arg-types ,@(mapcar #'value-record-primitive-type argument-records))
                (:result-types ,@(mapcar #'value-record-primitive-type result-records))
                (:generator
                 ,cost
                 (cond ((sb-c:location= ,x ,r)
                        (sb-assem:inst ,mnemonic ,@(when prefix `(,prefix)) ,r ,y ,@rest))
                       ((or (not (sb-c:tn-p ,y))
                            (not (sb-c:location= ,y ,r)))
                        (sb-c:move ,r ,x)
                        (sb-assem:inst ,mnemonic ,@(when prefix `(,prefix)) ,r ,y ,@rest))
                       (t
                        (sb-c:move tmp ,x)
                        (sb-assem:inst ,mnemonic ,@(when prefix `(,prefix)) tmp ,y ,@rest)
                        (sb-c:move ,r tmp))))))))))))

(defmacro define-primitive-vops ()
  `(progn
     ,@(loop for primitive-record in (filter-available-instruction-records #'primitive-record-p)
             collect `(define-primitive-vop ,(primitive-record-name primitive-record)))))

(define-primitive-vops)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Load VOPs
;;;
;;; Both load- and store VOPs are augmented with an auxiliary last argument
;;; that is a constant addend for the address calculation.  This addend is
;;; zero by default, but we can sometimes transform the code for the index
;;; calculation such that we have a nonzero addend.  We also generate two
;;; variants of the VOP - one for the general case, and one for the case
;;; where the index is a compile-time constant.

(defmacro define-load-vop (load-record-name)
  (with-accessors ((name load-record-name)
                   (vop load-record-vop)
                   (mnemonic load-record-mnemonic)
                   (value-record load-record-value-record)
                   (vector-record load-record-vector-record))
      (find-instruction-record load-record-name)
    (let* ((scalar-record
             (etypecase value-record
               (scalar-record value-record)
               (simd-record (simd-record-scalar-record value-record))))
           (element-size (ceiling (scalar-record-bits scalar-record) 8))
           (scale (ash element-size (- sb-vm:n-fixnum-tag-bits)))
           (displacement
             (multiple-value-bind (lo hi)
                 (sb-vm::displacement-bounds sb-vm:other-pointer-lowtag element-size sb-vm:vector-data-offset)
               `(integer ,lo ,hi))))
      `(progn
         (sb-c:defknown ,vop
             (,(value-record-name vector-record) index ,displacement)
             (values ,(value-record-name value-record) &optional)
             (sb-c:always-translatable)
           :overwrite-fndb-silently t)
         (sb-vm::define-vop (,vop)
           (:translate ,vop)
           (:policy :fast-safe)
           (:args (vector :scs (sb-vm::descriptor-reg))
                  (index :scs (sb-vm::any-reg)))
           (:info addend)
           (:arg-types ,(scalar-record-primitive-type vector-record)
                       sb-vm::positive-fixnum
                       (:constant ,displacement))
           (:results (result :scs ,(value-record-scs value-record)))
           (:result-types ,(value-record-primitive-type value-record))
           (:generator 2
                       (sb-assem:inst
                        ,mnemonic
                        result
                        (sb-vm::float-ref-ea vector index addend ,element-size :scale ,scale))))
         (sb-vm::define-vop (,(mksym (symbol-package vop) vop "-C"))
           (:translate ,vop)
           (:policy :fast-safe)
           (:args (vector :scs (sb-vm::descriptor-reg)))
           (:info index addend)
           (:arg-types ,(scalar-record-primitive-type vector-record)
                       (:constant sb-vm::low-index)
                       (:constant ,displacement))
           (:results (result :scs ,(value-record-scs value-record)))
           (:result-types ,(value-record-primitive-type value-record))
           (:generator 1
                       (sb-assem:inst
                        ,mnemonic
                        result
                        (sb-vm::float-ref-ea vector index addend ,element-size))))))))

(defmacro define-load-vops ()
  `(progn
     ,@(loop for load-record in (filter-available-instruction-records #'load-record-p)
             collect `(define-load-vop ,(load-record-name load-record)))))

(define-load-vops)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Store VOPs

(defmacro define-store-vop (store-record-name)
  (with-accessors ((name store-record-name)
                   (vop store-record-vop)
                   (mnemonic store-record-mnemonic)
                   (value-record store-record-value-record)
                   (vector-record store-record-vector-record))
      (find-instruction-record store-record-name)
    (let* ((scalar-record
             (etypecase value-record
               (scalar-record value-record)
               (simd-record (simd-record-scalar-record value-record))))
           (element-size (ceiling (scalar-record-bits scalar-record) 8))
           (scale (ash element-size (- sb-vm:n-fixnum-tag-bits)))
           (displacement
             (multiple-value-bind (lo hi)
                 (sb-vm::displacement-bounds sb-vm:other-pointer-lowtag element-size sb-vm:vector-data-offset)
               `(integer ,lo ,hi))))
      `(progn
         (sb-c:defknown ,vop
             (,(value-record-name value-record)
              ,(value-record-name vector-record)
              index ,displacement)
             (values ,(value-record-name value-record) &optional)
             (sb-c:always-translatable)
           :overwrite-fndb-silently t)
         (sb-vm::define-vop (,vop)
           (:translate ,vop)
           (:policy :fast-safe)
           (:args (value :scs ,(value-record-scs value-record))
                  (vector :scs (sb-vm::descriptor-reg))
                  (index :scs (sb-vm::any-reg)))
           (:info addend)
           (:arg-types ,(value-record-primitive-type value-record)
                       ,(scalar-record-primitive-type vector-record)
                       sb-vm::positive-fixnum
                       (:constant ,displacement))
           (:results (result :scs ,(value-record-scs value-record)))
           (:result-types ,(value-record-primitive-type value-record))
           (:generator 2
             (sb-assem:inst
              ,mnemonic
              (sb-vm::float-ref-ea vector index addend ,element-size :scale ,scale)
              value)
             (sb-c:move result value)))
         (sb-vm::define-vop (,(mksym (symbol-package vop) vop "-C"))
           (:translate ,vop)
           (:policy :fast-safe)
           (:args (value :scs ,(value-record-scs value-record))
                  (vector :scs (sb-vm::descriptor-reg)))
           (:info index addend)
           (:arg-types ,(value-record-primitive-type value-record)
                       ,(scalar-record-primitive-type vector-record)
                       (:constant sb-vm::low-index)
                       (:constant ,displacement))
           (:results (result :scs ,(value-record-scs value-record)))
           (:result-types ,(value-record-primitive-type value-record))
           (:generator 1
             (sb-assem:inst
              ,mnemonic
              (sb-vm::float-ref-ea vector index addend ,element-size)
              value)
             (sb-c:move result value)))))))

(defmacro define-store-vops ()
  `(progn
     ,@(loop for store-record in (filter-available-instruction-records #'store-record-p)
             collect `(define-store-vop ,(store-record-name store-record)))))

(define-store-vops)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Custom VOPs

(defmacro define-custom-vop (name &body clauses)
  (with-accessors ((name primitive-record-name)
                   (vop primitive-record-vop)
                   (argument-records primitive-record-argument-records)
                   (result-records primitive-record-result-records)
                   (cost primitive-record-cost)
                   (encoding primitive-record-encoding))
      (find-instruction-record name)
    (assert (eq encoding :custom))
    (labels ((find-clauses (key)
               (remove key clauses :test-not #'eq :key #'first))
             (find-clause (key)
               (let ((found (find-clauses key)))
                 (assert (= 1 (length found)))
                 (rest (first found)))))
      `(sb-c:define-vop (,vop)
         (:translate ,vop)
         (:policy :fast-safe)
         (:arg-types ,@(mapcar #'value-record-primitive-type argument-records))
         (:result-types ,@(mapcar #'value-record-primitive-type result-records))
         (:args
          ,@(loop for arg in (find-clause :args)
                  for argument-record in argument-records
                  collect `(,@arg :scs ,(value-record-scs argument-record))))
         ,@(find-clauses :info)
         ,@(find-clauses :temporary)
         (:results
          ,@(loop for result in (find-clause :results)
                  for result-record in result-records
                  collect `(,@result :scs ,(value-record-scs result-record))))
         (:generator ,cost ,@(find-clause :generator))))))

(in-package #:sb-vm)

(sb-simd::define-custom-vop sb-simd-sse::f32!-from-p128
  (:args (src :target dst))
  (:results (dst))
  (:generator
   (unless (location= src dst)
     (inst xorps dst dst)
     (inst movaps dst src))))

(sb-simd::define-custom-vop sb-simd-sse::f32.4!-from-f32
  (:args (src :target dst))
  (:results (dst))
  (:generator
   (unless (location= src dst)
     (inst xorps dst dst)
     (inst movaps dst src))))

(sb-simd::define-custom-vop sb-simd-sse2::f64!-from-p128
  (:args (src :target dst))
  (:results (dst))
  (:generator
   (unless (location= src dst)
     (inst xorpd dst dst)
     (inst movapd dst src))))

(sb-simd::define-custom-vop sb-simd-sse2::f64.2!-from-f64
  (:args (src :target dst))
  (:results (dst))
  (:generator
   (unless (location= src dst)
     (inst xorpd dst dst)
     (inst movapd dst src))))

(sb-simd::define-custom-vop sb-simd-avx::f32!-from-p128
  (:args (src :target dst))
  (:results (dst))
  (:generator
   (unless (location= src dst)
     (inst vxorps dst dst dst)
     (inst vmovaps dst src))))

(sb-simd::define-custom-vop sb-simd-avx::f64!-from-p128
  (:args (src :target dst))
  (:results (dst))
  (:generator
   (unless (location= src dst)
     (inst vxorpd dst dst dst)
     (inst vmovapd dst src))))

(sb-simd::define-custom-vop sb-simd-avx::f32!-from-p256
  (:args (src :target dst))
  (:results (dst))
  (:generator
   (unless (location= src dst)
     (inst vxorps dst dst dst)
     (inst vmovaps dst src))))

(sb-simd::define-custom-vop sb-simd-avx::f64!-from-p256
  (:args (src :target dst))
  (:results (dst))
  (:generator
   (unless (location= src dst)
     (inst vxorpd dst dst dst)
     (inst vmovapd dst src))))

(sb-simd::define-custom-vop sb-simd-avx::f32.4!-from-f32
  (:args (src :target dst))
  (:results (dst))
  (:generator
   (unless (location= src dst)
     (inst vxorps dst dst dst)
     (inst vmovaps dst src))))

(sb-simd::define-custom-vop sb-simd-avx::f32.4!-from-p256
  (:args (src :target dst))
  (:results (dst))
  (:generator
   (unless (location= src dst)
     (inst vxorps dst dst dst)
     (inst vmovaps dst src))))

(sb-simd::define-custom-vop sb-simd-avx::f64.2!-from-f64
  (:args (src :target dst))
  (:results (dst))
  (:generator
   (unless (location= src dst)
     (inst vxorpd dst dst dst)
     (inst vmovapd dst src))))

(sb-simd::define-custom-vop sb-simd-avx::f64.2!-from-p256
  (:args (src :target dst))
  (:results (dst))
  (:generator
   (unless (location= src dst)
     (inst vxorpd dst dst dst)
     (inst vmovapd dst src))))

(sb-simd::define-custom-vop sb-simd-avx::f32.8!-from-f32
  (:args (src :target dst))
  (:results (dst))
  (:generator
   (unless (location= src dst)
     (inst vxorps dst dst dst)
     (inst vmovaps dst src))))

(sb-simd::define-custom-vop sb-simd-avx::f64.4!-from-f64
  (:args (src :target dst))
  (:results (dst))
  (:generator
   (unless (location= src dst)
     (inst vxorpd dst dst dst)
     (inst vmovapd dst src))))

(sb-simd::define-custom-vop sb-simd-avx::u8.16!-from-p256
  (:args (src :target dst))
  (:results (dst))
  (:generator
   (unless (location= src dst)
     (inst vxorpd dst dst dst)
     (inst vmovapd dst src))))

(sb-simd::define-custom-vop sb-simd-avx::u16.8!-from-p256
  (:args (src :target dst))
  (:results (dst))
  (:generator
   (unless (location= src dst)
     (inst vxorpd dst dst dst)
     (inst vmovapd dst src))))

(sb-simd::define-custom-vop sb-simd-avx::u32.4!-from-p256
  (:args (src :target dst))
  (:results (dst))
  (:generator
   (unless (location= src dst)
     (inst vxorpd dst dst dst)
     (inst vmovapd dst src))))

(sb-simd::define-custom-vop sb-simd-avx::u64.2!-from-p256
  (:args (src :target dst))
  (:results (dst))
  (:generator
   (unless (location= src dst)
     (inst vxorpd dst dst dst)
     (inst vmovapd dst src))))

(sb-simd::define-custom-vop sb-simd-avx::s8.16!-from-p256
  (:args (src :target dst))
  (:results (dst))
  (:generator
   (unless (location= src dst)
     (inst vxorpd dst dst dst)
     (inst vmovapd dst src))))

(sb-simd::define-custom-vop sb-simd-avx::s16.8!-from-p256
  (:args (src :target dst))
  (:results (dst))
  (:generator
   (unless (location= src dst)
     (inst vxorpd dst dst dst)
     (inst vmovapd dst src))))

(sb-simd::define-custom-vop sb-simd-avx::s32.4!-from-p256
  (:args (src :target dst))
  (:results (dst))
  (:generator
   (unless (location= src dst)
     (inst vxorpd dst dst dst)
     (inst vmovapd dst src))))

(sb-simd::define-custom-vop sb-simd-avx::s64.2!-from-p256
  (:args (src :target dst))
  (:results (dst))
  (:generator
   (unless (location= src dst)
     (inst vxorpd dst dst dst)
     (inst vmovapd dst src))))
