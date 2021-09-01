(in-package #:sb-vm)

;;; Define VOPs for all the primitives, loads, and stores of each
;;; instruction set.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Primitive VOPs

(macrolet
    ((define-primitive-vop (primitive-record-name)
       (with-accessors ((name sb-simd-internals:primitive-record-name)
                        (vop sb-simd-internals:primitive-record-vop)
                        (mnemonic sb-simd-internals:primitive-record-mnemonic)
                        (argument-records sb-simd-internals:primitive-record-argument-records)
                        (result-records sb-simd-internals:primitive-record-result-records)
                        (cost sb-simd-internals:primitive-record-cost)
                        (pure sb-simd-internals:primitive-record-pure)
                        (commutative sb-simd-internals:primitive-record-commutative)
                        (prefix sb-simd-internals:primitive-record-prefix)
                        (suffix sb-simd-internals:primitive-record-suffix)
                        (encoding sb-simd-internals:primitive-record-encoding))
           (sb-simd-internals:find-instruction-record primitive-record-name)
         (let* ((asyms (sb-simd-internals:prefixed-symbols "A" (length argument-records)))
                (rsyms (sb-simd-internals:prefixed-symbols "R" (length result-records)))
                (defknown
                    `(defknown ,vop
                         (,@(mapcar #'sb-simd-internals:value-record-name argument-records))
                         (values ,@(mapcar #'sb-simd-internals:value-record-name result-records) &optional)
                         (,@(unless (eq encoding :none) '(always-translatable))
                          ,@(when pure '(foldable flushable movable)))
                       :overwrite-fndb-silently t))
                (arg-types
                  (mapcar #'sb-simd-internals:value-record-primitive-type argument-records))
                (result-types
                  (mapcar #'sb-simd-internals:value-record-primitive-type result-records))
                (args
                  (loop for asym in asyms
                        for argument-record in argument-records
                        when (symbolp (sb-simd-internals:value-record-primitive-type argument-record))
                          collect `(,asym :scs ,(sb-simd-internals:value-record-scs argument-record))))
                (info
                  (loop for asym in asyms
                        for argument-record in argument-records
                        unless (symbolp (sb-simd-internals:value-record-primitive-type argument-record))
                          collect asym))
                (results
                  (loop for rsym in rsyms
                        for result-record in result-records
                        collect `(,rsym :scs ,(sb-simd-internals:value-record-scs result-record)))))
           (ecase encoding
             ((:none :custom)
              `(progn ,defknown))
             (:standard
              `(progn
                 ,defknown
                 (define-vop (,vop)
                   (:translate ,vop)
                   (:policy :fast-safe)
                   (:args ,@args)
                   (:info ,@info)
                   (:results ,@results)
                   (:arg-types ,@arg-types)
                   (:result-types ,@result-types)
                   (:generator
                    ,cost
                    (inst
                     ,mnemonic
                     ,@(when prefix `(,prefix))
                     ,@rsyms
                     ,@asyms
                     ,@(when suffix `(,suffix)))))))
             (:move
              (let ((src (first asyms))
                    (dst (first rsyms)))
                `(progn
                   ,defknown
                   (define-vop (,vop)
                     (:translate ,vop)
                     (:policy :fast-safe)
                     (:args (,@(first args) :target ,dst) ,@(rest args))
                     (:info ,@info)
                     (:results ,@results)
                     (:arg-types ,@arg-types)
                     (:result-types ,@result-types)
                     (:generator
                      ,cost
                      (unless (sb-c:location= ,dst ,src)
                        (inst
                         ,mnemonic
                         ,@(when prefix `(,prefix))
                         ,@rsyms
                         ,@asyms
                         ,@(when suffix `(,suffix)))))))))
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
                     (:temporary (:sc ,(first (sb-simd-internals:value-record-scs (first argument-records)))) tmp)
                     (:info ,@info)
                     (:results ,@results)
                     (:arg-types ,@arg-types)
                     (:result-types ,@result-types)
                     (:generator
                      ,cost
                      (cond ((location= ,x ,r)
                             (inst ,mnemonic ,@(when prefix `(,prefix)) ,r ,y ,@rest ,@(when suffix `(,suffix))))
                            ((or (not (tn-p ,y))
                                 (not (location= ,y ,r)))
                             (move ,r ,x)
                             (inst ,mnemonic ,@(when prefix `(,prefix)) ,r ,y ,@rest ,@(when suffix `(,suffix))))
                            (t
                             (move tmp ,x)
                             (inst ,mnemonic ,@(when prefix `(,prefix)) tmp ,y ,@rest ,@(when suffix `(,suffix)))
                             (move ,r tmp))))))))
             (:sse+xmm0
              (let ((x (first asyms))
                    (y (second asyms))
                    (z (third asyms))
                    (r (first rsyms)))
                `(progn
                   ,defknown
                   (sb-c:define-vop (,vop)
                     (:translate ,vop)
                     (:policy :fast-safe)
                     (:args (,@(first args) :target ,r) ,(second args) (,@(third args) :target xmm0))
                     (:temporary (:sc ,(first (sb-simd-internals:value-record-scs (first argument-records)))) tmp)
                     (:temporary (:sc ,(first (sb-simd-internals:value-record-scs (second argument-records)))
                                  :from (:argument 0) :to :result :offset 0) xmm0)
                     (:info ,@info)
                     (:results ,@results)
                     (:arg-types ,@arg-types)
                     (:result-types ,@result-types)
                     (:generator
                      ,cost
                      (move xmm0 ,z)
                      (cond ((location= ,x ,r)
                             (inst ,mnemonic ,@(when prefix `(,prefix)) ,r ,y xmm0 ,@(when suffix `(,suffix))))
                            ((or (not (tn-p ,y))
                                 (not (location= ,y ,r)))
                             (move ,r ,x)
                             (inst ,mnemonic ,@(when prefix `(,prefix)) ,r ,y xmm0 ,@(when suffix `(,suffix))))
                            (t
                             (move tmp ,x)
                             (inst ,mnemonic ,@(when prefix `(,prefix)) tmp ,y xmm0 ,@(when suffix `(,suffix)))
                             (move ,r tmp))))))))))))
     (define-primitive-vops ()
       `(progn
          ,@(loop for primitive-record
                    in (sb-simd-internals:filter-available-instruction-records
                        #'sb-simd-internals:primitive-record-p)
                  collect `(define-primitive-vop ,(sb-simd-internals:primitive-record-name primitive-record))))))
  (define-primitive-vops))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; VREF VOPs
;;;
;;; Both load- and store VOPs are augmented with an auxiliary last argument
;;; that is a constant addend for the address calculation.  This addend is
;;; zero by default, but we can sometimes transform the code for the index
;;; calculation such that we have a nonzero addend.  We also generate two
;;; variants of the VOP - one for the general case, and one for the case
;;; where the index is a compile-time constant.

(macrolet
    ((define-vref-vop (vref-record-name)
       (with-accessors ((name sb-simd-internals:vref-record-name)
                        (vop sb-simd-internals:vref-record-vop)
                        (mnemonic sb-simd-internals:vref-record-mnemonic)
                        (value-record sb-simd-internals:vref-record-value-record)
                        (vector-record sb-simd-internals:vref-record-vector-record)
                        (store sb-simd-internals:store-record-p))
           (sb-simd-internals:find-instruction-record vref-record-name)
         (let* ((vop-c (sb-simd-internals:mksym (symbol-package vop) vop "-C"))
                (vector-type (sb-simd-internals:value-record-type vector-record))
                (vector-primitive-type (sb-simd-internals:value-record-primitive-type vector-record))
                (value-scs (sb-simd-internals:value-record-scs value-record))
                (value-type (sb-simd-internals:value-record-type value-record))
                (value-primitive-type (sb-simd-internals:value-record-primitive-type value-record))
                (scalar-record
                  (etypecase value-record
                    (sb-simd-internals:scalar-record value-record)
                    (sb-simd-internals:simd-record (sb-simd-internals:simd-record-scalar-record value-record))))
                (n-bytes (ceiling (sb-simd-internals:scalar-record-bits scalar-record) 8))
                (displacement
                  (multiple-value-bind (lo hi)
                      (displacement-bounds other-pointer-lowtag n-bytes vector-data-offset)
                    `(integer ,lo ,hi))))
           (multiple-value-bind (index-sc scale)
               (if (>= n-bytes (ash 1 n-fixnum-tag-bits))
                   (values 'any-reg (ash n-bytes (- n-fixnum-tag-bits)))
                   (values 'signed-reg n-bytes))
             `(progn
                (sb-c:defknown ,vop (,@(when store `(,value-type)) ,vector-type index ,displacement)
                    (values ,value-type &optional)
                    (always-translatable)
                  :overwrite-fndb-silently t)
                (define-vop (,vop)
                  (:translate ,vop)
                  (:policy :fast-safe)
                  (:args
                   ,@(when store `((value :scs ,value-scs :target result)))
                   (vector :scs (descriptor-reg))
                   (index :scs (,index-sc)))
                  (:info addend)
                  (:arg-types
                   ,@(when store `(,value-primitive-type))
                   ,vector-primitive-type
                   positive-fixnum
                   (:constant ,displacement))
                  (:results (result :scs ,value-scs))
                  (:result-types ,value-primitive-type)
                  (:generator
                   2
                   ,@(let ((ea `(ea (+ (* vector-data-offset n-word-bytes)
                                       (* addend ,n-bytes)
                                       (- other-pointer-lowtag))
                                    vector index ,scale)))
                       (if store
                           `((inst ,mnemonic ,ea value)
                             (move result value))
                           `((inst ,mnemonic result ,ea))))))
                (sb-vm::define-vop (,vop-c)
                  (:translate ,vop)
                  (:policy :fast-safe)
                  (:args ,@(when store `((value :scs ,value-scs :target result)))
                         (vector :scs (descriptor-reg)))
                  (:info index addend)
                  (:arg-types ,@(when store `(,value-primitive-type))
                              ,vector-primitive-type
                              (:constant low-index)
                              (:constant ,displacement))
                  (:results (result :scs ,value-scs))
                  (:result-types ,value-primitive-type)
                  (:generator
                   1
                   ,@(let ((ea `(ea (+ (* vector-data-offset n-word-bytes)
                                       (* ,n-bytes (+ index addend))
                                       (- other-pointer-lowtag))
                                    vector)))
                       (if store
                           `((inst ,mnemonic ,ea value)
                             (move result value))
                           `((inst ,mnemonic result ,ea)))))))))))
     (define-vref-vops ()
       `(progn
          ,@(loop for vref-record
                    in (sb-simd-internals:filter-available-instruction-records
                        #'sb-simd-internals:vref-record-p)
                  collect `(define-vref-vop ,(sb-simd-internals:vref-record-name vref-record))))))
  (define-vref-vops))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Custom VOPs

(macrolet
    ((define-custom-vop (name &body clauses)
       (with-accessors ((name sb-simd-internals:primitive-record-name)
                        (vop sb-simd-internals:primitive-record-vop)
                        (argument-records sb-simd-internals:primitive-record-argument-records)
                        (result-records sb-simd-internals:primitive-record-result-records)
                        (cost sb-simd-internals:primitive-record-cost)
                        (encoding sb-simd-internals:primitive-record-encoding))
           (sb-simd-internals:find-instruction-record name)
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
              (:arg-types ,@(mapcar #'sb-simd-internals:value-record-primitive-type argument-records))
              (:result-types ,@(mapcar #'sb-simd-internals:value-record-primitive-type result-records))
              (:args
               ,@(loop for arg in (find-clause :args)
                       for argument-record in argument-records
                       collect `(,@arg :scs ,(sb-simd-internals:value-record-scs argument-record))))
              ,@(find-clauses :info)
              ,@(find-clauses :temporary)
              (:results
               ,@(loop for result in (find-clause :results)
                       for result-record in result-records
                       collect `(,@result :scs ,(sb-simd-internals:value-record-scs result-record))))
              (:generator ,cost ,@(find-clause :generator)))))))
  ;; SSE
  (macrolet ((def (name cmp)
               `(define-custom-vop ,name
                  (:args (a :target tmp) (b))
                  (:temporary (:sc single-reg :from (:argument 0)) tmp)
                  (:results (dst))
                  (:generator
                   (unless (location= a tmp)
                     (inst xorps tmp tmp)
                     (inst movss tmp a))
                   (inst cmpss ,cmp tmp b)
                   (inst movq dst tmp)))))
    (def sb-simd-sse::two-arg-f32= :eq)
    (def sb-simd-sse::two-arg-f32/= :neq)
    (def sb-simd-sse::two-arg-f32< :lt)
    (def sb-simd-sse::two-arg-f32<= :le)
    (def sb-simd-sse::two-arg-f32> :nle)
    (def sb-simd-sse::two-arg-f32>= :nlt))
  (define-custom-vop sb-simd-sse::f32!-from-p128
    (:args (src :target dst))
    (:temporary (:sc single-sse-reg :from (:argument 0)) tmp)
    (:results (dst))
    (:generator
     (move tmp src)
     (inst xorps dst dst)
     (inst movss dst tmp)))
  ;; SSE2
  (macrolet ((def (name cmp)
               `(define-custom-vop ,name
                  (:args (a :target tmp) (b))
                  (:temporary (:sc single-reg :from (:argument 0)) tmp)
                  (:results (dst))
                  (:generator
                   (unless (location= a tmp)
                     (inst xorpd tmp tmp)
                     (inst movsd tmp a))
                   (inst cmpsd ,cmp tmp b)
                   (inst movq dst tmp)))))
    (def sb-simd-sse2::two-arg-f64= :eq)
    (def sb-simd-sse2::two-arg-f64/= :neq)
    (def sb-simd-sse2::two-arg-f64< :lt)
    (def sb-simd-sse2::two-arg-f64<= :le)
    (def sb-simd-sse2::two-arg-f64> :nle)
    (def sb-simd-sse2::two-arg-f64>= :nlt))
  (define-custom-vop sb-simd-sse2::f64!-from-p128
    (:args (src :target tmp))
    (:temporary (:sc double-sse-reg :from (:argument 0)) tmp)
    (:results (dst))
    (:generator
     (move tmp src)
     (inst xorpd dst dst)
     (inst movsd dst tmp)))
  ;; AVX
  (macrolet ((def (name cmp)
               `(define-custom-vop ,name
                  (:args (a :target tmp) (b))
                  (:temporary (:sc single-reg :from (:argument 0)) tmp)
                  (:results (dst))
                  (:generator
                   (unless (location= a tmp)
                     (inst vxorps tmp tmp tmp))
                   (inst vcmpss ,cmp tmp a b)
                   (inst vmovq dst tmp)))))
    (def sb-simd-avx::two-arg-f32= :eq)
    (def sb-simd-avx::two-arg-f32/= :neq)
    (def sb-simd-avx::two-arg-f32< :lt)
    (def sb-simd-avx::two-arg-f32<= :le)
    (def sb-simd-avx::two-arg-f32> :nle)
    (def sb-simd-avx::two-arg-f32>= :nlt))
  (macrolet ((def (name cmp)
               `(define-custom-vop ,name
                  (:args (a :target tmp) (b))
                  (:temporary (:sc single-reg :from (:argument 0)) tmp)
                  (:results (dst))
                  (:generator
                   (unless (location= a tmp)
                     (inst vxorpd tmp tmp tmp))
                   (inst vcmpsd ,cmp tmp a b)
                   (inst vmovq dst tmp)))))
    (def sb-simd-avx::two-arg-f64= :eq)
    (def sb-simd-avx::two-arg-f64/= :neq)
    (def sb-simd-avx::two-arg-f64< :lt)
    (def sb-simd-avx::two-arg-f64<= :le)
    (def sb-simd-avx::two-arg-f64> :nle)
    (def sb-simd-avx::two-arg-f64>= :nlt))
  (define-custom-vop sb-simd-avx::f32!-from-p128
    (:args (src :target tmp))
    (:temporary (:sc single-avx2-reg :from (:argument 0)) tmp)
    (:results (dst))
    (:generator
     (move tmp src)
     (inst vxorps dst dst dst)
     (inst movss dst tmp)))
  (define-custom-vop sb-simd-avx::f32!-from-p256
    (:args (src :target tmp))
    (:temporary (:sc single-avx2-reg :from (:argument 0)) tmp)
    (:results (dst))
    (:generator
     (move tmp src)
     (inst vxorps dst dst dst)
     (inst movss dst tmp)))
  (define-custom-vop sb-simd-avx::f64!-from-p128
    (:args (src :target tmp))
    (:temporary (:sc double-avx2-reg :from (:argument 0)) tmp)
    (:results (dst))
    (:generator
     (move tmp src)
     (inst vxorpd dst dst dst)
     (inst movsd dst tmp)))
  (define-custom-vop sb-simd-avx::f64!-from-p256
    (:args (src :target tmp))
    (:temporary (:sc double-avx2-reg :from (:argument 0)) tmp)
    (:results (dst))
    (:generator
     (move tmp src)
     (inst vxorpd dst dst dst)
     (inst movsd dst tmp))))
