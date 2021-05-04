(in-package #:sb-vm)

(defknown sb-simd::f64.4-data-vector-ref-with-offset
    (simple-array fixnum fixnum)
    sb-simd:f64.4
    (movable unsafely-flushable always-translatable)
  :overwrite-fndb-silently
  t)

(defknown sb-simd::f64.4-data-vector-set-with-offset
    (simple-array fixnum fixnum (simd-pack-256 double-float))
    sb-simd:f64.4
    (always-translatable)
  :overwrite-fndb-silently
  t)

(define-vop (f64.4-data-vector-ref-with-offset)
  (:note "inline array access")
  (:translate sb-simd::f64.4-data-vector-ref-with-offset)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg)))
  (:info offset)
  (:arg-types simple-array-double-float tagged-num
              (:constant (constant-displacement other-pointer-lowtag 8 vector-data-offset)))
  (:results (result :scs (double-avx2-reg)))
  (:result-types simd-pack-256-double)
  (:generator 7
    (inst vmovupd result (float-ref-ea object index offset 8
                                      :scale (ash 1 (- word-shift n-fixnum-tag-bits))))))

(define-vop (f64.4-data-vector-ref-c)
  (:note "inline array access")
  (:translate sb-simd::f64.4-data-vector-ref-with-offset)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:info index offset)
  (:arg-types simple-array-double-float
              (:constant low-index)
              (:constant (constant-displacement other-pointer-lowtag
                                                8 vector-data-offset)))
  (:results (result :scs (double-avx2-reg)))
  (:result-types simd-pack-256-double)
  (:generator 6
              (inst vmovupd result (float-ref-ea object index offset 8))))

(define-vop (f64.4-data-vector-set-with-offset)
  (:note "inline array access")
  (:translate sb-simd::f64.4-data-vector-set-with-offset)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (double-avx2-reg) :target result))
  (:info offset)
  (:arg-types simple-array-double-float tagged-num
              (:constant (constant-displacement other-pointer-lowtag 8 vector-data-offset))
              simd-pack-256-double)
  (:results (result :scs (double-avx2-reg)))
  (:result-types simd-pack-256-double)
  (:generator 20
              (inst vmovupd
                    (float-ref-ea object index offset 8
                                  :scale (ash 1 (- word-shift n-fixnum-tag-bits)))
                    value)
              (move result value)))

(define-vop (f64.4-data-vector-set-with-offset-c)
  (:note "inline array access")
  (:translate sb-simd::f64.4-data-vector-set-with-offset)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (value :scs (double-avx2-reg) :target result))
  (:info index offset)
  (:arg-types simple-array-double-float (:constant low-index)
              (:constant (constant-displacement other-pointer-lowtag 8 vector-data-offset))
              simd-pack-256-double)
  (:results (result :scs (double-avx2-reg)))
  (:result-types simd-pack-256-double)
  (:generator 19
              (inst vmovupd (float-ref-ea object index offset 8) value)
              (move result value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simpler aref functions
;; Based on Numericals of Shubhamkar Ayare alias digikar99
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(sb-simd::macro-when
 (member :SB-SIMD-PACK-256 sb-impl:+internal-features+)
 (progn
   (defmacro define-avx-aref (vop-ref-name vop-set-name scale
		 					  arg-type index vop-arg-type result-type inst)
     (destructuring-bind (simd-pack-type simd-reg simd-pack-type-vop)
         (ecase result-type
           (:f64  '((simd-pack-256 double-float) double-avx2-reg simd-pack-256-double))
           (:f32  '((simd-pack-256 single-float) single-avx2-reg simd-pack-256-single))
           (:u64  '((simd-pack-256 integer) int-avx2-reg simd-pack-256-int))
		   (:u32  '((simd-pack-256 integer) int-avx2-reg simd-pack-256-int)))
     `(progn
        (eval-when (:compile-toplevel :load-toplevel :execute)
		  (defknown (,vop-ref-name) (,arg-type ,index)
			  ,simd-pack-type
			  (movable foldable flushable always-translatable)
            :overwrite-fndb-silently t)
		  (define-vop (,vop-ref-name)
            (:translate ,vop-ref-name)
            (:args (v :scs (descriptor-reg))
                   (i :scs (any-reg)))
            (:arg-types ,vop-arg-type
                        tagged-num)
            (:results (dest :scs (,simd-reg)))
            (:result-types ,simd-pack-type-vop)
            (:policy :fast-safe)
            (:generator 4 (inst ,inst dest (float-ref-ea v i 0 0 :scale ,scale))))

		  (defknown (,vop-set-name) (,arg-type ,index ,simd-pack-type)
            ,simd-pack-type
            (always-translatable)
			:overwrite-fndb-silently t)
          (define-vop (,vop-set-name)
			  (:translate ,vop-set-name)
			(:args (v :scs (descriptor-reg))
                   (i :scs (any-reg))
                   (x :scs (,simd-reg)))
			(:arg-types ,vop-arg-type
						tagged-num
						,simd-pack-type-vop)
			(:policy :fast-safe)
			(:generator 4 (inst ,inst (float-ref-ea v i 0 0 :scale ,scale) x)))

		  (defknown %vzeroupper () (integer)
					(always-translatable)
					:overwrite-fndb-silently t)
		  (define-vop (%vzeroupper)
			  (:translate %vzeroupper)
			(:policy :fast-safe)
			(:generator 1 (inst vzeroupper)))))))

   (define-avx-aref %f64.4-ref %f64.4-set 4
					(simple-array double-float (*))
					(integer 0 #.most-positive-fixnum)
					simple-array-double-float
					:f64 vmovups)

   (define-avx-aref %f32.8-ref %f32.8-set 2
					(simple-array single-float (*))
					(integer 0 #.most-positive-fixnum)
					simple-array-single-float
					:f32 vmovups)

   (define-avx-aref %u64.4-ref %u64.4-set 4
					(simple-array (unsigned-byte 64) (*))
					(integer 0 #.most-positive-fixnum)
					simple-array-unsigned-byte-64
					:u64 vmovups)

   (define-avx-aref %u32.8-ref %u32.8-set 2
					(simple-array (unsigned-byte 32) (*))
					(integer 0 #.most-positive-fixnum)
					simple-array-unsigned-byte-32
					:u32 vmovups)))
