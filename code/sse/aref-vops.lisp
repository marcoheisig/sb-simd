(in-package :sb-vm)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simpler aref functions
;; Based on Numericals of Shubhamkar Ayare alias digikar99
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(sb-simd::macro-when
 (member :SB-SIMD-PACK sb-impl:+internal-features+)
 (progn
   (defmacro define-sse-aref (vop-ref-name vop-set-name scale
							  arg-type index vop-arg-type result-type inst)
     (destructuring-bind (simd-pack-type simd-reg simd-pack-type-vop)
         (ecase result-type
           (:f64  '((simd-pack double-float) double-sse-reg simd-pack-double))
           (:f32  '((simd-pack single-float) single-sse-reg simd-pack-single))
           (:u64  '((simd-pack (unsigned-byte 64)) int-sse-reg simd-pack-int))
		   (:u32  '((simd-pack (unsigned-byte 32)) int-sse-reg simd-pack-int)))
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
			  (:generator 4 (inst ,inst (float-ref-ea v i 0 0 :scale ,scale) x)))))))

   (define-sse-aref %f64.2-ref %f64.2-set 4
					(simple-array double-float (*))
					(integer 0 #.most-positive-fixnum)
					simple-array-double-float
					:f64 movaps)

   (define-sse-aref %f32.4-ref %f32.4-set 2
					(simple-array single-float (*))
					(integer 0 #.most-positive-fixnum)
					simple-array-single-float
					:f32 movaps)

   (define-sse-aref %u64.2-ref %u64.2-set 4
					(simple-array (unsigned-byte 64) (*))
					(integer 0 #.most-positive-fixnum)
					simple-array-unsigned-byte-64
					:u64 movaps)

   (define-sse-aref %u32.4-ref %u32.4-set 2
					(simple-array (unsigned-byte 32) (*))
					(integer 0 #.most-positive-fixnum)
					simple-array-unsigned-byte-32
					:u32 movaps)))
