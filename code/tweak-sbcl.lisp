(in-package "SB-VM")

(sb-cltl2:define-declaration instruction-sets (specifier env)
  (declare (ignore env))
  (values :declare specifier))

(defun node-instruction-sets (node)
  (sb-cltl2:declaration-information 'instruction-sets (sb-c::node-lexenv node)))

(define-vop (move-to-single-reg)
  (:args (x :scs (descriptor-reg)
            :load-if (not (sc-is x control-stack))))
  (:results (y :scs (single-reg)))
  (:note "descriptor to float coercion")
  (:node-var node)
  (:generator
   2
   (let ((instruction-sets (node-instruction-sets node)))
     (cond ((member :avx instruction-sets)
            (sc-case x
              (descriptor-reg
               (inst vmovq y x)
               (inst vshufps y y y #4r3331))
              (control-stack
               (inst vmovss y (ea (+ (frame-byte-offset (tn-offset x)) 4) rbp-tn)))))
           (t
            (sc-case x
              (descriptor-reg
               (inst movq y x)
               (inst shufps y y #4r3331))
              (control-stack
               (inst movss y (ea (+ (frame-byte-offset (tn-offset x)) 4) rbp-tn)))))))))

(define-vop (%single-float/signed)
  (:args (x :scs (signed-stack signed-reg)))
  (:results (y :scs (single-reg)))
  (:arg-types signed-num)
  (:result-types single-float)
  (:policy :fast-safe)
  (:note "inline float coercion")
  (:translate %single-float)
  (:vop-var vop)
  (:save-p :compute-only)
  (:node-var node)
  (:generator
   5
   (let ((instruction-sets (node-instruction-sets node)))
     (cond ((member :avx instruction-sets)
            (inst vxorps y y y)
            (note-float-location 'coerce vop x 'single-float)
            (inst vcvtsi2ss y x x))
           (t
            (inst xorps y y)
            (note-float-location 'coerce vop x 'single-float)
            (inst cvtsi2ss y x))))))

(define-vop (%single-float/double-float)
  (:args (x :scs (double-reg double-stack) :target y))
  (:results (y :scs (single-reg)))
  (:arg-types double-float)
  (:result-types single-float)
  (:policy :fast-safe)
  (:note "inline float coercion")
  (:translate %single-float)
  (:vop-var vop)
  (:save-p :compute-only)
  (:node-var node)
  (:generator
   2
   (let ((instruction-sets (node-instruction-sets node)))
     (cond ((member :avx instruction-sets)
            (unless (location= x y) (inst vxorps y y y))
            (note-float-location 'coerce vop x 'single-float)
            (inst vcvtsd2ss y (sc-case x (double-reg x) (double-stack (ea-for-sf-stack x))))
            (when (location= x y) (inst vshufps y y y #b11111100)))
           (t
            (unless (location= x y) (inst xorps y y))
            (note-float-location 'coerce vop x 'single-float)
            (inst cvtsd2ss y (sc-case x (double-reg x) (double-stack (ea-for-sf-stack x))))
            (when (location= x y) (inst shufps y y #b11111100)))))))

(define-vop (%double-float/single-float)
  (:args (x :scs (single-reg single-stack) :target y))
  (:results (y :scs (double-reg)))
  (:arg-types single-float)
  (:result-types double-float)
  (:policy :fast-safe)
  (:note "inline float coercion")
  (:translate %double-float)
  (:vop-var vop)
  (:save-p :compute-only)
  (:node-var node)
  (:generator
   2
   (let ((instruction-sets (node-instruction-sets node)))
     (cond ((member :avx instruction-sets)
            (unless (location= x y) (inst vxorpd y y y))
            (note-float-location 'coerce vop x 'double-float)
            (inst vcvtss2sd y (sc-case x (single-reg x) (single-stack (ea-for-sf-stack x)))))
           (t
            (unless (location= x y) (inst xorpd y y))
            (note-float-location 'coerce vop x 'double-float)
            (inst cvtss2sd y (sc-case x (single-reg x) (single-stack (ea-for-sf-stack x)))))))))

(define-vop (%double-float/signed)
  (:args (x :scs (signed-stack signed-reg)))
  (:results (y :scs (double-reg)))
  (:arg-types signed-num)
  (:result-types double-float)
  (:policy :fast-safe)
  (:note "inline float coercion")
  (:translate %double-float)
  (:vop-var vop)
  (:save-p :compute-only)
  (:node-var node)
  (:generator
   5
   (let ((instruction-sets (node-instruction-sets node)))
     (cond ((intersection instruction-sets '(:avx :avx2))
            (inst vxorpd y y y)
            (note-float-location 'coerce vop x 'double-float)
            (inst vcvtsi2sd y x x))
           (t
            (inst xorpd y y)
            (note-float-location 'coerce vop x 'double-float)
            (inst cvtsi2sd y x))))))
