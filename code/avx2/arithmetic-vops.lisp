(in-package #:sb-vm)

(macrolet ((frob (name inst reg type cost commutativep)
             `(sb-simd::define-simd-vop ,name
                  (((r :scs (,reg) ,@(unless commutativep '(:from (:argument 0)))) ,type)
                   ((x :scs (,reg) :target r) ,type)
                   ((y :scs (,reg)) ,type))
                (:generator ,cost (inst ,inst r x y)))))
  ;; U64.4
  ;; TODO
  ;; f64.4
  (frob sb-simd::f64.4-binary+ vaddpd double-avx2-reg sb-simd:f64.4 2 t)
  (frob sb-simd::f64.4-binary- vsubpd double-avx2-reg sb-simd:f64.4 2 nil)
  (frob sb-simd::f64.4-binary* vmulpd double-avx2-reg sb-simd:f64.4 2 t)
  (frob sb-simd::f64.4-binary/ vdivpd double-avx2-reg sb-simd:f64.4 8 nil)
  ;; f32.8
  (frob sb-simd::f32.8-binary+ vaddps single-avx2-reg sb-simd:f32.8 2 t)
  (frob sb-simd::f32.8-binary- vsubps single-avx2-reg sb-simd:f32.8 2 nil)
  (frob sb-simd::f32.8-binary* vmulpd single-avx2-reg sb-simd:f32.8 2 t)
  (frob sb-simd::f32.8-binary/ vdivpd single-avx2-reg sb-simd:f32.8 8 nil))
