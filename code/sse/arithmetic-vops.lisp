(in-package #:sb-vm)

(macrolet ((frob (name inst reg type cost commutativep)
             `(sb-simd::define-simd-vop ,name
                  (((r :scs (,reg) ,@(unless commutativep '(:from (:argument 0)))) ,type )
                   ((x :scs (,reg) :target r) ,type)
                   ((y :scs (,reg)) ,type))
                (:generator ,cost
                            (move r x)
                            (inst ,inst r y)))))
  ;; f64.2
  (frob sb-simd::f64.2-binary+ addpd double-sse-reg sb-simd:f64.2 2 t)
  (frob sb-simd::f64.2-binary- subpd double-sse-reg sb-simd:f64.2 2 nil)
  (frob sb-simd::f64.2-binary* mulpd double-sse-reg sb-simd:f64.2 2 t)
  (frob sb-simd::f64.2-binary/ divpd double-sse-reg sb-simd:f64.2 8 nil)
  ;; f32.4
  (frob sb-simd::f32.4-binary+ addps single-sse-reg sb-simd:f32.4 2 t)
  (frob sb-simd::f32.4-binary- subps single-sse-reg sb-simd:f32.4 2 nil)
  (frob sb-simd::f32.4-binary* mulpd single-sse-reg sb-simd:f32.4 2 t)
  (frob sb-simd::f32.4-binary/ divpd single-sse-reg sb-simd:f32.4 8 nil))
