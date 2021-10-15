(defsystem #:sb-simd
  :description "A convenient SIMD interface for SBCL."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "MIT"

  :depends-on ("sb-cltl2")

  :serial t
  :components
  ((:file "packages")
   (:file "utilities")
   (:file "printable")
   (:file "tweak-sbcl")
   (:file "instruction-set")
   (:file "instruction-set-case")
   (:file "record")
   (:file "missing-instruction")
   (:module "instruction-sets"
    :components
    ((:file "sb-simd")
     (:file "x86-64")
     (:file "sse")
     (:file "sse2")
     (:file "sse3")
     (:file "ssse3")
     (:file "sse4.1")
     (:file "sse4.2")
     (:file "avx")
     (:file "avx2")))
   (:file "define-types")
   (:file "define-instruction-vops")
   (:file "define-vref-vops")
   (:file "define-custom-vops")
   (:file "define-vop-functions")
   (:file "define-scalar-casts")
   (:file "define-fake-vops")
   (:file "define-simd-casts")
   (:file "define-instructions")
   (:file "define-vrefs")
   (:file "define-reffers")
   (:file "define-arefs")
   (:file "define-ifs")
   (:file "define-commutatives")
   (:file "define-reducers")
   (:file "define-comparisons")
   (:file "define-unequals")
   (:file "define-modify-macros")
   (:file "vector")))
