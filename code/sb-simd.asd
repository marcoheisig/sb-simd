(defsystem #:sb-simd
  :description "A convenient SIMD interface for SBCL."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "MIT"

  :depends-on ("sb-cltl2")

  :serial t
  :components
  ((:file "packages")
   (:file "utilities")
   (:file "tweak-sbcl")
   (:file "instruction-set")
   (:file "missing-instruction")
   (:module "instruction-sets"
    :components
    ((:file "common")
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
   (:file "define-vops")
   (:file "define-vop-functions")
   (:file "define-scalar-casts")
   (:file "define-pseudo-vops")
   (:file "define-simd-casts")
   (:file "define-primitives")
   (:file "define-loads-and-stores")
   (:file "define-arefs")
   (:file "define-commutatives")
   (:file "define-reducers")
   (:file "define-comparisons")
   (:file "define-unequals")
   (:file "define-modify-macros")
   (:file "vector")))
