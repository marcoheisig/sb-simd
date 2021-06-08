(defsystem #:sb-simd
  :description "A convenient SIMD interface for SBCL."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "MIT"

  :serial t
  :components
  ((:file "packages")
   (:file "utilities")
   (:file "value-record")
   (:file "value-record-tables")
   (:file "define-types")
   (:file "instruction-set")
   (:file "instruction-set-tables")
   (:file "missing-instruction")
   (:file "define-vops")
   (:file "define-pseudo-vops")
   (:file "define-primitives")
   (:file "define-loads-and-stores")
   (:file "define-arefs")
   (:file "define-commutatives")
   (:file "define-reducers")
   (:file "define-comparisons")
   (:file "define-unequals")
   (:file "define-modify-macros")))
