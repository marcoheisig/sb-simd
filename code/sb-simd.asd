(defsystem #:sb-simd
  :description "A convenient SIMD interface for SBCL."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "MIT"

  :depends-on
  ("alexandria"
   "bitfield")

  :serial t
  :components
  ((:file "packages")
   (:file "utilities")
   (:file "cpu-features")
   (:file "unsigned-bytes")
   (:file "signed-bytes")
   (:file "records")
   (:file "tables")
   (:file "types")
   (:file "casts")
   (:file "vops")
   (:file "instructions")
   (:file "nary-functions")
   (:file "modify-macros")
   (:file "aref")
   ;(:file "vector")
   ))
