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
   (:file "tables")
   (:file "types")
   (:file "casts")
   (:file "vops")
   (:file "stubs")
   (:file "wrappers")
   #+(or)
   (:file "aref")))
