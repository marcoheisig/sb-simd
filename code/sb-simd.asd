(defsystem #:sb-simd
  :description "A convenient SIMD interface for SBCL."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "MIT"

  :depends-on ("alexandria")

  :serial t
  :components
  ((:file "packages")
   (:file "utilities")
   (:file "tables")
   (:file "naming-convention")
   (:file "scalar-types")
   (:file "simd-types")
   (:file "define-simd-vop")

   (:module "sse" :if-feature (:and :x86-64 :sb-simd-pack)
    :components
    ((:file "cast-vops")
     (:file "arithmetic-vops")
     (:file "aref-vops")))

   (:module "avx2" :if-feature :avx2
    :components
    ((:file "cast-vops")
     (:file "arithmetic-vops")
     (:file "aref-vops")))

   (:file "casts")
   (:file "arithmetic")
   (:file "aref")))
