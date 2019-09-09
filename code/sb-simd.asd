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
   (:file "scalar")
   (:file "simd-pack")
   (:file "define-simple-vop")

   (:module "sse" :if-feature (:and :x86-64 :sb-simd-pack)
    :components
    ((:file "simd-pack")))

   (:module "avx2" :if-feature :avx2
    :components
    ((:file "simd-pack")))))
