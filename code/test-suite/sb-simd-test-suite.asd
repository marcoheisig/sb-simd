(defsystem "sb-simd-test-suite"
  :description "The sb-simd test suite."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "MIT"

  :depends-on ("sb-simd")

  :perform
  (test-op (o c) (symbol-call '#:sb-simd-test-suite '#:run-test-suite))

  :serial t
  :components
  ((:file "packages")
   (:file "utilities")
   (:file "numbers")
   (:file "generators")
   (:file "test-suite")
   (:file "package-checks")
   (:file "define-aref-test")
   (:file "define-simple-simd-test")
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
     (:file "avx2")))))
