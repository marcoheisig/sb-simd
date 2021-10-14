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
   (:file "numbers")
   (:file "utilities")
   (:file "generators")
   (:file "test-suite")
   (:file "define-aref-test")
   (:file "define-simple-simd-test")
   (:file "package-checks")))
