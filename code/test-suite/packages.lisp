(cl:in-package #:common-lisp-user)

(defpackage #:sb-simd-test-suite
  (:use #:common-lisp)
  (:export
   #:run-test-suite
   #:define-test
   #:is
   #:signals
   #:all-tests
   #:check-package
   #:run-tests))
