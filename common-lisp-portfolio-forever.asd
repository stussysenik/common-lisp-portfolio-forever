;;;; common-lisp-portfolio-forever.asd

(asdf:defsystem #:common-lisp-portfolio-forever
  :description "A Common Lisp + Coalton content engine for portfolio authoring."
  :author "Senik <itsmxzou@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :defsystem-depends-on (#:coalton-asdf)
  :depends-on (#:coalton)
  :pathname "src/"
  :serial t
  :in-order-to ((test-op (test-op "common-lisp-portfolio-forever/tests")))
  :components ((:file "packages")
               (:coalton-file "types/core")
               (:coalton-file "types/validation")
               (:file "portfolio")
               (:file "interrogate")
               (:file "import")
               (:file "serialize")))

(asdf:defsystem #:common-lisp-portfolio-forever/tests
  :description "Tests for common-lisp-portfolio-forever"
  :depends-on (#:common-lisp-portfolio-forever
               #:coalton/testing
               #:fiasco
               #:named-readtables)
  :pathname "tests/"
  :serial t
  :components ((:file "tests"))
  :perform (test-op (o s)
                    (symbol-call '#:common-lisp-portfolio-forever/tests
                                 '#:run-tests)))
