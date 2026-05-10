;;;; tests.lisp — Coalton tests for the portfolio content engine

(defpackage #:common-lisp-portfolio-forever/tests
  (:use #:cl)
  (:export #:run-tests)
  (:import-from #:coalton-testing #:is #:coalton-fiasco-init)
  (:local-nicknames (#:core #:common-lisp-portfolio-forever/types/core)
                    (#:valid #:common-lisp-portfolio-forever/types/validation)
                    (#:list #:coalton-library/list)
                    (#:builtin #:coalton-library/builtin)))

(in-package #:common-lisp-portfolio-forever/tests)

(named-readtables:in-readtable coalton:coalton)

(fiasco:define-test-package #:common-lisp-portfolio-forever/fiasco-test-package)

(coalton-fiasco-init #:common-lisp-portfolio-forever/fiasco-test-package)

(cl:defun run-tests ()
  "Run all portfolio content engine tests via Fiasco."
  (fiasco:run-package-tests
   :packages '(#:common-lisp-portfolio-forever/fiasco-test-package)
   :interactive cl:t))

;;; ============================================================
;;; Test: Tag validation
;;; ============================================================

(define-test test-valid-tag ()
  (is (list:null? (valid:validate-tag (core:Tag "coalton")))))

(define-test test-empty-tag ()
  (is (builtin:not (list:null? (valid:validate-tag (core:Tag ""))))))

;;; ============================================================
;;; Test: Person validation
;;; ============================================================

(define-test test-valid-person ()
  (is (list:null?
       (valid:validate-person
        (core:Person "Senik" "A programmer." coalton:None "NYC" coalton:True
                     "email@example.com"
                     (coalton:make-list (core:Tagline "en" "Design Engineer")))))))

(define-test test-person-missing-name ()
  (is (builtin:not (list:null?
            (valid:validate-person
             (core:Person "" "bio" coalton:None "NYC" coalton:True "e@e.com"
                          (coalton:make-list (core:Tagline "en" "tag"))))))))

(define-test test-person-no-taglines ()
  (is (builtin:not (list:null?
            (valid:validate-person
             (core:Person "Senik" "bio" coalton:None "NYC" coalton:True "e@e.com"
                          coalton:Nil))))))

;;; ============================================================
;;; Test: Project validation
;;; ============================================================

(define-test test-valid-project ()
  (is (list:null?
       (valid:validate-project
        (core:Project "foo-compiler" "Foo Compiler" "A compiler."
                      (coalton:make-list (core:Tag "compilers") (core:Tag "coalton"))
                      coalton:Nil "compilers" 2026 4 coalton:None coalton:False)))))

(define-test test-project-empty-slug ()
  (is (builtin:not (list:null?
            (valid:validate-project
             (core:Project "" "Foo" "summary"
                           (coalton:make-list (core:Tag "x"))
                           coalton:Nil "cat" 2026 1 coalton:None coalton:False))))))

(define-test test-project-no-tags ()
  (is (builtin:not (list:null?
            (valid:validate-project
             (core:Project "slug" "Foo" "summary" coalton:Nil coalton:Nil
                           "cat" 2026 1 coalton:None coalton:False))))))

(define-test test-project-bad-year ()
  (is (builtin:not (list:null?
            (valid:validate-project
             (core:Project "slug" "Foo" "summary"
                           (coalton:make-list (core:Tag "x"))
                           coalton:Nil "cat" 2010 1 coalton:None coalton:False))))))
