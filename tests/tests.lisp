;;;; tests.lisp — Coalton tests for the portfolio content engine

(defpackage #:common-lisp-portfolio-forever/tests
  (:use #:cl)
  (:import-from #:coalton #:coalton-toplevel)
  (:import-from #:coalton-testing #:define-test #:is #:coalton-fiasco-init)
  (:import-from #:coalton-library/list #:make-list)
  (:local-nicknames (#:core #:common-lisp-portfolio-forever/types/core)
                    (#:valid #:common-lisp-portfolio-forever/types/validation)
                    (#:list #:coalton-library/list)))

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

(coalton-toplevel
  (define-test test-valid-tag ()
    (is (list:null? (valid:validate-tag (core:Tag "coalton")))))

  (define-test test-empty-tag ()
    (is (not (list:null? (valid:validate-tag (core:Tag "")))))))

;;; ============================================================
;;; Test: Person validation
;;; ============================================================

(coalton-toplevel
  (define-test test-valid-person ()
    (let ((p (core:Person
              "Senik"
              "A programmer."
              coalton:None
              "NYC"
              coalton:True
              "email@example.com"
              (make-list (core:Tagline "en" "Design Engineer")))))
      (is (list:null? (valid:validate-person p)))))

  (define-test test-person-missing-name ()
    (let ((p (core:Person "" "bio" coalton:None "NYC" coalton:True "e@e.com"
                          (make-list (core:Tagline "en" "tag")))))
      (is (not (list:null? (valid:validate-person p))))))

  (define-test test-person-no-taglines ()
    (let ((p (core:Person "Senik" "bio" coalton:None "NYC" coalton:True "e@e.com"
                          coalton:Nil)))
      (is (not (list:null? (valid:validate-person p)))))))

;;; ============================================================
;;; Test: Project validation
;;; ============================================================

(coalton-toplevel
  (define-test test-valid-project ()
    (let ((p (core:Project
              "foo-compiler" "Foo Compiler" "A compiler."
              (make-list (core:Tag "compilers") (core:Tag "coalton"))
              coalton:Nil
              "compilers" 2026 4 coalton:None coalton:False)))
      (is (list:null? (valid:validate-project p)))))

  (define-test test-project-empty-slug ()
    (let ((p (core:Project "" "Foo" "summary"
                           (make-list (core:Tag "x"))
                           coalton:Nil "cat" 2026 1 coalton:None coalton:False)))
      (is (not (list:null? (valid:validate-project p))))))

  (define-test test-project-no-tags ()
    (let ((p (core:Project "slug" "Foo" "summary" coalton:Nil coalton:Nil "cat" 2026 1 coalton:None coalton:False)))
      (is (not (list:null? (valid:validate-project p))))))

  (define-test test-project-bad-year ()
    (let ((p (core:Project "slug" "Foo" "summary"
                           (make-list (core:Tag "x"))
                           coalton:Nil "cat" 2010 1 coalton:None coalton:False)))
      (is (not (list:null? (valid:validate-project p)))))))
