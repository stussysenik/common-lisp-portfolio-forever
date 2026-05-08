;;;; portfolio.lisp — Common Lisp runtime for the portfolio content engine
;;;;
;;;; Coalton types are defined in types/core.coal and types/validation.coal.
;;;; We access Coalton constructors via their Coalton package prefix.
;;;; CL code here handles state management, interrogation, and emission.

(in-package #:portfolio)

;;; ============================================================
;;; In-memory portfolio state
;;; ============================================================

(defvar *portfolio* nil
  "The current portfolio as a Coalton Portfolio value.")

;;; ============================================================
;;; Coalton type constructors — thin wrappers around Coalton packages
;;; ============================================================

;; Coalton packages from .coal files become CL packages.
;; For brevity, we intern accessor symbols that forward to the real packages.
;; If the Coalton packages don't exist yet at read time, we handle it at runtime.

(defmacro ct-construct (coalton-package-name constructor-name &rest args)
  "Call a Coalton type constructor at runtime.
   COALTON-PACKAGE-NAME is a string like \"CORE/TYPES\".
   CONSTRUCTOR-NAME is a symbol naming the constructor."
  `(funcall (intern (symbol-name ',constructor-name)
                    (find-package ,coalton-package-name))
            ,@args))

;; Simpler: define runtime accessor functions for each constructor.

(defun ct-tag (text)
  "Create a Coalton Core Tag."
  (funcall (find-symbol "TAG" "COMMON-LISP-PORTFOLIO-FOREVER/TYPES/CORE") text))

(defun ct-tagline (lang text)
  (funcall (find-symbol "TAGLINE" "COMMON-LISP-PORTFOLIO-FOREVER/TYPES/CORE") lang text))

(defun ct-link (label url)
  (funcall (find-symbol "LINK" "COMMON-LISP-PORTFOLIO-FOREVER/TYPES/CORE") label url))

(defun ct-some (value)
  "Create a Coalton Some (Optional)."
  (coalton:Some value))

(defun ct-none ()
  "Coalton None (Optional)."
  coalton:None)

(defun ct-nil ()
  "Coalton Nil (empty list)."
  coalton:Nil)

(defun ct-true ()
  "Coalton True."
  coalton:True)

(defun ct-false ()
  "Coalton False."
  coalton:False)

(defun ct-person (name short long location available email taglines)
  (funcall (find-symbol "PERSON" "COMMON-LISP-PORTFOLIO-FOREVER/TYPES/CORE")
           name short long location available email taglines))

(defun ct-project (slug title summary tags links cat year month color wip)
  (funcall (find-symbol "PROJECT" "COMMON-LISP-PORTFOLIO-FOREVER/TYPES/CORE")
           slug title summary tags links cat year month color wip))

(defun ct-section (heading body keywords)
  (funcall (find-symbol "SECTION" "COMMON-LISP-PORTFOLIO-FOREVER/TYPES/CORE")
           heading body keywords))

(defun ct-skill (name category tags)
  (funcall (find-symbol "SKILL" "COMMON-LISP-PORTFOLIO-FOREVER/TYPES/CORE")
           name category tags))

(defun ct-talk (title type date description link)
  (funcall (find-symbol "TALK" "COMMON-LISP-PORTFOLIO-FOREVER/TYPES/CORE")
           title type date description link))

(defun ct-portfolio (person projects skills sections talks)
  (funcall (find-symbol "PORTFOLIO" "COMMON-LISP-PORTFOLIO-FOREVER/TYPES/CORE")
           person projects skills sections talks))

;;; ============================================================
;;; Validation wrappers
;;; ============================================================

(defun validate-portfolio (portfolio-value)
  "Validate a Coalton Portfolio value. Returns T if valid, NIL with errors."
  (let ((fn (find-symbol "VALIDATE-PORTFOLIO"
                         "COMMON-LISP-PORTFOLIO-FOREVER/TYPES/VALIDATION")))
    (funcall fn portfolio-value)))

(defun portfolio-valid-p (portfolio-value)
  "True if the portfolio has no validation errors."
  (let ((fn (find-symbol "IS-PORTFOLIO-VALID?"
                         "COMMON-LISP-PORTFOLIO-FOREVER/TYPES/VALIDATION")))
    (funcall fn portfolio-value)))

;;; ============================================================
;;; Load hook
;;; ============================================================

(format t "~&;;; Portfolio Content Engine loaded.~%")
(format t "~&;;; Try: (portfolio:validate-portfolio ...)~%")
