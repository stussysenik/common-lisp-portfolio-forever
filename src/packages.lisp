;;;; packages.lisp — Package definitions

(in-package #:cl-user)

;;; CL package for runtime code (interrogation, emit, CLI)
;;; We do NOT :use coalton to avoid 30+ name conflicts with CL.
;;; Coalton symbols are accessed via coalton: prefix.
;;; Coalton type constructors become CL functions in their Coalton package.

(defpackage #:portfolio
  (:documentation "Common Lisp runtime for portfolio content engine.")
  (:use #:cl)
  (:export
   #:*portfolio*
   #:ct-tag #:ct-tagline #:ct-link #:ct-some #:ct-none #:ct-nil #:ct-true #:ct-false
   #:ct-person #:ct-project #:ct-section #:ct-skill #:ct-talk #:ct-portfolio
   #:validate-portfolio #:portfolio-valid-p
   #:describe-portfolio
   #:ask-person #:ask-project #:ask-section #:ask-skill #:ask-talk
   #:add-project #:add-skill
   #:make-tag #:make-tagline #:make-link
   #:clist #:clist*
   #:export-all #:preview-profile #:preview-project #:preview-all
   #:scaffold #:refine #:ship
   #:ls #:edit #:new #:rm
   #:save #:restore
   #:bootstrap
   #:seed-person #:seed-projects #:seed-skills #:seed-talks
   #:*content-target*))


