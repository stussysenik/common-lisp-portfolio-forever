;;;; interrogate.lisp — REPL interrogation functions for portfolio content
;;;;
;;;; Each ask-* function interactively prompts the user to construct
;;;; a Coalton type value. Shows existing values as defaults (progressive
;;;; completion). All return Coalton values ready for portfolio assembly.
;;;;
;;;; Usage:
;;;;   (portfolio:describe-portfolio)   — full interactive builder
;;;;   (portfolio:ask-person)           — just the profile
;;;;   (portfolio:ask-project)          — just one project

(in-package #:portfolio)

;;; ============================================================
;;; Coalton symbol helpers
;;; ============================================================

(defvar *ct-core* (or (find-package "COMMON-LISP-PORTFOLIO-FOREVER/TYPES/CORE")
                       (error "Core Coalton package not loaded yet.")))

(defvar *ct-valid* (or (find-package "COMMON-LISP-PORTFOLIO-FOREVER/TYPES/VALIDATION")
                        (error "Validation Coalton package not loaded yet.")))

(defun ct-fn (pkg name)
  "Resolve a Coalton function symbol at runtime."
  (let ((sym (find-symbol (string-upcase name) pkg)))
    (unless sym
      (error "Coalton symbol ~A not found in ~A" name (package-name pkg)))
    sym))

(defun ct-call (pkg name &rest args)
  "Call a Coalton function at runtime."
  (apply (ct-fn pkg name) args))

;;; ============================================================
;;; Coalton value constructors (shorthand)
;;; ============================================================

(defun make-tag (text)
  (ct-call *ct-core* "Tag" text))

(defun make-tagline (lang text)
  (ct-call *ct-core* "Tagline" lang text))

(defun make-link (label url)
  (ct-call *ct-core* "Link" label url))

(defun make-person (name short long location available email taglines)
  (ct-call *ct-core* "Person" name short long location available email taglines))

(defun make-project (slug title summary tags links category year month color wip)
  (ct-call *ct-core* "Project" slug title summary tags links category year month color wip))

(defun make-skill (name category tags)
  (ct-call *ct-core* "Skill" name category tags))

(defun make-section (heading body keywords)
  (ct-call *ct-core* "Section" heading body keywords))

(defun make-talk (title type date description link)
  (ct-call *ct-core* "Talk" title type date description link))

(defun make-portfolio (person projects skills sections talks)
  (ct-call *ct-core* "Portfolio" person projects skills sections talks))

(defun make-content-paragraph (text)
  (ct-call *ct-core* "Paragraph" text))

(defun make-content-bulletlist (items)
  (ct-call *ct-core* "BulletList" items))

(defun make-content-codeblock (lang code)
  (ct-call *ct-core* "CodeBlock" lang code))

;;; ============================================================
;;; Coalton list builder
;;; ============================================================

(defun clist (&rest items)
  "Build a Coalton List from CL values (must already be Coalton values)."
  (if (null items)
      coalton:Nil
      (coalton:Cons (car items) (clist* (cdr items)))))

(defun clist* (items)
  "Build a Coalton List from a CL list of Coalton values."
  (if (null items)
      coalton:Nil
      (coalton:Cons (car items) (clist* (cdr items)))))

;;; ============================================================
;;; Coalton list accessors (for extracting defaults)
;;; ============================================================

(defun ct-access (pkg fname value)
  "Call a Coalton accessor on VALUE."
  (funcall (ct-fn pkg fname) value))

;;; ============================================================
;;; Optional helpers
;;; ============================================================

(defun ct-some-p (optional)
  "True if OPTIONAL is a Coalton Some."
  (not (eq optional coalton:None)))

(defun ct-maybe-unwrap (optional)
  "Extract value from a Coalton Optional, or NIL if None."
  (if (eq optional coalton:None)
      nil
      optional))

;;; ============================================================
;;; Prompt primitives
;;; ============================================================

(defun prompt (control-string &rest args)
  "Print a prompt to *query-io* without newline."
  (apply #'format *query-io* control-string args)
  (finish-output *query-io*))

(defun get-line ()
  "Read a line from *query-io*, trimming whitespace."
  (string-trim '(#\Space #\Tab) (read-line *query-io* nil "")))

(defun ask-string (text &optional default)
  "Prompt for a string. Shows DEFAULT in brackets if provided.
   Empty input returns DEFAULT (or empty string if no default)."
  (prompt "~&~A" text)
  (when default
    (prompt " [~A]" default))
  (prompt ": ")
  (let ((input (get-line)))
    (if (string= input "")
        (or default "")
        input)))

(defun ask-integer (text &optional default)
  "Prompt for an integer."
  (loop
    (prompt "~&~A" text)
    (when default
      (prompt " [~D]" default))
    (prompt ": ")
    (let* ((raw (get-line))
           (input (if (string= raw "")
                      (format nil "~D" (or default 0))
                      raw)))
      (handler-case
          (return (parse-integer input))
        (error ()
          (format *query-io* "~&  Please enter a number.~%"))))))

(defun ask-boolean (text &optional default)
  "Prompt y/n, return Coalton Boolean (True/False)."
  (loop
    (prompt "~&~A (y/n)" text)
    (when default
      (prompt " [~A]" (if default "y" "n")))
    (prompt ": ")
    (let ((input (string-downcase (get-line))))
      (cond
        ((search "y" input) (return coalton:True))
        ((search "n" input) (return coalton:False))
        ((string= input "")
         (return (if default coalton:True coalton:False)))
        (t (format *query-io* "~&  Please answer y or n.~%"))))))

(defun ask-optional-string (text &optional default)
  "Prompt for an optional string. Empty input = coalton:None.
   If DEFAULT is provided, shows it and uses coalton:Some on empty input."
  (prompt "~&~A (optional, enter to skip)" text)
  (when default
    (prompt " [~A]" default))
  (prompt ": ")
  (let ((input (get-line)))
    (cond
      ((and (string= input "") default)
       (coalton:Some default))
      ((string= input "")
       coalton:None)
      (t
       (coalton:Some input)))))

;;; ============================================================
;;; Collection prompts (tags, taglines, links)
;;; ============================================================

(defun ask-tags (label &optional existing-tags)
  "Collect Coalton Tags. Shows existing tags as defaults if any.
   EXISTING-TAGS should be a Coalton (List Tag)."
  (let ((result (if existing-tags existing-tags coalton:Nil)))
    (format *query-io* "~&~%  --- ~A ---" label)
    (when (and existing-tags (not (eq existing-tags coalton:Nil)))
      (format *query-io* "~&  Existing tags will be preserved. Add more below.~%"))
    (loop
      (prompt "~&  Tag (enter to finish): ")
      (let ((input (get-line)))
        (if (string= input "")
            (return)
            (setf result (coalton:Cons (make-tag input) result)))))
    (ct-reverse result)))

(defun ask-taglines (&optional existing-taglines)
  "Collect Coalton Taglines (lang + text pairs)."
  (let ((result (if existing-taglines existing-taglines coalton:Nil)))
    (format *query-io* "~&~%  --- Taglines ---")
    (loop
      (prompt "~&  Language code (e.g. en, fr) or enter to finish: ")
      (let ((lang (get-line)))
        (when (string= lang "")
          (return))
        (prompt "~&    Text for ~A: " lang)
        (let ((text (get-line)))
          (unless (string= text "")
            (setf result (coalton:Cons (make-tagline lang text) result))))))
    (ct-reverse result)))

(defun ask-links (&optional existing-links)
  "Collect Coalton Links (label + url pairs)."
  (let ((result (if existing-links existing-links coalton:Nil)))
    (format *query-io* "~&~%  --- Links ---")
    (loop
      (prompt "~&  Link label (enter to finish): ")
      (let ((label (get-line)))
        (when (string= label "")
          (return))
        (prompt "~&    URL for ~A: " label)
        (let ((url (get-line)))
          (unless (string= url "")
            (setf result (coalton:Cons (make-link label url) result))))))
    (ct-reverse result)))

;;; ============================================================
;;; Coalton list reversal helper
;;; ============================================================

(defun ct-reverse (ct-list)
  "Reverse a Coalton List (coalton:List)."
  (let ((result coalton:Nil))
    (loop while (not (eq ct-list coalton:Nil))
          do (setf result (coalton:Cons (cl:car ct-list) result)
                   ct-list (cl:cdr ct-list)))
    result))

;;; ============================================================
;;; Entity interrogators
;;; ============================================================

(defun ask-person (&optional existing)
  "Interactively build a Coalton Person. EXISTING provides defaults.
   Returns a Coalton Person value."
  (let* ((def-name (when existing (ct-access *ct-core* "person-name" existing)))
         (def-short (when existing (ct-access *ct-core* "person-short-bio" existing)))
         (def-long (when existing (ct-access *ct-core* "person-long-bio" existing)))
         (def-loc (when existing (ct-access *ct-core* "person-location" existing)))
         (def-avail (when existing (ct-access *ct-core* "person-available" existing)))
         (def-email (when existing (ct-access *ct-core* "person-email" existing)))
         (def-tls (when existing (ct-access *ct-core* "person-taglines" existing))))

    (format *query-io* "~&~%╔══════════════════════════════════════╗")
    (format *query-io* "~&║        PERSON (PROFILE)              ║")
    (format *query-io* "~&╚══════════════════════════════════════╝~%")

    (let* ((name (ask-string "Your full name" def-name))
           (short (ask-string "Short bio (one sentence)" def-short))
           (long-opt (let ((def-long-str (ct-maybe-unwrap def-long)))
                       (ask-optional-string "Long bio (multi-paragraph)" def-long-str)))
           (location (ask-string "Location (City, Country)" def-loc))
           (available (ask-boolean "Available for work" (when existing
                                                          (eq def-avail coalton:True))))
           (email (ask-string "Email address" def-email))
           (taglines (ask-taglines def-tls)))

      (let ((person (make-person name short long-opt location available email taglines)))
        (show-validation "person" (validate-entity "validate-person" person))
        person))))

(defun ask-project (&optional existing)
  "Interactively build a Coalton Project.
   Returns a Coalton Project value."
  (let* ((def-slug (when existing (ct-access *ct-core* "project-slug" existing)))
         (def-title (when existing (ct-access *ct-core* "project-title" existing)))
         (def-summary (when existing (ct-access *ct-core* "project-summary" existing)))
         (def-tags (when existing (ct-access *ct-core* "project-tags" existing)))
         (def-links (when existing (ct-access *ct-core* "project-links" existing)))
         (def-cat (when existing (ct-access *ct-core* "project-category" existing)))
         (def-year (when existing (ct-access *ct-core* "project-year" existing)))
         (def-month (when existing (ct-access *ct-core* "project-month" existing)))
         (def-wip (when existing (ct-access *ct-core* "project-wip" existing))))

    (format *query-io* "~&~%╔══════════════════════════════════════╗")
    (format *query-io* "~&║        PROJECT                       ║")
    (format *query-io* "~&╚══════════════════════════════════════╝~%")

    (let* ((title (ask-string "Project title" def-title))
           (slug (progn
                   (format *query-io* "~&")
                   (ask-string "URL slug (lowercase, hyphens)"
                               (or def-slug
                                   (slugify title)))))
           (summary (ask-string "Short summary (1-2 sentences)" def-summary))
           (category (ask-string "Category (e.g. compilers, web, game)" def-cat))
           (year (ask-integer "Year (YYYY)" def-year))
           (month (ask-integer "Month (1-12)" def-month))
           (tags (ask-tags "Project tags" def-tags))
           (links (ask-links def-links))
           (featured-color (ask-optional-string "Featured color (hex, e.g. #ff6b6b)" nil))
           (wip (ask-boolean "Work in progress?" (when existing
                                                   (eq def-wip coalton:True)))))

      (let ((project (make-project slug title summary tags links category year month
                                   featured-color wip)))
        (show-validation "project" (validate-entity "validate-project" project))
        project))))

(defun ask-section (&optional existing)
  "Interactively build a Coalton Section.
   Returns a Coalton Section value."
  (let* ((def-heading (when existing (ct-access *ct-core* "section-heading" existing)))
         (def-keywords (when existing (ct-access *ct-core* "section-keywords" existing))))

    (format *query-io* "~&~%╔══════════════════════════════════════╗")
    (format *query-io* "~&║        SECTION (PAGE)                 ║")
    (format *query-io* "~&╚══════════════════════════════════════╝~%")

    (let* ((heading (ask-string "Section heading" def-heading))
           (body (ask-content-blocks))
           (keywords (ask-tags "Section keywords/tags" def-keywords)))

      (let ((section (make-section heading body keywords)))
        (show-validation "section" (validate-entity "validate-section" section))
        section))))

(defun ask-skill (&optional existing)
  "Interactively build a Coalton Skill.
   Returns a Coalton Skill value."
  (let* ((def-name (when existing (ct-access *ct-core* "skill-name" existing)))
         (def-cat (when existing (ct-access *ct-core* "skill-category" existing)))
         (def-tags (when existing (ct-access *ct-core* "skill-tags" existing))))

    (format *query-io* "~&~%╔══════════════════════════════════════╗")
    (format *query-io* "~&║        SKILL                         ║")
    (format *query-io* "~&╚══════════════════════════════════════╝~%")

    (let* ((name (ask-string "Skill name" def-name))
           (category (ask-string "Category (e.g. Languages, Frameworks, Tools)" def-cat))
           (tags (ask-tags "Skill tags" def-tags)))

      (let ((skill (make-skill name category tags)))
        (show-validation "skill" (validate-entity "validate-skill" skill))
        skill))))

(defun ask-talk (&optional existing)
  "Interactively build a Coalton Talk.
   Returns a Coalton Talk value."
  (let* ((def-title (when existing (ct-access *ct-core* "talk-title" existing)))
         (def-type (when existing (ct-access *ct-core* "talk-type" existing)))
         (def-date (when existing (ct-access *ct-core* "talk-date" existing)))
         (def-desc (when existing (ct-access *ct-core* "talk-description" existing)))
         (def-link (when existing (ct-access *ct-core* "talk-link" existing)))
         (def-link-str (ct-maybe-unwrap def-link)))

    (format *query-io* "~&~%╔══════════════════════════════════════╗")
    (format *query-io* "~&║        TALK / INTERVIEW              ║")
    (format *query-io* "~&╚══════════════════════════════════════╝~%")

    (let* ((title (ask-string "Talk title" def-title))
           (talk-type (let ((raw (ask-string "Type (talk / interview)"
                                             (or def-type "talk"))))
                        (if (search "interview" raw :test #'char-equal)
                            "interview"
                            "talk")))
           (date (ask-string "Date (YYYY-MM-DD)" def-date))
           (description (ask-string "Description" def-desc))
           (link-opt (ask-optional-string "Link URL" def-link-str)))

      (let ((talk (make-talk title talk-type date description link-opt)))
        (show-validation "talk" (validate-entity "validate-talk" talk))
        talk))))

;;; ============================================================
;;; Content block builder
;;; ============================================================

(defun ask-content-blocks ()
  "Interactively build a Coalton List of Content blocks.
   Supports Paragraph, BulletList, and CodeBlock."
  (let ((result coalton:Nil))
    (format *query-io* "~&~%  --- Section content (body) ---")
    (loop
      (format *query-io* "~&")
      (prompt "~&  Add content: [p]aragraph, [b]ullet list, [c]ode block, or enter to finish: ")
      (let ((choice (string-downcase (get-line))))
        (cond
          ((string= choice "") (return))
          ((string= choice "p")
           (prompt "~&    Paragraph text: ")
           (let ((text (get-line)))
             (unless (string= text "")
               (setf result (coalton:Cons (make-content-paragraph text) result)))))
          ((string= choice "b")
           (let ((items coalton:Nil))
             (loop
               (prompt "~&    Bullet point (enter to finish bullet list): ")
               (let ((item (get-line)))
                 (if (string= item "")
                     (return)
                     (setf items (coalton:Cons item items)))))
             (unless (eq items coalton:Nil)
               (setf result (coalton:Cons (make-content-bulletlist (ct-reverse items))
                                          result)))))
          ((string= choice "c")
           (prompt "~&    Language: ")
           (let ((lang (get-line)))
             (prompt "~&    Code (end with a line containing only .):~%")
             (let ((lines nil))
               (loop
                 (prompt "~&      ")
                 (let ((line (get-line)))
                   (if (string= line ".")
                       (return)
                       (push line lines))))
               (let ((code (format nil "~{~A~^~%~}" (nreverse lines))))
                 (unless (string= code "")
                    (setf result (coalton:Cons (make-content-codeblock lang code)
                                               result)))))))
           (t (format *query-io* "~&  Unknown choice: ~A~%" choice)))))
    (ct-reverse result)))

;;; ============================================================
;;; Validation helpers
;;; ============================================================

(defun validate-entity (fn-name entity)
  "Run a Coalton validation function on ENTITY.
   FN-NAME is a string like \"validate-person\".
   Returns the Coalton (List String) of errors."
  (funcall (ct-fn *ct-valid* fn-name) entity))

(defun show-validation (entity-name errors)
  "Display validation results. ERRORS is a Coalton (List String)."
  (if (eq errors coalton:Nil)
      (format *query-io* "~&  ✓ ~A is valid.~%" entity-name)
      (progn
        (format *query-io* "~&  ✗ ~A has issues:~%" entity-name)
        (loop while (not (eq errors coalton:Nil))
              do (format *query-io* "    - ~A~%" (cl:car errors))
                 (setf errors (cl:cdr errors))))))

;;; ============================================================
;;; Portfolio orchestrator
;;; ============================================================

(defun describe-portfolio ()
  "Interactive portfolio builder. Guides the user through creating
   a complete portfolio step by step. Stores the result in *portfolio*."
  (format *query-io* "~&~%")
  (format *query-io* "~&██████████████████████████████████████████")
  (format *query-io* "~&█  PORTFOLIO CONTENT ENGINE             █")
  (format *query-io* "~&█  Interactive portfolio builder        █")
  (format *query-io* "~&██████████████████████████████████████████")
  (format *query-io* "~&~%")

  (let* ((existing-person (and *portfolio*
                               (ct-access *ct-core* "portfolio-person" *portfolio*)))
         (person (ask-person existing-person))

         (existing-projects (and *portfolio*
                                 (ct-access *ct-core* "portfolio-projects" *portfolio*)))
         (projects (ask-entity-list "PROJECT" #'ask-project existing-projects))

         (existing-skills (and *portfolio*
                               (ct-access *ct-core* "portfolio-skills" *portfolio*)))
         (skills (ask-entity-list "SKILL" #'ask-skill existing-skills))

         (existing-sections (and *portfolio*
                                 (ct-access *ct-core* "portfolio-sections" *portfolio*)))
         (sections (ask-entity-list "SECTION" #'ask-section existing-sections))

         (existing-talks (and *portfolio*
                              (ct-access *ct-core* "portfolio-talks" *portfolio*)))
         (talks (ask-entity-list "TALK" #'ask-talk existing-talks)))

    (let ((portfolio (make-portfolio person projects skills sections talks)))
      (format *query-io* "~&~%╔══════════════════════════════════════╗")
      (format *query-io* "~&║        VALIDATION SUMMARY             ║")
      (format *query-io* "~&╚══════════════════════════════════════╝~%")

      (let ((errors (validate-entity "validate-portfolio" portfolio)))
        (if (eq errors coalton:Nil)
            (progn
              (setf *portfolio* portfolio)
              (format *query-io* "~&  ✓ Portfolio is fully valid!~%")
              (format *query-io* "~&  Stored in *PORTFOLIO*.~%")
              (format *query-io* "~&  ~D projects, ~D skills, ~D sections, ~D talks~%"
                      (ct-list-length projects)
                      (ct-list-length skills)
                      (ct-list-length sections)
                      (ct-list-length talks))
              portfolio)
            (progn
              (format *query-io* "~&  ✗ Portfolio has ~D validation errors:~%"
                      (ct-list-length errors))
              (loop while (not (eq errors coalton:Nil))
                    do (format *query-io* "    - ~A~%" (cl:car errors))
                       (setf errors (cl:cdr errors)))
              (format *query-io* "~&  Portfolio NOT stored (fix errors first).~%")
              nil))))))

(defun ask-entity-list (label ask-fn &optional existing-list)
  "Interactively collect a list of entities.
   LABEL is a display string. ASK-FN is called with optional existing.
   EXISTING-LIST is a Coalton (List ...)."
  (let ((result (if existing-list existing-list coalton:Nil)))
    (loop
      (format *query-io* "~&~%  --- ~A (currently ~D) ---" label (ct-list-length result))
      (prompt "~&  [a]dd, [r]emove, [l]ist, or enter to finish: ")
      (let ((choice (string-downcase (get-line))))
        (cond
          ((string= choice "") (return (ct-reverse result)))
          ((string= choice "a")
           (let ((entity (funcall ask-fn nil)))
             (when entity
               (setf result (coalton:Cons entity result)))))
          ((string= choice "r")
           (format *query-io* "~&  (not implemented: rebuild the entity list instead)~%"))
          ((string= choice "l")
           (if (eq result coalton:Nil)
               (format *query-io* "~&  (empty)~%")
               (let ((i 0) (cur result))
                 (loop while (not (eq cur coalton:Nil))
                       do (format *query-io* "~&  ~D. ~A~%" (incf i)
                                  (handler-case
                                      (let ((e (cl:car cur)))
                                        (or (ct-safe-access e "project-title")
                                            (ct-safe-access e "skill-name")
                                            (ct-safe-access e "section-heading")
                                            (ct-safe-access e "talk-title")
                                            (ct-safe-access e "person-name")
                                            "---"))
                                    (error () "---")))
                          (setf cur (cl:cdr cur))))))
          (t (format *query-io* "~&  Unknown choice.~%")))))))

(defun ct-safe-access (entity fname)
  "Try to access a field; return NIL if it fails."
  (handler-case
      (funcall (find-symbol (string-upcase fname) *ct-core*) entity)
    (error () nil)))

(defun ct-list-length (ct-list)
  "Count elements in a Coalton List."
  (let ((n 0))
    (loop while (not (eq ct-list coalton:Nil))
          do (incf n)
             (setf ct-list (cl:cdr ct-list)))
    n))

;;; ============================================================
;;; Utility: slug generator
;;; ============================================================

(defun slugify (str)
  "Convert a string to a URL slug (lowercase, hyphens)."
  (let ((result (make-string (length str) :initial-element #\Space)))
    (loop for c across str
          for i from 0
          do (setf (char result i)
                   (cond
                     ((alphanumericp c) (char-downcase c))
                     ((char= c #\Space) #\-)
                     ((char= c #\-) #\-)
                     ((char= c #\_) #\-)
                     ((char= c #\.) #\-)
                     (t #\-))))
    (string-trim
     '(#\-)
     (remove-if (lambda (c) (and (char/= c #\-) (not (alphanumericp c)))) result))))

;;; ============================================================
;;; Quick entry points
;;; ============================================================

(defun add-project ()
  "Quickly add a single project to *portfolio*."
  (unless *portfolio*
    (error "No *portfolio* exists. Run (portfolio:describe-portfolio) first."))
  (let* ((proj (ask-project))
         (existing (ct-access *ct-core* "portfolio-projects" *portfolio*))
         (person (ct-access *ct-core* "portfolio-person" *portfolio*))
         (skills (ct-access *ct-core* "portfolio-skills" *portfolio*))
         (sections (ct-access *ct-core* "portfolio-sections" *portfolio*))
         (talks (ct-access *ct-core* "portfolio-talks" *portfolio*)))
    (setf *portfolio* (make-portfolio person
                                       (coalton:Cons proj existing)
                                       skills sections talks))
    (format *query-io* "~&  ✓ Project added to *portfolio*.~%")))

(defun add-skill ()
  "Quickly add a single skill to *portfolio*."
  (unless *portfolio*
    (error "No *portfolio* exists. Run (portfolio:describe-portfolio) first."))
  (let* ((skill (ask-skill))
         (existing (ct-access *ct-core* "portfolio-skills" *portfolio*))
         (person (ct-access *ct-core* "portfolio-person" *portfolio*))
         (projects (ct-access *ct-core* "portfolio-projects" *portfolio*))
         (sections (ct-access *ct-core* "portfolio-sections" *portfolio*))
         (talks (ct-access *ct-core* "portfolio-talks" *portfolio*)))
    (setf *portfolio* (make-portfolio person projects
                                       (coalton:Cons skill existing)
                                       sections talks))
    (format *query-io* "~&  ✓ Skill added to *portfolio*.~%")))

;;; ============================================================
;;; Mode 1: SCAFFOLD — capture initial content (rarely used)
;;; ============================================================

(defun scaffold (&optional target)
  "Create fresh portfolio content from scratch.
   Without arg: full portfolio builder (resets *portfolio*).
   With keyword arg: scaffold a single entity type.
   TARGET: :portfolio (default), :person, :project, :skill, :section, :talk."
  (let ((target (or target :portfolio)))
    (ecase target
      (:portfolio
       (setf *portfolio* nil)
       (format *query-io* "~&  Scaffolding fresh portfolio...~%")
       (describe-portfolio))
      (:person
       (let ((p (ask-person)))
         (format *query-io* "~&~%  Person built. Store it in a portfolio with (describe-portfolio).~%")
         p))
      (:project
       (let ((p (ask-project)))
         (format *query-io* "~&~%  Project built. Add to *portfolio* with (refine).~%")
         p))
      (:skill
       (let ((p (ask-skill)))
         (format *query-io* "~&~%  Skill built. Add to *portfolio* with (refine).~%")
         p))
      (:section
       (let ((p (ask-section)))
         (format *query-io* "~&~%  Section built. Add to *portfolio* with (refine).~%")
         p))
      (:talk
       (let ((p (ask-talk)))
         (format *query-io* "~&~%  Talk built. Add to *portfolio* with (refine).~%")
         p)))))

;;; ============================================================
;;; Mode 2: REFINE — maintain/mature/polish (daily driver)
;;; ============================================================

(defun refine (&optional entity)
  "Maintain and polish existing portfolio content.
   Without arg: interactive menu on *portfolio*.
   With keyword arg: refine a specific entity type in-place.
   ENTITY: NIL (menu), :person, :projects, :skills, :sections, :talks."
  (unless *portfolio*
    (format *query-io* "~&  No *portfolio* exists. Run (scaffold) first.~%")
    (return-from refine nil))
  (if entity
      (refine-entity entity)
      (refine-menu)))

(defun refine-menu ()
  "Interactive refinement menu for the current portfolio."
  (loop
    (format *query-io* "~&~%")
    (format *query-io* "~&╔══════════════════════════════════════╗")
    (format *query-io* "~&║        REFINE PORTFOLIO               ║")
    (format *query-io* "~&╠══════════════════════════════════════╣")
    (format *query-io* "~&║  [p]  Person (profile)               ║")
    (format *query-io* "~&║  [w]  Works (projects) ~2D             ║"
            (ct-list-length (ct-access *ct-core* "portfolio-projects" *portfolio*)))
    (format *query-io* "~&║  [s]  Skills ~2D                       ║"
            (ct-list-length (ct-access *ct-core* "portfolio-skills" *portfolio*)))
    (format *query-io* "~&║  [c]  Sections ~2D                     ║"
            (ct-list-length (ct-access *ct-core* "portfolio-sections" *portfolio*)))
    (format *query-io* "~&║  [t]  Talks ~2D                        ║"
            (ct-list-length (ct-access *ct-core* "portfolio-talks" *portfolio*)))
    (format *query-io* "~&║  [v]  Validate all                    ║")
    (format *query-io* "~&║  [st] Status / summary                ║")
    (format *query-io* "~&║  [.]  Done (back)                     ║")
    (format *query-io* "~&╚══════════════════════════════════════╝")
    (prompt "~&> ")
    (let ((choice (string-downcase (get-line))))
      (cond
        ((string= choice "p") (refine-entity :person))
        ((string= choice "w") (refine-entity :projects))
        ((string= choice "s") (refine-entity :skills))
        ((string= choice "c") (refine-entity :sections))
        ((string= choice "t") (refine-entity :talks))
        ((string= choice "v") (show-full-validation))
        ((string= choice "st") (show-status))
        ((string= choice ".") (return-from refine-menu))
        ((string= choice "") (return-from refine-menu))
        (t (format *query-io* "~&  Unknown: ~A~%" choice))))))

(defun refine-entity (entity)
  "Refine a specific entity type in the portfolio."
  (ecase entity
    (:person
     (let ((existing (ct-access *ct-core* "portfolio-person" *portfolio*))
           (projects (ct-access *ct-core* "portfolio-projects" *portfolio*))
           (skills (ct-access *ct-core* "portfolio-skills" *portfolio*))
           (sections (ct-access *ct-core* "portfolio-sections" *portfolio*))
           (talks (ct-access *ct-core* "portfolio-talks" *portfolio*)))
       (setf *portfolio* (make-portfolio (ask-person existing)
                                          projects skills sections talks))
       (format *query-io* "~&  ✓ Person updated.~%")))
    (:projects
     (let* ((existing (ct-access *ct-core* "portfolio-projects" *portfolio*))
            (updated (ask-entity-list "PROJECT" #'ask-project existing))
            (person (ct-access *ct-core* "portfolio-person" *portfolio*))
            (skills (ct-access *ct-core* "portfolio-skills" *portfolio*))
            (sections (ct-access *ct-core* "portfolio-sections" *portfolio*))
            (talks (ct-access *ct-core* "portfolio-talks" *portfolio*)))
       (setf *portfolio* (make-portfolio person updated skills sections talks))))
    (:skills
     (let* ((existing (ct-access *ct-core* "portfolio-skills" *portfolio*))
            (updated (ask-entity-list "SKILL" #'ask-skill existing))
            (person (ct-access *ct-core* "portfolio-person" *portfolio*))
            (projects (ct-access *ct-core* "portfolio-projects" *portfolio*))
            (sections (ct-access *ct-core* "portfolio-sections" *portfolio*))
            (talks (ct-access *ct-core* "portfolio-talks" *portfolio*)))
       (setf *portfolio* (make-portfolio person projects updated sections talks))))
    (:sections
     (let* ((existing (ct-access *ct-core* "portfolio-sections" *portfolio*))
            (updated (ask-entity-list "SECTION" #'ask-section existing))
            (person (ct-access *ct-core* "portfolio-person" *portfolio*))
            (projects (ct-access *ct-core* "portfolio-projects" *portfolio*))
            (skills (ct-access *ct-core* "portfolio-skills" *portfolio*))
            (talks (ct-access *ct-core* "portfolio-talks" *portfolio*)))
       (setf *portfolio* (make-portfolio person projects skills updated talks))))
    (:talks
     (let* ((existing (ct-access *ct-core* "portfolio-talks" *portfolio*))
            (updated (ask-entity-list "TALK" #'ask-talk existing))
            (person (ct-access *ct-core* "portfolio-person" *portfolio*))
            (projects (ct-access *ct-core* "portfolio-projects" *portfolio*))
            (skills (ct-access *ct-core* "portfolio-skills" *portfolio*))
            (sections (ct-access *ct-core* "portfolio-sections" *portfolio*)))
       (setf *portfolio* (make-portfolio person projects skills sections updated talks))))))

;;; ============================================================
;;; Mode 3: SHIP — validate + export (the finish line)
;;; ============================================================

(defun ship (&key (target (content-target)) (dry-run t))
  "Validate the portfolio and export to markdown.
   TARGET: output directory (default: ./output/).
   DRY-RUN: if T (default), validate only, don't write files."
  (unless *portfolio*
    (format *query-io* "~&  No *portfolio* exists. Run (scaffold) first.~%")
    (return-from ship nil))
  (format *query-io* "~&~%╔══════════════════════════════════════╗")
  (format *query-io* "~&║        SHIP — validate & export       ║")
  (format *query-io* "~&╚══════════════════════════════════════╝~%")
  (let* ((errors (funcall (ct-fn *ct-valid* "validate-portfolio") *portfolio*))
         (valid-p (eq errors coalton:Nil)))
    (if valid-p
        (progn
          (format *query-io* "~&  ✓ Portfolio is valid.~%")
          (show-status)
          (if dry-run
              (format *query-io* "~&  Dry run — no files written.~%")
              (progn
                (export-all :target target)
                (format *query-io* "~&  ✓ Shipped to ~A~%" target))))
        (progn
          (format *query-io* "~&  ✗ ~D validation errors — must fix before shipping:~%"
                  (ct-list-length errors))
          (loop while (not (eq errors coalton:Nil))
                do (format *query-io* "    - ~A~%" (cl:car errors))
                   (setf errors (cl:cdr errors)))
          (format *query-io* "~&  Run (refine) to fix issues.~%"))))
  t)

;;; ============================================================
;;; Shared helpers
;;; ============================================================

(defun show-status ()
  "Print a summary of the current portfolio."
  (let ((person (ct-access *ct-core* "portfolio-person" *portfolio*))
        (projects (ct-access *ct-core* "portfolio-projects" *portfolio*))
        (skills (ct-access *ct-core* "portfolio-skills" *portfolio*))
        (sections (ct-access *ct-core* "portfolio-sections" *portfolio*))
        (talks (ct-access *ct-core* "portfolio-talks" *portfolio*)))
    (format *query-io* "~&  ┌────────────────────────────────────┐")
    (format *query-io* "~&  │ Name: ~27A │"
            (handler-case (ct-get person "person-name") (error () "")))
    (format *query-io* "~&  ├────────────────────────────────────┤")
    (format *query-io* "~&  │ Projects: ~2D   Skills: ~2D            │"
            (ct-list-length projects) (ct-list-length skills))
    (format *query-io* "~&  │ Sections: ~2D   Talks:   ~2D            │"
            (ct-list-length sections) (ct-list-length talks))
    (let ((errors (funcall (ct-fn *ct-valid* "validate-portfolio") *portfolio*)))
      (format *query-io* "~&  ├────────────────────────────────────┤")
      (if (eq errors coalton:Nil)
          (format *query-io* "~&  │ Status: VALID                       │")
          (format *query-io* "~&  │ Status: ~2D ERROR(S)                  │"
                  (ct-list-length errors))))
    (format *query-io* "~&  └────────────────────────────────────┘~%")))

(defun show-full-validation ()
  "Run full portfolio validation and display all results."
  (let ((errors (funcall (ct-fn *ct-valid* "validate-portfolio") *portfolio*)))
    (if (eq errors coalton:Nil)
        (format *query-io* "~&  ✓ Portfolio is fully valid.~%")
        (progn
          (format *query-io* "~&  ✗ ~D validation errors:~%"
                  (ct-list-length errors))
          (loop while (not (eq errors coalton:Nil))
                do (format *query-io* "    - ~A~%" (cl:car errors))
                   (setf errors (cl:cdr errors)))))))

;;; ============================================================
;;; Load banner
;;; ============================================================

(format t "~&;;; Interrogation functions loaded.~%")
(format t "~&;;; (scaffold)  — create fresh content~%")
(format t "~&;;; (refine)    — maintain & polish~%")
(format t "~&;;; (ship)      — validate & export~%")
