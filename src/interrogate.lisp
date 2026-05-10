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
;;; Entity access helpers
;;; ============================================================

(defun portfolio-person ()
  (ct-access *ct-core* "portfolio-person" *portfolio*))
(defun portfolio-projects ()
  (ct-access *ct-core* "portfolio-projects" *portfolio*))
(defun portfolio-skills ()
  (ct-access *ct-core* "portfolio-skills" *portfolio*))
(defun portfolio-sections ()
  (ct-access *ct-core* "portfolio-sections" *portfolio*))
(defun portfolio-talks ()
  (ct-access *ct-core* "portfolio-talks" *portfolio*))

(defun rebuild-portfolio (person projects skills sections talks)
  (setf *portfolio* (make-portfolio person projects skills sections talks)))

(defun find-project (slug)
  (loop for p in (ct-list-to-cl (portfolio-projects))
        when (string= (ct-get p "project-slug") slug)
        return p))

(defun find-skill (name)
  (loop for s in (ct-list-to-cl (portfolio-skills))
        when (string= (ct-get s "skill-name") name)
        return s))

(defun find-talk (title)
  (loop for t* in (ct-list-to-cl (portfolio-talks))
        when (string= (ct-get t* "talk-title") title)
        return t*))

(defun find-section (heading)
  (loop for s in (ct-list-to-cl (portfolio-sections))
        when (string= (ct-get s "section-heading") heading)
        return s))

;;; ============================================================
;;; Pad / truncate helpers for aligned tables
;;; ============================================================

(defun pad (str width &optional (ellipsis t))
  (let* ((s (or str ""))
         (len (length s)))
    (cond
      ((= len width) s)
      ((> len width) (if ellipsis
                         (concatenate 'string (subseq s 0 (- width 3)) "...")
                         (subseq s 0 width)))
      (t (concatenate 'string s (make-string (- width len) :initial-element #\Space))))))

(defun fmt-row (fstr &rest args)
  (apply #'format *query-io* fstr args))

;;; ============================================================
;;; Mode: LS — table views of all content
;;; ============================================================

(defun ls (&optional category)
  "Table view of portfolio content.
   CATEGORY: :projects, :skills, :talks, :sections, or NIL for summary."
  (unless *portfolio*
    (format *query-io* "~&  No *portfolio* exists.~%")
    (return-from ls nil))
  (ecase (or category :all)
    (:all      (ls-all))
    (:projects (ls-projects))
    (:skills   (ls-skills))
    (:talks    (ls-talks))
    (:sections (ls-sections))))

(defun ls-all ()
  (show-status)
  (terpri)
  (ls-projects)
  (terpri)
  (ls-skills)
  (terpri)
  (ls-talks))

(defun ls-projects ()
  (let ((projects (ct-list-to-cl (portfolio-projects))))
    (fmt-row "~&┌────┬──────────────────────┬──────────────────────────────────────┬────┬──────┬────────────────────┐~%")
    (fmt-row "~&│  # │ SLUG                  │ TITLE                                │ YR │ WIP  │ CATEGORY           │~%")
    (fmt-row "~&├────┼──────────────────────┼──────────────────────────────────────┼────┼──────┼────────────────────┤~%")
    (loop for i from 1
          for p in projects
          do (fmt-row "~&│ ~2D │ ~A │ ~A │ ~2D │ ~A │ ~A │~%"
                       i
                       (pad (ct-get p "project-slug") 20)
                       (pad (ct-get p "project-title") 36)
                       (ct-get p "project-year")
                       (pad (if (eq (ct-get p "project-wip") coalton:True) "✓" " ") 4)
                       (pad (ct-get p "project-category") 18)))
    (fmt-row "~&└────┴──────────────────────┴──────────────────────────────────────┴────┴──────┴────────────────────┘~%")
    (fmt-row "~&  ~D projects. Select by slug: (edit :project \"slug\") or by #:~%" (length projects))))

(defun ls-skills ()
  (let ((skills (ct-list-to-cl (portfolio-skills))))
    (fmt-row "~&┌────┬──────────────────────┬────────────────────────────┐~%")
    (fmt-row "~&│  # │ NAME                 │ CATEGORY                   │~%")
    (fmt-row "~&├────┼──────────────────────┼────────────────────────────┤~%")
    (loop for i from 1
          for s in skills
          do (fmt-row "~&│ ~2D │ ~A │ ~A │~%"
                       i
                       (pad (ct-get s "skill-name") 20)
                       (pad (ct-get s "skill-category") 26)))
    (fmt-row "~&└────┴──────────────────────┴────────────────────────────┘~%")
    (fmt-row "~&  ~D skills. Select by name: (edit :skill \"Name\")~%" (length skills))))

(defun ls-talks ()
  (let ((talks (ct-list-to-cl (portfolio-talks))))
    (fmt-row "~&┌────┬────────────────────────────────────────────┬───────────┬──────────┬─────────┐~%")
    (fmt-row "~&│  # │ TITLE                                      │ TYPE      │ DATE     │ LINK    │~%")
    (fmt-row "~&├────┼────────────────────────────────────────────┼───────────┼──────────┼─────────┤~%")
    (loop for i from 1
          for t* in talks
          do (fmt-row "~&│ ~2D │ ~A │ ~A │ ~A │ ~A │~%"
                       i
                       (pad (ct-get t* "talk-title") 42)
                       (pad (ct-get t* "talk-type") 9)
                       (pad (ct-get t* "talk-date") 8)
                       (pad (ct-optional-value t* "talk-link") 7)))
    (fmt-row "~&└────┴────────────────────────────────────────────┴───────────┴──────────┴─────────┘~%")
    (fmt-row "~&  ~D talks. Select by title: (edit :talk \"Title\")~%" (length talks))))

(defun ls-sections ()
  (let ((sections (ct-list-to-cl (portfolio-sections))))
    (if (null sections)
        (fmt-row "~&  (no sections)~%")
        (progn
          (fmt-row "~&┌────┬──────────────────────────────┬──────────┐~%")
          (fmt-row "~&│  # │ HEADING                      │ KEYWORDS │~%")
          (fmt-row "~&├────┼──────────────────────────────┼──────────┤~%")
          (loop for i from 1
                for s in sections
                do (fmt-row "~&│ ~2D │ ~A │ ~3D │~%"
                             i
                             (pad (ct-get s "section-heading") 28)
                             (ct-list-length (ct-get s "section-keywords"))))
          (fmt-row "~&└────┴──────────────────────────────┴──────────┘~%"))))
  (fmt-row "~&  Select by heading: (edit :section \"Heading\")~%"))

;;; ============================================================
;;; Mode: EDIT — edit a single entity by slug/name
;;; ============================================================

(defun edit (category key)
  "Edit a specific entity identified by CATEGORY and KEY.
   CATEGORY: :project (key=slug), :skill (key=name), :talk (key=title),
             :section (key=heading), :person (key ignored).
   Examples: (edit :project \"breakdex\")  (edit :skill \"Motion Design\")"
  (unless *portfolio*
    (format *query-io* "~&  No *portfolio* exists.~%")
    (return-from edit nil))
  (ecase category
    (:person  (edit-person))
    (:project (edit-project-by-slug key))
    (:skill   (edit-skill-by-name key))
    (:talk    (edit-talk-by-title key))
    (:section (edit-section-by-heading key))))

(defun edit-person ()
  (let ((existing (portfolio-person)))
    (rebuild-portfolio (ask-person existing)
                       (portfolio-projects)
                       (portfolio-skills)
                       (portfolio-sections)
                       (portfolio-talks))
    (fmt-row "~&  ✓ Person updated.~%")))

(defun edit-project-by-slug (slug)
  (let* ((existing (find-project slug)))
    (unless existing
      (format *query-io* "~&  ✗ No project with slug \"~A\"~%" slug)
      (return-from edit-project-by-slug nil))
    (let ((updated (ask-project existing)))
      (rebuild-portfolio (portfolio-person)
                         (replace-in-list (portfolio-projects)
                                          (lambda (p) (string= (ct-get p "project-slug") slug))
                                          updated)
                         (portfolio-skills)
                         (portfolio-sections)
                         (portfolio-talks))
      (fmt-row "~&  ✓ Project \"~A\" updated.~%" slug))))

(defun edit-skill-by-name (name)
  (let* ((existing (find-skill name)))
    (unless existing
      (format *query-io* "~&  ✗ No skill named \"~A\"~%" name)
      (return-from edit-skill-by-name nil))
    (let ((updated (ask-skill existing)))
      (rebuild-portfolio (portfolio-person)
                         (portfolio-projects)
                         (replace-in-list (portfolio-skills)
                                          (lambda (s) (string= (ct-get s "skill-name") name))
                                          updated)
                         (portfolio-sections)
                         (portfolio-talks))
      (fmt-row "~&  ✓ Skill \"~A\" updated.~%" name))))

(defun edit-talk-by-title (title)
  (let* ((existing (find-talk title)))
    (unless existing
      (format *query-io* "~&  ✗ No talk titled \"~A\"~%" title)
      (return-from edit-talk-by-title nil))
    (let ((updated (ask-talk existing)))
      (rebuild-portfolio (portfolio-person)
                         (portfolio-projects)
                         (portfolio-skills)
                         (portfolio-sections)
                         (replace-in-list (portfolio-talks)
                                          (lambda (t*) (string= (ct-get t* "talk-title") title))
                                          updated))
      (fmt-row "~&  ✓ Talk \"~A\" updated.~%" title))))

(defun edit-section-by-heading (heading)
  (let* ((existing (find-section heading)))
    (unless existing
      (format *query-io* "~&  ✗ No section \"~A\"~%" heading)
      (return-from edit-section-by-heading nil))
    (let ((updated (ask-section existing)))
      (rebuild-portfolio (portfolio-person)
                         (portfolio-projects)
                         (portfolio-skills)
                         (replace-in-list (portfolio-sections)
                                          (lambda (s) (string= (ct-get s "section-heading") heading))
                                          updated)
                         (portfolio-talks))
      (fmt-row "~&  ✓ Section \"~A\" updated.~%" heading))))

;;; ============================================================
;;; Mode: NEW — create a new entity from scratch
;;; ============================================================

(defun new (category)
  "Create a new entity and append it to the portfolio.
   CATEGORY: :project, :skill, :talk, :section.
   Example: (new :project)"
  (unless *portfolio*
    (format *query-io* "~&  No *portfolio* exists.~%")
    (return-from new nil))
  (ecase category
    (:project (new-project))
    (:skill   (new-skill))
    (:talk    (new-talk))
    (:section (new-section))))

(defun new-project ()
  (let ((p (ask-project)))
    (when p
      (rebuild-portfolio (portfolio-person)
                         (coalton:Cons p (portfolio-projects))
                         (portfolio-skills)
                         (portfolio-sections)
                         (portfolio-talks))
      (fmt-row "~&  ✓ Project \"~A\" added.~%" (ct-get p "project-slug")))))

(defun new-skill ()
  (let ((s (ask-skill)))
    (when s
      (rebuild-portfolio (portfolio-person)
                         (portfolio-projects)
                         (coalton:Cons s (portfolio-skills))
                         (portfolio-sections)
                         (portfolio-talks))
      (fmt-row "~&  ✓ Skill \"~A\" added.~%" (ct-get s "skill-name")))))

(defun new-talk ()
  (let ((t* (ask-talk)))
    (when t*
      (rebuild-portfolio (portfolio-person)
                         (portfolio-projects)
                         (portfolio-skills)
                         (portfolio-sections)
                         (coalton:Cons t* (portfolio-talks)))
      (fmt-row "~&  ✓ Talk \"~A\" added.~%" (ct-get t* "talk-title")))))

(defun new-section ()
  (let ((s (ask-section)))
    (when s
      (rebuild-portfolio (portfolio-person)
                         (portfolio-projects)
                         (portfolio-skills)
                         (coalton:Cons s (portfolio-sections))
                         (portfolio-talks))
      (fmt-row "~&  ✓ Section \"~A\" added.~%" (ct-get s "section-heading")))))

;;; ============================================================
;;; Mode: RM — remove an entity by slug/name
;;; ============================================================

(defun rm (category key)
  "Remove an entity identified by CATEGORY and KEY.
   Examples: (rm :project \"breakdex\")  (rm :skill \"Sound Design\")"
  (unless *portfolio*
    (format *query-io* "~&  No *portfolio* exists.~%")
    (return-from rm nil))
  (ecase category
    (:project (rm-project key))
    (:skill   (rm-skill key))
    (:talk    (rm-talk key))
    (:section (rm-section key))))

(defun rm-project (slug)
  (let ((existing (find-project slug)))
    (unless existing
      (format *query-io* "~&  ✗ No project \"~A\"~%" slug)
      (return-from rm-project nil))
    (rebuild-portfolio (portfolio-person)
                       (remove-from-list (portfolio-projects)
                                         (lambda (p) (string= (ct-get p "project-slug") slug)))
                       (portfolio-skills)
                       (portfolio-sections)
                       (portfolio-talks))
    (fmt-row "~&  ✓ Project \"~A\" removed.~%" slug)))

(defun rm-skill (name)
  (let ((existing (find-skill name)))
    (unless existing
      (format *query-io* "~&  ✗ No skill \"~A\"~%" name)
      (return-from rm-skill nil))
    (rebuild-portfolio (portfolio-person)
                       (portfolio-projects)
                       (remove-from-list (portfolio-skills)
                                         (lambda (s) (string= (ct-get s "skill-name") name)))
                       (portfolio-sections)
                       (portfolio-talks))
    (fmt-row "~&  ✓ Skill \"~A\" removed.~%" name)))

(defun rm-talk (title)
  (let ((existing (find-talk title)))
    (unless existing
      (format *query-io* "~&  ✗ No talk \"~A\"~%" title)
      (return-from rm-talk nil))
    (rebuild-portfolio (portfolio-person)
                       (portfolio-projects)
                       (portfolio-skills)
                       (portfolio-sections)
                       (remove-from-list (portfolio-talks)
                                         (lambda (t*) (string= (ct-get t* "talk-title") title))))
    (fmt-row "~&  ✓ Talk \"~A\" removed.~%" title)))

(defun rm-section (heading)
  (let ((existing (find-section heading)))
    (unless existing
      (format *query-io* "~&  ✗ No section \"~A\"~%" heading)
      (return-from rm-section nil))
    (rebuild-portfolio (portfolio-person)
                       (portfolio-projects)
                       (portfolio-skills)
                       (remove-from-list (portfolio-sections)
                                         (lambda (s) (string= (ct-get s "section-heading") heading)))
                       (portfolio-talks))
    (fmt-row "~&  ✓ Section \"~A\" removed.~%" heading)))

;;; ============================================================
;;; Coalton list: replace / remove element
;;; ============================================================

(defun replace-in-list (ct-list pred new-item)
  "Replace first element matching PRED with NEW-ITEM in a Coalton List."
  (if (eq ct-list coalton:Nil)
      coalton:Nil
      (if (funcall pred (cl:car ct-list))
          (coalton:Cons new-item (cl:cdr ct-list))
          (coalton:Cons (cl:car ct-list) (replace-in-list (cl:cdr ct-list) pred new-item)))))

(defun remove-from-list (ct-list pred)
  "Remove all elements matching PRED from a Coalton List."
  (if (eq ct-list coalton:Nil)
      coalton:Nil
      (if (funcall pred (cl:car ct-list))
          (remove-from-list (cl:cdr ct-list) pred)
          (coalton:Cons (cl:car ct-list) (remove-from-list (cl:cdr ct-list) pred)))))

;;; ============================================================
;;; Mode: SCAFFOLD — capture initial content
;;; ============================================================

(defun scaffold (&optional target)
  "Create fresh portfolio content from scratch.
   Without arg: full portfolio builder (resets *portfolio*).
   With keyword: scaffold a single entity type.
   TARGET: :portfolio (default), :person, :project, :skill, :section, :talk."
  (let ((target (or target :portfolio)))
    (ecase target
      (:portfolio
       (setf *portfolio* nil)
       (format *query-io* "~&  Scaffolding fresh portfolio...~%")
       (describe-portfolio))
      (:person
       (let ((p (ask-person)))
         (format *query-io* "~&~%  Person built. Use in portfolio or with (bootstrap).~%")
         p))
      (:project (let ((p (ask-project))) (format *query-io* "~&~%  Use (new :project) to add to portfolio.~%") p))
      (:skill   (let ((p (ask-skill))) (format *query-io* "~&~%  Use (new :skill) to add to portfolio.~%") p))
      (:section (let ((p (ask-section))) (format *query-io* "~&~%  Use (new :section) to add to portfolio.~%") p))
      (:talk    (let ((p (ask-talk))) (format *query-io* "~&~%  Use (new :talk) to add to portfolio.~%") p)))))

;;; ============================================================
;;; Mode: REFINE — parent drill-down menu
;;; ============================================================

(defun refine (&optional entity)
  "Maintain and polish existing portfolio content.
   Without arg: parent menu showing all categories.
   With keyword: jump directly to entity type.
   ENTITY: NIL (menu), :person, :projects, :skills, :sections, :talks."
  (unless *portfolio*
    (format *query-io* "~&  No *portfolio* exists. Run (bootstrap) or (scaffold) first.~%")
    (return-from refine nil))
  (if entity
      (refine-category entity)
      (refine-menu)))

(defun refine-menu ()
  (loop
    (terpri)
    (show-status)
    (terpri)
    (fmt-row "~&  [p]  Person       [ls] List all tables~%")
    (fmt-row "~&  [w]  Projects     [n]  New entity~%")
    (fmt-row "~&  [s]  Skills       [.]  Done~%")
    (fmt-row "~&  [t]  Talks                      ~%")
    (fmt-row "~&  [c]  Sections                   ~%")
    (prompt "~&> ")
    (let ((choice (string-downcase (get-line))))
      (cond
        ((string= choice "p") (refine-category :person))
        ((string= choice "w") (refine-category :projects))
        ((string= choice "s") (refine-category :skills))
        ((string= choice "t") (refine-category :talks))
        ((string= choice "c") (refine-category :sections))
        ((string= choice "ls") (ls-all))
        ((string= choice "n") (refine-new))
        ((string= choice ".") (return-from refine-menu))
        ((string= choice "") (return-from refine-menu))
        (t (fmt-row "~&  Unknown: ~A~%" choice))))))

(defun refine-category (cat)
  "Show table for a category, then drill-down options."
  (let ((projects (portfolio-projects))
        (skills (portfolio-skills))
        (talks (portfolio-talks)))
    (ecase cat
      (:person   (edit-person))
      (:projects (progn (ls-projects)
                        (refine-drill :project "slug")))
      (:skills   (progn (ls-skills)
                        (refine-drill :skill "name")))
      (:talks    (progn (ls-talks)
                        (refine-drill :talk "title")))
      (:sections (progn (ls-sections)
                        (refine-drill :section "heading"))))))

(defun refine-drill (cat key-label)
  "After showing a table, offer edit/new/rm by key."
  (loop
    (prompt "~&  [e]dit <~A>  [n]ew  [r]m <~A>  [.] back: " key-label key-label)
    (let* ((input (get-line))
           (parts (split-input input)))
      (cond
        ((or (string= input ".") (string= input "")) (return))
        ((string= (car parts) "e")
         (if (cdr parts)
             (edit cat (cadr parts))
             (format *query-io* "~&  Usage: e <~A>~%" key-label)))
        ((string= (car parts) "n") (new cat))
        ((string= (car parts) "r")
         (if (cdr parts)
             (rm cat (cadr parts))
             (format *query-io* "~&  Usage: r <~A>~%" key-label)))
        (t (format *query-io* "~&  Unknown: ~A~%" input))))))

(defun refine-new ()
  "Prompt which category to create a new entity in."
  (prompt "~&  New: [w] project  [s] skill  [t] talk  [c] section: ")
  (let ((choice (string-downcase (get-line))))
    (cond
      ((string= choice "w") (new :project))
      ((string= choice "s") (new :skill))
      ((string= choice "t") (new :talk))
      ((string= choice "c") (new :section))
      (t (format *query-io* "~&  Cancelled.~%")))))

;;; ============================================================
;;; Utility: split "e breakdex" into ("e" "breakdex")
;;; ============================================================

(defun split-input (str)
  (let ((pos (position #\Space str)))
    (if pos
        (list (subseq str 0 pos)
              (string-trim " " (subseq str (1+ pos))))
        (list str))))

;;; ============================================================
;;; Mode: SHIP — validate + export
;;; ============================================================

(defun ship (&key (target (content-target)) (dry-run t))
  "Validate the portfolio and export to markdown.
   TARGET: output directory (default: ./output/).
   DRY-RUN: if T (default), validate only, don't write files."
  (unless *portfolio*
    (format *query-io* "~&  No *portfolio* exists. Run (bootstrap) or (scaffold) first.~%")
    (return-from ship nil))
  (format *query-io* "~&~%╔══════════════════════════════════════╗")
  (format *query-io* "~&║        SHIP — validate & export       ║")
  (format *query-io* "~&╚══════════════════════════════════════╝~%")
  (let* ((errors (funcall (ct-fn *ct-valid* "validate-portfolio") *portfolio*))
         (valid-p (eq errors coalton:Nil)))
    (if valid-p
        (progn
          (fmt-row "~&  ✓ Portfolio is valid.~%")
          (show-status)
          (if dry-run
              (fmt-row "~&  Dry run — no files written.~%")
              (progn
                (export-all :target target)
                (fmt-row "~&  ✓ Shipped to ~A~%" target))))
        (progn
          (fmt-row "~&  ✗ ~D validation errors — must fix before shipping:~%"
                  (ct-list-length errors))
          (loop while (not (eq errors coalton:Nil))
                do (fmt-row "    - ~A~%" (cl:car errors))
                   (setf errors (cl:cdr errors)))
          (fmt-row "~&  Run (refine) to fix issues, then try again.~%"))))
  t)

;;; ============================================================
;;; Shared helpers
;;; ============================================================

(defun show-status ()
  "Print a summary of the current portfolio."
  (let ((person (portfolio-person))
        (projects (portfolio-projects))
        (skills (portfolio-skills))
        (sections (portfolio-sections))
        (talks (portfolio-talks)))
    (fmt-row "~&  ┌────────────────────────────────────┐")
    (fmt-row "~&  │ Name: ~27A │"
            (handler-case (ct-get person "person-name") (error () "")))
    (fmt-row "~&  ├────────────────────────────────────┤")
    (fmt-row "~&  │ Projects: ~2D   Skills: ~2D            │"
            (ct-list-length projects) (ct-list-length skills))
    (fmt-row "~&  │ Sections: ~2D   Talks:   ~2D            │"
            (ct-list-length sections) (ct-list-length talks))
    (let ((errors (funcall (ct-fn *ct-valid* "validate-portfolio") *portfolio*)))
      (fmt-row "~&  ├────────────────────────────────────┤")
      (if (eq errors coalton:Nil)
          (fmt-row "~&  │ Status: VALID                       │")
          (fmt-row "~&  │ Status: ~2D ERROR(S)                  │"
                  (ct-list-length errors))))
    (fmt-row "~&  └────────────────────────────────────┘~%")))

(defun show-full-validation ()
  (let ((errors (funcall (ct-fn *ct-valid* "validate-portfolio") *portfolio*)))
    (if (eq errors coalton:Nil)
        (fmt-row "~&  ✓ Portfolio is fully valid.~%")
        (progn
          (fmt-row "~&  ✗ ~D validation errors:~%" (ct-list-length errors))
          (loop while (not (eq errors coalton:Nil))
                do (fmt-row "    - ~A~%" (cl:car errors))
                   (setf errors (cl:cdr errors)))))))

;;; ============================================================
;;; Load banner
;;; ============================================================

(format t "~&;;; Interrogation functions loaded.~%")
(format t "~&;;; (ls)             — table view of all content~%")
(format t "~&;;; (ls :projects)   — project table with slugs~%")
(format t "~&;;; (edit :project \"slug\") — surgical edit by slug~%")
(format t "~&;;; (new :project)   — create & append new entity~%")
(format t "~&;;; (rm :project \"slug\")  — remove by slug~%")
(format t "~&;;; (refine)         — parent menu with drill-down~%")
(format t "~&;;; (ship)           — validate + export~%")
