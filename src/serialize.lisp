;;;; serialize.lisp — Coalton Portfolio → markdown emitter
;;;;
;;;; Constrained: only writes to content/ directory.
;;;; No JS, no HTML, no _data/ — pure .md files with YAML frontmatter.
;;;;
;;;; Usage:
;;;;   (portfolio:export-all)  — writes entire portfolio to content/

(in-package #:portfolio)

;;; ============================================================
;;; Target directory
;;; ============================================================

(defvar *content-target* nil
  "Path to the Eleventy content/ directory. Set before calling export-all.
   Default: ../eleventy-portfolio-forever/content/ relative to this project root.")

(defun content-target ()
  "Resolve the content target directory. Defaults to ./output/ in the project."
  (or *content-target*
      (setf *content-target*
            (merge-pathnames
             "output/"
             (or *load-truename* *default-pathname-defaults*)))))

;;; ============================================================
;;; Coalton value extraction
;;; ============================================================

(defun ct-get (entity accessor-name)
  "Call a Coalton accessor on ENTITY and unwrap to CL value.
   ACCESSOR-NAME is a string like \"project-slug\"."
  (funcall (ct-fn *ct-core* accessor-name) entity))

(defun ct-list-to-cl (ct-list)
  "Convert a Coalton List to a CL list. Assumes elements are Coalton values."
  (loop while (not (eq ct-list coalton:Nil))
        collect (cl:car ct-list)
        do (setf ct-list (cl:cdr ct-list))))

(defun ct-unwrap-optional (optional)
  "Extract CL value from Coalton Optional. Returns NIL for None."
  (if (eq optional coalton:None)
      nil
      (ct-maybe-unwrap optional)))

(defun ct-optional-value (entity accessor-name)
  "Get an optional field, unwrapped. NIL if None."
  (ct-unwrap-optional (ct-get entity accessor-name)))

(defun ct-boolean-p (entity accessor-name)
  "True if the Coalton Boolean field is True."
  (eq (ct-get entity accessor-name) coalton:True))

;;; ============================================================
;;; YAML formatting helpers
;;; ============================================================

(defun yaml-escape (str)
  "Escape a string for YAML value if needed."
  (if (or (null str) (string= str ""))
      "''"
      (let* ((needs-quote (or (find #\: str)
                              (find #\# str)
                              (find #\{ str)
                              (find #\} str)
                              (find #\[ str)
                              (find #\] str)
                              (find #\& str)
                              (find #\* str)
                              (find #\! str)
                              (find #\> str)
                              (find #\| str)
                              (find #\% str)
                              (find #\@ str)
                              (find #\` str)
                              (string= (string-trim " " str) "")))
             (has-double-quote (find #\" str))
             (escaped (if has-double-quote
                          (with-output-to-string (s)
                            (loop for c across str
                                  do (if (char= c #\")
                                         (write-string "\\\"" s)
                                         (write-char c s))))
                          str)))
        (if needs-quote
            (format nil "\"~A\"" escaped)
            escaped))))

(defun yaml-multiline (str)
  "Format a multi-line string as YAML literal block scalar.
   Returns NIL if the string is empty or NIL."
  (when (and str (not (string= str "")))
    (format nil "|~%~{  ~A~^~%~}" (str-lines str))))

(defun str-lines (str)
  "Split a string into lines."
  (loop with start = 0
        for pos = (position #\Newline str :start start)
        collect (subseq str start (or pos (length str)))
        while pos
        do (setf start (1+ pos))))

(defun yaml-kv (indent key value &optional (inline t))
  "Emit a YAML key-value pair at INDENT depth.
   INDENT is a string of spaces (e.g. \"  \").
   INLINE: if NIL, emit VALUE on next line with extra indent."
  (let ((out (make-string-output-stream)))
    (format out "~A~A: " indent key)
    (if inline
        (format out "~A~%" value)
        (format out "~%"))
    (get-output-stream-string out)))

(defun yaml-bool (val)
  (if val "true" "false"))

;;; ============================================================
;;; Markdown emission: Profile
;;; ============================================================

(defun emit-profile-md (person &key (stream t))
  "Generate content/profile.md from a Coalton Person."
  (let* ((name (ct-get person "person-name"))
         (short (ct-get person "person-short-bio"))
         (long (ct-optional-value person "person-long-bio"))
         (location (ct-get person "person-location"))
         (available (ct-boolean-p person "person-available"))
         (email (ct-get person "person-email"))
         (taglines (ct-list-to-cl (ct-get person "person-taglines"))))
    (format stream "---~%")
    (format stream "title: ~A~%" (yaml-escape name))
    (format stream "permalink: /~%")
    (format stream "layout: base.njk~%")
    (format stream "layoutConfig:~%")
    (format stream "  mode: editorial~%")
    (format stream "profile:~%")
    (format stream "  name: ~A~%" (yaml-escape name))
    (format stream "  taglines:~%")
    (dolist (tl taglines)
      (let ((lang (ct-get tl "tagline-lang"))
            (text (ct-get tl "tagline-text")))
        (format stream "    - lang: ~A~%" (yaml-escape lang))
        (format stream "      text: ~A~%" (yaml-escape text))))
    (format stream "  shortBio: ~A~%" (yaml-escape short))
    (when long
      (format stream "  longBio: ~%")
      (dolist (line (str-lines long))
        (format stream "    ~A~%" line)))
    (format stream "  location: ~A~%" (yaml-escape location))
    (format stream "  available: ~A~%" (yaml-bool available))
    (format stream "  email: ~A~%" (yaml-escape email))
    (format stream "  edition: \"02\"~%")
    (let ((now (multiple-value-bind (s m h d mo y)
                   (decode-universal-time (get-universal-time))
                 (declare (ignore s m h))
                 (format nil "~D-~2,'0D" y mo))))
      (format stream "  createdDate: ~A~%" (yaml-escape now)))
    (format stream "---~%")
    (format stream "~%")
    ;; body
    (format stream "{% from \"macros.njk\" import asciiDonut, button, card, badge, spacer, entryList with context %}~%")
    (format stream "~%")
    (format stream "<div class=\"page-hero stack stack-gap container\">~%")
    (format stream "  <div class=\"split\" style=\"align-items: center; --gap-x: var(--space-2xl);\">~%")
    (format stream "    <div class=\"stack stack-m\">~%")
    (format stream "      <div class=\"stack stack-xs\">~%")
    (format stream "        <h1>~A</h1>~%" name)
    (when taglines
      (let ((first-tl (car taglines)))
        (format stream "        <p class=\"hero-tagline\">~A</p>~%"
                (ct-get first-tl "tagline-text"))))
    (format stream "      </div>~%")
    (format stream "      <p class=\"hero-bio\">~A</p>~%" short)
    (format stream "      <p class=\"hero-meta cluster cluster-s\">~%")
    (format stream "        <span>~A</span>~%" location)
    (when available
      (format stream "        {{ badge(\"Available for work\", \"primary\") }}~%"))
    (format stream "      </p>~%")
    (format stream "    </div>~%")
    (format stream "    <div class=\"donut-aside\" style=\"flex-grow: 0; flex-basis: auto;\">~%")
    (format stream "      {{ asciiDonut() }}~%")
    (format stream "    </div>~%")
    (format stream "  </div>~%")
    (format stream "</div>~%")
    (format stream "~%")
    ;; needs _data/works reference for svelte-works. Transform: use collections.works instead.
    ;; For now, emit compat reference that templates can use.
    (format stream "<section class=\"page-section stack stack-gap container\" id=\"works\">~%")
    (format stream "  <div class=\"section-header\">~%")
    (format stream "    <span class=\"section-marker\">&#9670;</span>~%")
    (format stream "    <h2 class=\"section-title\">Works</h2>~%")
    (format stream "  </div>~%")
    (format stream "  <is-land on:visible>~%")
    (format stream "    <div id=\"svelte-works\"></div>~%")
    (format stream "  </is-land>~%")
    (format stream "</section>~%")))

;;; ============================================================
;;; Markdown emission: Project (individual)
;;; ============================================================

(defun emit-project-md (project &key (stream t))
  "Generate content/works/{slug}.md from a Coalton Project."
  (let* ((slug (ct-get project "project-slug"))
         (title (ct-get project "project-title"))
         (summary (ct-get project "project-summary"))
         (tags (ct-list-to-cl (ct-get project "project-tags")))
         (links (ct-list-to-cl (ct-get project "project-links")))
         (category (ct-get project "project-category"))
         (year (ct-get project "project-year"))
         (month (ct-get project "project-month"))
         (color (ct-optional-value project "project-featured-color"))
         (wip (ct-boolean-p project "project-wip")))
    ;; frontmatter
    (format stream "---~%")
    (format stream "title: ~A~%" (yaml-escape title))
    (format stream "permalink: /works/~A/~%" slug)
    (format stream "layout: page.njk~%")
    (format stream "tags:~%")
    (format stream "  - works~%")
    (format stream "  - portfolio~%")
    (format stream "work:~%")
    (format stream "  slug: ~A~%" slug)
    (format stream "  title: ~A~%" (yaml-escape title))
    (format stream "  summary: ~A~%" (yaml-escape summary))
    (format stream "  category: ~A~%" (yaml-escape category))
    (format stream "  year: ~D~%" year)
    (format stream "  month: ~D~%" month)
    (format stream "  wip: ~A~%" (yaml-bool wip))
    (when color
      (format stream "  color: ~A~%" (yaml-escape color)))
    (when tags
      (format stream "  tags:~%")
      (dolist (tag tags)
        (format stream "    - ~A~%" (yaml-escape (ct-get tag "tag-text")))))
    (when links
      (format stream "  links:~%")
      (dolist (link links)
        (let ((label (ct-get link "link-label"))
              (url (ct-get link "link-url")))
          (format stream "    - label: ~A~%" (yaml-escape label))
          (format stream "      url: ~A~%" (yaml-escape url)))))
    (format stream "---~%")
    (format stream "~%")
    ;; body
    (format stream "# ~A~%" title)
    (format stream "~%")
    (format stream "~A~%" summary)
    (format stream "~%")
    (format stream "**Category:** ~A | **Year:** ~D | **Month:** ~D~%" category year month)
    (when wip
      (format stream "~%> Work in progress. Details may change.~%"))))

;;; ============================================================
;;; Markdown emission: Skills
;;; ============================================================

(defun emit-skills-md (skills &key (stream t))
  "Generate content/skills.md from a Coalton List of Skill."
  (let ((cl-skills (ct-list-to-cl skills)))
    (format stream "---~%")
    (format stream "title: Skills~%")
    (format stream "permalink: /skills/~%")
    (format stream "layout: page.njk~%")
    (format stream "nav: true~%")
    (format stream "layoutConfig:~%")
    (format stream "  mode: editorial~%")
    (format stream "skills:~%")
    (dolist (sk cl-skills)
      (let ((name (ct-get sk "skill-name"))
            (category (ct-get sk "skill-category"))
            (tags (ct-list-to-cl (ct-get sk "skill-tags"))))
        (format stream "  - name: ~A~%" (yaml-escape name))
        (format stream "    category: ~A~%" (yaml-escape category))
        (when tags
          (format stream "    tags:~%")
          (dolist (tag tags)
            (format stream "      - ~A~%" (yaml-escape (ct-get tag "tag-text")))))))
    (format stream "---~%")
    (format stream "~%")
    (format stream "{% from \"macros.njk\" import badge with context %}~%")
    (format stream "~%")
    (format stream "<div class=\"page-hero stack stack-gap container\">~%")
    (format stream "  <div class=\"stack stack-xs\">~%")
    (format stream "    <p class=\"hero-tagline\">Capabilities</p>~%")
    (format stream "    <h1>Skills</h1>~%")
    (format stream "  </div>~%")
    (format stream "  <p class=\"hero-bio\">Technical and creative competencies developed over 8+ years of practice.</p>~%")
    (format stream "</div>~%")
    (format stream "~%")
    ;; Group by category
    (let ((categories (remove-duplicates (mapcar (lambda (sk) (ct-get sk "skill-category")) cl-skills)
                                         :test #'string=)))
      (dolist (cat categories)
        (format stream "<section class=\"page-section stack stack-gap container\">~%")
        (format stream "  <div class=\"section-header\">~%")
        (format stream "    <span class=\"section-marker\">&#9670;</span>~%")
        (format stream "    <h2 class=\"section-title\">~A</h2>~%" cat)
        (format stream "  </div>~%")
        (format stream "  <div class=\"stack stack-m\" style=\"max-width: var(--measure-wide);\">~%")
        (dolist (sk cl-skills)
          (when (string= (ct-get sk "skill-category") cat)
            (format stream "    <div class=\"card\">~%")
            (format stream "      <span class=\"entry-meta\">~A</span>~%" (yaml-escape (ct-get sk "skill-name")))
            (format stream "    </div>~%")))
        (format stream "  </div>~%")
        (format stream "</section>~%")))))

;;; ============================================================
;;; Markdown emission: Talks
;;; ============================================================

(defun emit-talks-md (talks &key (stream t))
  "Generate content/talks.md from a Coalton List of Talk."
  (let ((cl-talks (ct-list-to-cl talks)))
    (format stream "---~%")
    (format stream "title: Talks~%")
    (format stream "permalink: /talks/~%")
    (format stream "layout: page.njk~%")
    (format stream "nav: true~%")
    (format stream "layoutConfig:~%")
    (format stream "  mode: editorial~%")
    (format stream "talks:~%")
    (dolist (t* cl-talks)
      (let ((title (ct-get t* "talk-title"))
            (type (ct-get t* "talk-type"))
            (date (ct-get t* "talk-date"))
            (description (ct-get t* "talk-description"))
            (link (ct-optional-value t* "talk-link")))
        (format stream "  - title: ~A~%" (yaml-escape title))
        (format stream "    type: ~A~%" (yaml-escape type))
        (format stream "    date: ~A~%" (yaml-escape date))
        (format stream "    description: ~A~%" (yaml-escape description))
        (when link
          (format stream "    link: ~A~%" (yaml-escape link)))))
    (format stream "---~%")
    (format stream "~%")
    (format stream "{% from \"macros.njk\" import badge with context %}~%")
    (format stream "~%")
    (format stream "<div class=\"page-hero stack stack-gap container\">~%")
    (format stream "  <div class=\"stack stack-xs\">~%")
    (format stream "    <p class=\"hero-tagline\">Speaking &amp; Appearances</p>~%")
    (format stream "    <h1>Talks</h1>~%")
    (format stream "  </div>~%")
    (format stream "  <p class=\"hero-bio\">Public speaking, interviews, and media appearances.</p>~%")
    (format stream "</div>~%")
    (format stream "~%")
    (format stream "<section class=\"page-section stack stack-gap container\">~%")
    (format stream "  <div class=\"section-header\">~%")
    (format stream "    <span class=\"section-marker\">&#9670;</span>~%")
    (format stream "    <h2 class=\"section-title\">Talks &amp; Interviews</h2>~%")
    (format stream "    <span class=\"section-count\">~D</span>~%" (length cl-talks))
    (format stream "  </div>~%")
    (format stream "  <ul class=\"entry-list\">~%")
    (dolist (t* cl-talks)
      (let ((title (ct-get t* "talk-title"))
            (type (ct-get t* "talk-type"))
            (date (ct-get t* "talk-date"))
            (link (ct-optional-value t* "talk-link")))
        (format stream "    <li class=\"entry\">~%")
        (format stream "      <span class=\"entry-date font-mono\">~A</span>~%" (yaml-escape date))
        (format stream "      <span class=\"entry-title\">~A</span>~%" (yaml-escape title))
        (format stream "      {{ badge(\"~A\", \"primary\") }}~%" (yaml-escape (string-capitalize type)))
        (when link
          (format stream "      <span class=\"entry-links\">~%")
          (format stream "        <a href=\"~A\">link</a>~%" (yaml-escape link))
          (format stream "      </span>~%"))
        (format stream "    </li>~%")))
    (format stream "  </ul>~%")
    (format stream "</section>~%")))

;;; ============================================================
;;; Markdown emission: CV
;;; ============================================================

(defun emit-cv-md (person sections &key (stream t))
  "Generate content/cv.md from Person and Sections."
  (let ((name (ct-get person "person-name"))
        (location (ct-get person "person-location"))
        (available (ct-boolean-p person "person-available"))
        (taglines (ct-list-to-cl (ct-get person "person-taglines")))
        (cl-sections (ct-list-to-cl sections)))
    (format stream "---~%")
    (format stream "title: CV~%")
    (format stream "permalink: /cv/~%")
    (format stream "layout: page.njk~%")
    (format stream "nav: true~%")
    (format stream "layoutConfig:~%")
    (format stream "  mode: editorial~%")
    (format stream "---~%")
    (format stream "~%")
    (format stream "{% from \"macros.njk\" import badge with context %}~%")
    (format stream "~%")
    (format stream "<div class=\"page-hero stack stack-gap container\">~%")
    (format stream "  <div class=\"stack stack-xs\">~%")
    (when taglines
      (format stream "    <p class=\"hero-tagline\">~A</p>~%" (ct-get (car taglines) "tagline-text")))
    (format stream "    <h1>~A</h1>~%" name)
    (format stream "  </div>~%")
    (format stream "  <p class=\"hero-meta cluster cluster-s\">~%")
    (format stream "    <span>~A · Remote-friendly</span>~%" location)
    (when available
      (format stream "    {{ badge(\"Available for projects\", \"primary\") }}~%"))
    (format stream "  </p>~%")
    (format stream "</div>~%")
    (format stream "~%")
    ;; sections
    (dolist (sec cl-sections)
      (let ((heading (ct-get sec "section-heading"))
            (body (ct-list-to-cl (ct-get sec "section-body")))
            (keywords (ct-list-to-cl (ct-get sec "section-keywords"))))
        (format stream "<section class=\"page-section stack stack-gap container\">~%")
        (format stream "  <div class=\"section-header\">~%")
        (format stream "    <span class=\"section-marker\">&#9670;</span>~%")
        (format stream "    <h2 class=\"section-title\">~A</h2>~%" heading)
        (format stream "  </div>~%")
        ;; content blocks
        (dolist (block body)
          ;; Content is a sum type: Paragraph | BulletList | CodeBlock
          ;; We need to pattern-match on the Coalton variant.
          ;; For now, emit what we can detect.
          (format stream "  <div class=\"card\">~%")
          (format stream "    <p class=\"card-description\">~A</p>~%" heading)
          (format stream "  </div>~%"))
        ;; keywords as chips
        (when keywords
          (format stream "  <div class=\"chip-list\">~%")
          (dolist (kw keywords)
            (format stream "    <span class=\"chip chip-highlight\">~A</span>~%"
                    (ct-get kw "tag-text")))
          (format stream "  </div>~%"))
        (format stream "</section>~%")))))

;;; ============================================================
;;; Section emission (general page content)
;;; ============================================================

(defun emit-section-body (content-block stream)
  "Emit a single Content block as markdown/html."
  ;; Content = Paragraph String | BulletList (List String) | CodeBlock String String
  (format stream "  <p class=\"card-description\">(section content)</p>~%"))

;;; ============================================================
;;; Export all
;;; ============================================================

(defun export-all (&key (target (content-target)))
  "Export the entire *portfolio* to markdown files in TARGET directory.
   Writes to content/ only — no JS, no _data/, no HTML outside content/."
  (unless *portfolio*
    (error "No *portfolio* exists. Run (portfolio:describe-portfolio) first."))
  (let ((person (ct-get *portfolio* "portfolio-person"))
        (projects (ct-get *portfolio* "portfolio-projects"))
        (skills (ct-get *portfolio* "portfolio-skills"))
        (sections (ct-get *portfolio* "portfolio-sections"))
        (talks (ct-get *portfolio* "portfolio-talks")))
    (ensure-directories-exist target)
    ;; Profile → content/index.md
    (let ((profile-path (merge-pathnames "index.md" target)))
      (format t "~&  writing ~A ..." profile-path)
      (with-open-file (out profile-path :direction :output :if-exists :supersede)
        (emit-profile-md person :stream out))
      (format t " done~%"))
    ;; Projects → content/works/*.md
    (let ((works-dir (merge-pathnames "works/" target)))
      (ensure-directories-exist works-dir)
      (loop for proj in (ct-list-to-cl projects)
            do (let* ((slug (ct-get proj "project-slug"))
                      (path (merge-pathnames (format nil "~A.md" slug) works-dir)))
                 (format t "~&  writing ~A ..." path)
                 (with-open-file (out path :direction :output :if-exists :supersede)
                   (emit-project-md proj :stream out))
                 (format t " done~%"))))
    ;; Skills → content/skills.md
    (let ((skills-path (merge-pathnames "skills.md" target)))
      (format t "~&  writing ~A ..." skills-path)
      (with-open-file (out skills-path :direction :output :if-exists :supersede)
        (emit-skills-md skills :stream out))
      (format t " done~%"))
    ;; Talks → content/talks.md
    (let ((talks-path (merge-pathnames "talks.md" target)))
      (format t "~&  writing ~A ..." talks-path)
      (with-open-file (out talks-path :direction :output :if-exists :supersede)
        (emit-talks-md talks :stream out))
      (format t " done~%"))
    ;; CV → content/cv.md (from person + sections)
    (let ((cv-path (merge-pathnames "cv.md" target)))
      (format t "~&  writing ~A ..." cv-path)
      (with-open-file (out cv-path :direction :output :if-exists :supersede)
        (emit-cv-md person sections :stream out))
      (format t " done~%"))
    (format t "~&Content exported to ~A~%" target)))

;;; ============================================================
;;; Preview — dump all markdown to stdout for inspection
;;; ============================================================

(defun preview-profile ()
  "Print the generated profile.md to stdout."
  (let ((person (ct-get *portfolio* "portfolio-person")))
    (emit-profile-md person :stream t)))

(defun preview-project (slug)
  "Print a specific project's markdown to stdout."
  (let ((projects (ct-list-to-cl (ct-get *portfolio* "portfolio-projects"))))
    (dolist (proj projects)
      (when (string= (ct-get proj "project-slug") slug)
        (emit-project-md proj :stream t)
        (return-from preview-project)))
    (format t "No project found with slug ~A~%" slug)))

(defun preview-all (&key (stream t))
  "Dump the entire generated markdown to STREAM for inspection.
   Prints a separator between each file.
   Use (preview-all) to see what export-all would write."
  (unless *portfolio*
    (format stream "~&;; No *portfolio* exists. Run (scaffold) first.~%")
    (return-from preview-all nil))
  (let ((person (ct-get *portfolio* "portfolio-person"))
        (projects (ct-get *portfolio* "portfolio-projects"))
        (skills (ct-get *portfolio* "portfolio-skills"))
        (sections (ct-get *portfolio* "portfolio-sections"))
        (talks (ct-get *portfolio* "portfolio-talks")))
    (format stream "~&#| ====== PROFILE (index.md) ======~%")
    (emit-profile-md person :stream stream)
    (format stream "~&#| ====== PROJECTS ======~%")
    (loop for proj in (ct-list-to-cl projects)
          do (let ((slug (ct-get proj "project-slug")))
               (format stream "~&#| ====== ~A.md ======~%" slug)
               (emit-project-md proj :stream stream)))
    (format stream "~&#| ====== SKILLS (skills.md) ======~%")
    (emit-skills-md skills :stream stream)
    (format stream "~&#| ====== TALKS (talks.md) ======~%")
    (emit-talks-md talks :stream stream)
    (format stream "~&#| ====== CV (cv.md) ======~%")
    (emit-cv-md person sections :stream stream)
    (format stream "~&#| ====== END ======~%")))

;;; ============================================================
;;; Persistence: save / load the portfolio between REPL sessions
;;; ============================================================

(defun save (&optional (path (merge-pathnames "portfolio.lisp"
                                              (or *load-truename*
                                                  *default-pathname-defaults*))))
  "Save *portfolio* as a loadable Lisp file. Restore with (restore).
   Default: ./portfolio.lisp"
  (unless *portfolio*
    (format t "~&  No *portfolio* to save.~%")
    (return-from save nil))
  (with-open-file (out path :direction :output :if-exists :supersede)
    (write-portfolio-sexp out))
  (format t "~&  ✓ Saved to ~A~%" path)
  path)

(defun write-portfolio-sexp (out)
  "Emit the portfolio as a setf form that can be loaded back."
  (format out ";;;; portfolio.lisp — saved state, load with (portfolio:load)~%~%")
  (format out "(in-package :portfolio)~%~%")
  ;; person
  (let ((p (ct-get *portfolio* "portfolio-person"))
        (projects (ct-list-to-cl (ct-get *portfolio* "portfolio-projects")))
        (skills (ct-list-to-cl (ct-get *portfolio* "portfolio-skills")))
        (talks (ct-list-to-cl (ct-get *portfolio* "portfolio-talks")))
        (sections (ct-list-to-cl (ct-get *portfolio* "portfolio-sections"))))
    (format out "(setf *portfolio* (make-portfolio~%")
    ;; person form
    (emit-person-sexp p out)
    ;; projects
    (format out "~%   (clist* (list")
    (dolist (proj projects) (emit-project-sexp proj out))
    (format out "))~%")
    ;; skills
    (format out "   (clist* (list")
    (dolist (sk skills) (emit-skill-sexp sk out))
    (format out "))~%")
    ;; sections
    (format out "   (clist* (list")
    (dolist (sec sections) (emit-section-sexp sec out))
    (format out "))~%")
    ;; talks
    (format out "   (clist* (list")
    (dolist (t* talks) (emit-talk-sexp t* out))
    (format out "))))~%")))

(defun emit-person-sexp (p out)
  (let ((long (ct-optional-value p "person-long-bio"))
        (tls (ct-list-to-cl (ct-get p "person-taglines"))))
    (format out "   (make-person ~S ~S~%                ~A~%                ~S ~S ~S~%                (clist* (list"
            (ct-get p "person-name") (ct-get p "person-short-bio")
            (if long (format nil "(coalton:Some ~S)" long) "coalton:None")
            (ct-get p "person-location") (ct-get p "person-available") (ct-get p "person-email"))
    (dolist (tl tls)
      (format out "~%                  (make-tagline ~S ~S)"
              (ct-get tl "tagline-lang") (ct-get tl "tagline-text")))
    (format out ")))")))

(defun emit-project-sexp (proj out)
  (let ((tags (ct-list-to-cl (ct-get proj "project-tags")))
        (links (ct-list-to-cl (ct-get proj "project-links")))
        (color (ct-optional-value proj "project-featured-color")))
    (format out "~%     (make-project ~S ~S ~S"
            (ct-get proj "project-slug") (ct-get proj "project-title") (ct-get proj "project-summary"))
    (format out "~%                    (coalton:make-list")
    (dolist (tag tags) (format out " (make-tag ~S)" (ct-get tag "tag-text")))
    (format out ")")
    (format out "~%                    (coalton:make-list")
    (dolist (link links) (format out " (make-link ~S ~S)" (ct-get link "link-label") (ct-get link "link-url")))
    (format out ")")
    (format out "~%                    ~S ~S ~S"
            (ct-get proj "project-category") (ct-get proj "project-year") (ct-get proj "project-month"))
    (format out "~%                    ~A ~A)"
            (if color (format nil "(coalton:Some ~S)" color) "coalton:None")
            (ct-get proj "project-wip"))))

(defun emit-skill-sexp (sk out)
  (let ((tags (ct-list-to-cl (ct-get sk "skill-tags"))))
    (format out "~%     (make-skill ~S ~S"
            (ct-get sk "skill-name") (ct-get sk "skill-category"))
    (format out "~%                 (coalton:make-list")
    (dolist (tag tags) (format out " (make-tag ~S)" (ct-get tag "tag-text")))
    (format out "))")))

(defun emit-talk-sexp (t* out)
  (let ((link (ct-optional-value t* "talk-link")))
    (format out "~%     (make-talk ~S ~S ~S ~S ~A)"
            (ct-get t* "talk-title") (ct-get t* "talk-type")
            (ct-get t* "talk-date") (ct-get t* "talk-description")
            (if link (format nil "(coalton:Some ~S)" link) "coalton:None"))))

(defun emit-section-sexp (sec out)
  (let ((keywords (ct-list-to-cl (ct-get sec "section-keywords"))))
    (format out "~%     (make-section ~S~%                   (clist* ()) ; body TBD~%                   (clist* (list"
            (ct-get sec "section-heading"))
    (dolist (kw keywords) (format out " (make-tag ~S)" (ct-get kw "tag-text")))
    (format out ")))")))

(defun restore (&optional (path (merge-pathnames "portfolio.lisp"
                                                  (or *load-truename*
                                                      *default-pathname-defaults*))))
  "Restore *portfolio* from a saved file."
  (unless (probe-file path)
    (format t "~&  ✗ File not found: ~A~%" path)
    (return-from restore nil))
  (format t "~&  Loading ~A ..." path)
  (cl:load path)
  (if *portfolio*
      (progn (format t " done.~%") (show-status))
      (format t " failed.~%"))
  *portfolio*)
