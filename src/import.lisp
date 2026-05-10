;;;; import.lisp — Bootstrap *portfolio* from Eleventy _data/ content
;;;;
;;;; Seeds the Coalton portfolio types from the existing Eleventy
;;;; data. This is the bridge between the type-safe CL world and
;;;; the live Eleventy content. After loading, you can refine and
;;;; ship without losing existing content.
;;;;
;;;; Usage:
;;;;   (portfolio:bootstrap)  — load the bundled seed data
;;;;   (portfolio:preview-all) — see what export-all would write

(in-package #:portfolio)

;;; ============================================================
;;; Quick constructors for seed data
;;; ============================================================

(defmacro with-portfolio-data (&body body)
  "Ensures Coalton packages are loaded before constructing data."
  `(progn ,@body))

(defun seed-person ()
  "Stüssy Senik profile from Eleventy _data/profile.js"
  (let ((taglines (coalton:make-list
                   (make-tagline "de" "Design Engineer · Creative")
                   (make-tagline "ja" "クリエイティブ・テクノロジスト"))))
    (make-person
     "Stüssy Senik"
     "Building at the intersection of engineering, science, creativity, filmmaking, and design. Focused on design experiences + details, things that perform, minimalism, simplicity."
     (coalton:Some
      "Long bio about design, engineering, and creativity.")
     "NYC / PRAGUE"
     coalton:True
     "itsmxzou@gmail.com"
     taglines)))

(defun seed-projects ()
  "Projects from Eleventy _data/works.js"
  (clist*
   (list
    (make-project "clean-writer" "www.clean-writer.mxzou.com"
                  "A deep dive into high-precision mechanical interfaces."
                  (coalton:make-list (make-tag "Next.js") (make-tag "React") (make-tag "mechanical"))
                  (coalton:make-list (make-link "visit" "https://www.clean-writer.mxzou.com"))
                  "thinking tools" 2026 1
                  (coalton:Some "oklch(0.65 0.20 140)") coalton:False)
    (make-project "breakdex" "breakdex"
                  "Observing the movement of high-viscosity fluids within a confined chamber."
                  (coalton:make-list (make-tag "iOS") (make-tag "SwiftUI") (make-tag "Flutter"))
                  (coalton:make-list (make-link "github" "#"))
                  "iOS/Swift UI/Flutter" 2026 1
                  (coalton:Some "oklch(0.60 0.15 285)") coalton:False)
    (make-project "find-your-answer" "find your answer"
                  "Structural housing for a multi-element optical array."
                  (coalton:make-list (make-tag "perplexica") (make-tag "search") (make-tag "clone"))
                  (coalton:make-list (make-link "demo" "#"))
                  "perplexica clone" 2026 1
                  (coalton:Some "oklch(0.70 0.12 200)") coalton:True)
    (make-project "mymindclone-web" "www.mymindclone-web.com"
                  "Testing the failure points of a custom mechanical keyboard switch."
                  (coalton:make-list (make-tag "personal") (make-tag "software") (make-tag "web"))
                  (coalton:make-list (make-link "visit" "https://www.mymindclone-web.com"))
                  "personal software" 2025 12
                  (coalton:Some "oklch(0.75 0.10 40)") coalton:False)
    (make-project "mit-ocw-reels" "MIT OCW reels"
                  "An exploration into passive cooling for high-performance computing."
                  (coalton:make-list (make-tag "mechanical") (make-tag "cooling") (make-tag "HPC"))
                  (coalton:make-list (make-link "watch" "#"))
                  "mechanical" 2025 11
                  (coalton:Some "oklch(0.65 0.25 15)") coalton:False))))

(defun seed-skills ()
  "Skills from Eleventy _data/skills.js"
  (clist*
   (list
    (make-skill "Motion Design" "design"
                (coalton:make-list (make-tag "motion")))
    (make-skill "UX & Product Design" "design"
                (coalton:make-list (make-tag "ux") (make-tag "product")))
    (make-skill "Visual Design" "design"
                (coalton:make-list (make-tag "visual")))
    (make-skill "Sound Design" "design"
                (coalton:make-list (make-tag "sound") (make-tag "audio")))
    (make-skill "3D & CGI" "design"
                (coalton:make-list (make-tag "3d") (make-tag "CGI")))
    (make-skill "Unreal Engine" "technology"
                (coalton:make-list (make-tag "unreal") (make-tag "gamedev")))
    (make-skill "Rhino & Grasshopper" "technology"
                (coalton:make-list (make-tag "rhino") (make-tag "grasshopper")))
    (make-skill "WebGPU & WASM" "technology"
                (coalton:make-list (make-tag "webgpu") (make-tag "wasm")))
    (make-skill "Three.js & WebGL" "technology"
                (coalton:make-list (make-tag "threejs") (make-tag "webgl")))
    (make-skill "Hardware & Sensors" "technology"
                (coalton:make-list (make-tag "hardware") (make-tag "sensors")))
    (make-skill "Creative Coding" "technology"
                (coalton:make-list (make-tag "creative-coding") (make-tag "art")))
    (make-skill "Art Direction" "art"
                (coalton:make-list (make-tag "art-direction")))
    (make-skill "Filmmaking" "art"
                (coalton:make-list (make-tag "film") (make-tag "video")))
    (make-skill "Lighting Design" "art"
                (coalton:make-list (make-tag "lighting")))
    (make-skill "Digital Fabrication" "art"
                (coalton:make-list (make-tag "fabrication") (make-tag "digital")))
    (make-skill "Illustration" "art"
                (coalton:make-list (make-tag "illustration") (make-tag "drawing"))))))

(defun seed-talks ()
  "Talks from Eleventy _data/talks.js"
  (clist*
   (list
    (make-talk "26' season" "talk" "2025-12" "Season announcement." (coalton:Some "#"))
    (make-talk "good things are coming (soon)" "interview" "2025-12"
               "Interview about upcoming projects." (coalton:Some "#")))))

(defun seed-sections ()
  "Empty sections — extend via refine."
  coalton:Nil)

;;; ============================================================
;;; Bootstrap
;;; ============================================================

(defun bootstrap ()
  "Seed *portfolio* from the bundled Eleventy _data/ content.
   This mirrors the real data in eleventy-portfolio-forever/_data/.
   After bootstrapping, use (refine) to edit and (ship) to export."
  (format t "~&Seeding portfolio from Eleventy _data/ ...")
  (let ((person (seed-person))
        (projects (seed-projects))
        (skills (seed-skills))
        (talks (seed-talks))
        (sections (seed-sections)))
    (setf *portfolio* (make-portfolio person projects skills sections talks))
    (format t " done.~%")
    (let ((errors (funcall (ct-fn *ct-valid* "validate-portfolio") *portfolio*)))
      (if (eq errors coalton:Nil)
          (progn
            (format t "~&  ✓ Portfolio is valid.~%")
            (show-status))
          (progn
            (format t "~&  ✗ ~D validation errors:~%" (ct-list-length errors))
            (loop while (not (eq errors coalton:Nil))
                  do (format t "    - ~A~%" (cl:car errors))
                     (setf errors (cl:cdr errors)))
            (format t "~&  Use (refine) to fix, then try again.~%"))))
    *portfolio*))

;;; ============================================================
;;; Load banner
;;; ============================================================

(format t "~&;;; Import/bootstrap functions loaded.~%")
(format t "~&;;; Try: (portfolio:bootstrap) — seed from eleventy data~%")
(format t "~&;;;   or: (portfolio:preview-all) — see export shape~%")
