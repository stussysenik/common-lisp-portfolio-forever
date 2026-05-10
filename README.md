# common-lisp-portfolio-forever

A type-driven portfolio content engine written in Common Lisp + [Coalton](https://github.com/coalton-lang/coalton). All content lives in typed structures; the REPL is the CMS. Exports markdown for [Eleventy](https://www.11ty.dev/).

```
┌────┬──────────────────────┬──────────────────────────────────────┬────┬──────┬────────────────────┐
│  # │ SLUG                  │ TITLE                                │ YR │ WIP  │ CATEGORY           │
├────┼──────────────────────┼──────────────────────────────────────┼────┼──────┼────────────────────┤
│  1 │ clean-writer         │ www.clean-writer.mxzou.com           │ 2026 │      │ thinking tools     │
│  2 │ breakdex             │ breakdex                             │ 2026 │      │ iOS/Swift UI/Fl... │
│  3 │ find-your-answer     │ find your answer                     │ 2026 │ ✓    │ perplexica clone   │
│  4 │ mymindclone-web      │ www.mymindclone-web.com              │ 2025 │      │ personal software  │
│  5 │ mit-ocw-reels        │ MIT OCW reels                        │ 2025 │      │ mechanical         │
└────┴──────────────────────┴──────────────────────────────────────┴────┴──────┴────────────────────┘
  5 projects. Select by slug: (edit :project "slug")
```

## Principles

- **Type-driven**. Coalton types define the content model. Empty strings are impossible — validation happens at the type level.
- **Slug = filename**. `edit :project "breakdex"` → `output/works/breakdex.md`. Zero indirection.
- **REPL is the CMS**. Tables, menus, surgical edits — all from the Lisp prompt.
- **Save/restore**. Portfolio persists as a `.lisp` file between sessions. No database.

## Quick Start

Requires [SBCL](http://www.sbcl.org/) and [Quicklisp](https://www.quicklisp.org/beta/). Coalton is pulled automatically from Quicklisp.

```bash
sbcl --eval "(ql:quickload :common-lisp-portfolio-forever)" \
     --eval "(in-package :portfolio)" \
     --eval "(bootstrap)"
```

This loads 5 projects, 16 skills, and 2 talks from the bundled Eleventy seed data.

## Three Modes

### `scaffold` — Create (rarely used)

Build fresh content from scratch or create individual entities.

```lisp
(scaffold)             ; full interactive portfolio builder
(scaffold :project)    ; build a single project
(scaffold :skill)      ; build a single skill
```

### `refine` — Maintain (daily driver)

Parent menu with drill-down into each category.

```lisp
(refine)               ; menu: [p]erson [w]orks [s]kills [t]alks [c]v
(refine :projects)      ; jump straight to projects table
```

Inside a category:
- `e <slug>` — edit (re-interrogate with existing values as defaults)
- `n` — create new entity, auto-slug from title
- `r <slug>` — remove
- `.` — back

### `ship` — Export (the finish line)

```lisp
(ship)                 ; dry-run: validate only
(ship :dry-run nil)    ; write ./output/*.md
(ship :dry-run nil :target "../eleventy-portfolio-forever/content/")  ; custom target
```

## Expression-Based REPL Commands

Surgical operations by slug — no menus required:

| Command | Does |
|---------|------|
| `(ls)` | Summary + all tables |
| `(ls :projects)` | Project table (slug, title, year, wip, category) |
| `(ls :skills)` | Skill table (name, category) |
| `(ls :talks)` | Talk table (title, type, date) |
| `(edit :project "breakdex")` | Re-interrogate project with defaults |
| `(edit :skill "Motion Design")` | Edit skill by name |
| `(edit :talk "26' season")` | Edit talk by title |
| `(edit :person)` | Edit profile |
| `(new :project)` | Create project, auto-slug |
| `(new :skill)` | Create skill |
| `(new :talk)` | Create talk |
| `(rm :project "breakdex")` | Remove by slug |
| `(rm :skill "Sound Design")` | Remove by name |
| `(preview-all)` | Dump all generated markdown to stdout |
| `(save)` | Persist to `./portfolio.lisp` |
| `(restore)` | Restore from `./portfolio.lisp` |
| `(ship)` | Validate (dry-run) |
| `(ship :dry-run nil)` | Export to `./output/` |

## Persistence

The portfolio lives in memory as `*portfolio*`. Save between REPL sessions:

```lisp
(save)                              ; → ./portfolio.lisp
(save "my-state.lisp")             ; custom path

;; Next session:
(restore)                           ; ← ./portfolio.lisp
(restore "my-state.lisp")          ; custom path
```

The saved file is loadable Lisp — you can inspect it, version-control it, or edit it directly.

## Content Model

Typed entities in Coalton (`src/types/core.coal`):

```
Person   → name, short-bio, long-bio(opt), location, available, email, taglines
Project  → slug, title, summary, tags, links, category, year, month, color(opt), wip
Skill    → name, category, tags
Talk     → title, type, date, description, link(opt)
Section  → heading, body(content blocks), keywords
```

Validation functions (`src/types/validation.coal`) return `(List String)` — empty list means valid.

## Export Format

Generated markdown files in `output/`:

```
output/
├── index.md            # profile page
├── skills.md           # skills page
├── talks.md            # talks page
├── cv.md               # CV page
└── works/
    ├── clean-writer.md
    ├── breakdex.md
    ├── find-your-answer.md
    ├── mymindclone-web.md
    └── mit-ocw-reels.md
```

Each file has YAML frontmatter + Nunjucks template body matching the Eleventy portfolio structure.

## Project Structure

```
src/
├── packages.lisp        # Package definitions and exports
├── types/
│   ├── core.coal        # Coalton type definitions
│   └── validation.coal  # Type-safe validation
├── portfolio.lisp       # Coalton constructor wrappers, *portfolio* state
├── interrogate.lisp     # REPL interrogation, TUI tables, scaffold/refine/ship
├── import.lisp          # Bootstrap from Eleventy _data/ seed
└── serialize.lisp       # Markdown export, preview-all, save/restore
tests/
└── tests.lisp           # Coalton validation tests (9 passing)
```

## Tests

```lisp
(asdf:test-system :common-lisp-portfolio-forever)
```

## License

MIT
