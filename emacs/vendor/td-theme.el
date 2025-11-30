;;; ocaml-forum-tonsky-theme.el --- Dark OCaml-forum-like theme, Tonsky style -*- lexical-binding: t; -*-

(deftheme td
  "Minimal dark theme inspired by discuss.ocaml.org code blocks, following Tonsky's syntax-highlighting principles.")

(let* ((class '((class color) (min-colors 89)))

       ;; Core palette (from screenshot)
       (bg         "#292c34")  ;; main background
       (bg-alt     "#252830")  ;; slightly darker/alternative bg
       (bg-accent  "#512222")  ;; burgundy panel / accent
       (fg         "#d1d5e0")  ;; main foreground (slightly lighter than screenshot)
       (fg-soft    "#a4a9b7")  ;; secondary text
       (fg-faint   "#7b818c")  ;; comments, de-emphasised text
       (border     "#1e2027")

       ;; Accents (averaged from screenshot)
       (accent-1   "#6d4d7e")  ;; purple – keywords / control
       (accent-2   "#446d92")  ;; blue-cyan – types / constants
       (accent-3   "#8d7a59")  ;; yellow-brown – strings
       (accent-good "#607756") ;; soft green – success / added

       ;; UI status colors
       (error      "#c65a5a")
       (warning    "#d19a66")
       (info       "#5ca0d3"))

  (custom-theme-set-faces
   'td

   ;; Basic faces
   `(default ((,class (:background ,bg :foreground ,fg))))
   `(cursor  ((,class (:background ,accent-3))))
   `(fringe  ((,class (:background ,bg))))
   `(region  ((,class (:background ,bg-alt :foreground ,fg))))
   `(highlight ((,class (:background ,bg-alt))))
   `(hl-line ((,class (:background ,bg-alt))))
   `(vertical-border ((,class (:foreground ,border))))
   `(shadow  ((,class (:foreground ,fg-faint))))
   `(link    ((,class (:foreground ,accent-2 :underline t))))
   `(link-visited ((,class (:foreground ,accent-2 :underline t))))

   ;; Line numbers
   `(line-number ((,class (:foreground ,fg-faint :background ,bg))))
   `(line-number-current-line ((,class (:foreground ,fg-soft :background ,bg))))

   ;; Minibuffer / prompts
   `(minibuffer-prompt ((,class (:foreground ,accent-2 :weight semi-bold))))
   `(prompt ((,class (:foreground ,accent-2 :weight semi-bold))))

   ;; Mode line – simple, not too shouty
   `(mode-line
     ((,class (:background ,bg-alt :foreground ,fg-soft
                           :box (:line-width 1 :color ,border)))))
   `(mode-line-inactive
     ((,class (:background ,bg :foreground ,fg-faint
                           :box (:line-width 1 :color ,border)))))

   ;; Tonsky-inspired code faces:
   ;; - Few colors, reduced visual noise
   ;; - Comments faint & italic
   ;; - Strings get their own warm color
   ;; - Keywords purple but not screaming
   ;; - Types/constants bluish
   ;; - Names mostly default fg with mild emphasis

   ;; Comments
   `(font-lock-comment-face
     ((,class (:foreground ,fg-faint :slant italic))))
   `(font-lock-comment-delimiter-face
     ((,class (:inherit font-lock-comment-face))))
   `(font-lock-doc-face
     ((,class (:foreground ,fg-faint :slant italic))))

   ;; Strings & chars (accent-3)
   `(font-lock-string-face
     ((,class (:foreground ,accent-3))))
   `(font-lock-doc-string-face
     ((,class (:inherit font-lock-string-face))))

   ;; Keywords / control (accent-1)
   `(font-lock-keyword-face
     ((,class (:foreground ,accent-1 :weight regular))))
   `(font-lock-builtin-face
     ((,class (:foreground ,accent-1))))

   ;; Types / modules / constants (accent-2)
   `(font-lock-type-face
     ((,class (:foreground ,accent-2 :weight semi-bold))))
   `(font-lock-constant-face
     ((,class (:foreground ,accent-2))))
   `(font-lock-preprocessor-face
     ((,class (:foreground ,accent-2 :weight normal))))

   ;; Function & variable names:
   ;; keep close to default to avoid rainbow; use weight/italic only.
   `(font-lock-function-name-face
     ((,class (:foreground ,fg :weight semi-bold))))
   `(font-lock-variable-name-face
     ((,class (:foreground ,fg))))
   `(font-lock-warning-face
     ((,class (:foreground ,warning :weight bold))))

   ;; Numbers – treat as constants (accent-2)
   `(font-lock-number-face
     ((,class (:foreground ,accent-2))))
   ;; Some modes use this:
   `(font-lock-negation-char-face
     ((,class (:foreground ,accent-1))))

   ;; Show-paren
   `(show-paren-match
     ((,class (:background ,bg-accent :foreground ,fg :weight bold))))
   `(show-paren-mismatch
     ((,class (:background ,error :foreground ,bg :weight bold))))

   ;; Search
   `(isearch ((,class (:background ,accent-2 :foreground ,bg :weight bold))))
   `(lazy-highlight
     ((,class (:background ,bg-accent :foreground ,fg-soft))))

   ;; Compilation / flycheck / etc.
   `(success ((,class (:foreground ,accent-good :weight semi-bold))))
   `(warning ((,class (:foreground ,warning :weight semi-bold))))
   `(error   ((,class (:foreground ,error   :weight semi-bold))))

   ;; Org-mode (very minimal)
   `(org-level-1 ((,class (:foreground ,accent-2 :weight bold :height 1.15))))
   `(org-level-2 ((,class (:foreground ,accent-1 :weight semi-bold :height 1.10))))
   `(org-level-3 ((,class (:foreground ,fg-soft :weight semi-bold))))
   `(org-code    ((,class (:foreground ,accent-3))))
   `(org-verbatim((,class (:foreground ,accent-3))))
   `(org-block   ((,class (:background ,bg-alt :extend t))))
   `(org-block-begin-line ((,class (:foreground ,fg-faint :background ,bg-alt))))
   `(org-block-end-line   ((,class (:foreground ,fg-faint :background ,bg-alt))))

   ;; Tree-sitter / treesit faces (for newer Emacs)
   ;; Map them onto our small palette.
   `(treesit-face-comment               ((,class (:inherit font-lock-comment-face))))
   `(treesit-face-string                ((,class (:inherit font-lock-string-face))))
   `(treesit-face-keyword               ((,class (:inherit font-lock-keyword-face))))
   `(treesit-face-builtin               ((,class (:inherit font-lock-builtin-face))))
   `(treesit-face-type                  ((,class (:inherit font-lock-type-face))))
   `(treesit-face-constant              ((,class (:inherit font-lock-constant-face))))
   `(treesit-face-function-call         ((,class (:inherit font-lock-function-name-face))))
   `(treesit-face-function              ((,class (:inherit font-lock-function-name-face))))
   `(treesit-face-variable              ((,class (:inherit font-lock-variable-name-face))))
   `(treesit-face-number                ((,class (:inherit font-lock-number-face))))
   ))

(provide-theme 'td)
