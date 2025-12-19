;;; td-theme.el --- Dark OCaml-forum-like theme, Tonsky style -*- lexical-binding: t; -*-

(deftheme td
  "Minimal dark theme inspired by discuss.ocaml.org code blocks, following Tonsky's syntax-highlighting principles.")

(let* ((class '((class color) (min-colors 89)))

       ;; Background: forum uses rgba(0,0,0,0.25) over a dark page.
       ;; We approximate with a solid dark gray.
       (bg         "#191919")
       (bg-alt     "#191919")
       (border     "#1e2027")

       ;; Base foreground: use punctuation color as main fg
       ;; and keep a slightly lighter variant for emphasis.
       (fg         "#cccccc")  ;; --hljs-punctuation
       (fg-soft    "#e0e0e0")
       (fg-faint   "#848484")  ;; --hljs-comment ≈ rgb(132.6,132.6,132.6)

       ;; Palette from your CSS variables
       (kw         "#88aece")  ;; --hljs-attr / --hljs-keyword
       (attr       "#c59bc1")  ;; --hljs-attribute
       (add        "#76c490")  ;; --hljs-addition
       (del        "#de7176")  ;; --hljs-deletion
       (title      "#f08d49")  ;; --hljs-title / --hljs-name
       (string     "#b5bd68")  ;; --hljs-string / --hljs-symbol / --hljs-variable

       ;; Convenience aliases
       (success    add)
       (warning    title)
       (error      del))

  (custom-theme-set-faces
   'td

   ;; Basic UI
   `(default ((,class (:background ,bg :foreground ,fg))))
   `(cursor  ((,class (:background ,title))))
   `(fringe  ((,class (:background ,bg))))
   `(region  ((,class (:background "#3a3a3a"))))
   `(highlight ((,class (:background "#3a3a3a"))))
   `(hl-line ((,class (:background ,bg-alt))))
   `(vertical-border ((,class (:foreground ,border))))
   `(shadow  ((,class (:foreground ,fg-faint))))
   `(link    ((,class (:foreground ,kw :underline t))))
   `(link-visited ((,class (:foreground ,kw :underline t))))

   ;; Line numbers
   `(line-number ((,class (:foreground ,fg-faint :background ,bg))))
   `(line-number-current-line ((,class (:foreground ,fg :background ,bg))))

   ;; Minibuffer / prompts
   `(minibuffer-prompt ((,class (:foreground ,kw :weight semi-bold))))
   `(prompt ((,class (:foreground ,kw :weight semi-bold))))

   ;; Mode line
   `(mode-line
     ((,class (:background ,bg-alt :foreground ,fg-soft
                           :box (:line-width 1 :color ,border)))))
   `(mode-line-inactive
     ((,class (:background ,bg :foreground ,fg-faint
                           :box (:line-width 1 :color ,border)))))

   ;; Tonsky-style code faces:
   ;; - Few hues: blue, purple, orange, green
   ;; - Comments gray and quiet
   ;; - Strings green
   ;; - Keywords blue
   ;; - Types / constructors / important names orange
   ;; - Most identifiers stay near default fg.

   ;; Comments
   `(font-lock-comment-face
     ((,class (:foreground ,fg-faint))))
   `(font-lock-comment-delimiter-face
     ((,class (:inherit font-lock-comment-face))))
   `(font-lock-doc-face
     ((,class (:inherit font-lock-comment-face))))

   ;; Strings / chars / regexps
   `(font-lock-string-face
     ((,class (:foreground ,string))))
   `(font-lock-doc-string-face
     ((,class (:inherit font-lock-string-face))))

   ;; Keywords / control
   `(font-lock-keyword-face
     ((,class (:foreground ,kw :weight regular))))
   ;; Builtins / “attributes”
   `(font-lock-builtin-face
     ((,class (:foreground ,attr))))

   ;; Types / constructors / module names – use title orange
   `(font-lock-type-face
     ((,class (:foreground ,title :weight semi-bold))))
   `(font-lock-constant-face
     ((,class (:foreground ,title))))

   ;; Function & variable names: mostly neutral, slightly emphasized.
   `(font-lock-function-name-face
     ((,class (:foreground ,fg-soft :weight semi-bold))))
   `(font-lock-variable-name-face
     ((,class (:foreground ,fg))))

   ;; Numbers – treat like strings/symbols (green)
   `(font-lock-number-face
     ((,class (:foreground ,string))))
   `(font-lock-warning-face
     ((,class (:foreground ,warning :weight bold))))

   ;; Punctuation (some modes use this)
   `(font-lock-negation-char-face
     ((,class (:foreground ,kw))))

   ;; Show-paren
   `(show-paren-match
     ((,class (:background ,border :foreground ,fg-soft :weight bold))))
   `(show-paren-mismatch
     ((,class (:background ,error :foreground ,bg :weight bold))))

   ;; Search
   `(isearch ((,class (:background ,kw :foreground ,bg :weight bold))))
   `(lazy-highlight
     ((,class (:background ,border :foreground ,fg-soft))))

   ;; Compilation / flycheck / etc.
   `(success ((,class (:foreground ,success :weight semi-bold))))
   `(warning ((,class (:foreground ,warning :weight semi-bold))))
   `(error   ((,class (:foreground ,error   :weight semi-bold))))

   ;; Org-mode (kept simple)
   `(org-level-1 ((,class (:foreground ,title :weight bold :height 1.15))))
   `(org-level-2 ((,class (:foreground ,kw :weight semi-bold :height 1.10))))
   `(org-level-3 ((,class (:foreground ,fg-soft :weight semi-bold))))
   `(org-code    ((,class (:foreground ,string))))
   `(org-verbatim((,class (:foreground ,string))))
   `(org-block   ((,class (:background ,bg-alt :extend t))))
   `(org-block-begin-line ((,class (:foreground ,fg-faint :background ,bg-alt))))
   `(org-block-end-line   ((,class (:foreground ,fg-faint :background ,bg-alt))))

   ;; Tree-sitter / treesit faces – map onto our palette
   `(treesit-face-comment               ((,class (:inherit font-lock-comment-face))))
   `(treesit-face-string                ((,class (:inherit font-lock-string-face))))
   `(treesit-face-keyword               ((,class (:inherit font-lock-keyword-face))))
   `(treesit-face-builtin               ((,class (:inherit font-lock-builtin-face))))
   `(treesit-face-type                  ((,class (:inherit font-lock-type-face))))
   `(treesit-face-constant              ((,class (:inherit font-lock-constant-face))))
   `(treesit-face-number                ((,class (:inherit font-lock-number-face))))
   `(treesit-face-function              ((,class (:inherit font-lock-function-name-face))))
   `(treesit-face-function-call         ((,class (:inherit font-lock-function-name-face))))
   `(treesit-face-variable              ((,class (:inherit font-lock-variable-name-face))))
   ))

(provide-theme 'td)
