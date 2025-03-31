;;; hvm-mode.el --- Major mode for editing HVM3 files. -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;; Syntax highlighting for HVM3 language, styled to match Agda-like highlighting.

;;; Code:
(defvar hvm-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Treat underscore as part of words
    (modify-syntax-entry ?_ "w" st)
    ;; Single-line comments with //
    (modify-syntax-entry ?/ ". 12" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  "Syntax table for `hvm-mode'.")

;; Define custom faces, inheriting from built-in Emacs faces
(defface hvm-keyword-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for HVM keywords."
  :group 'hvm-faces)

(defface hvm-variable-face
  '((t (:inherit font-lock-variable-name-face :italic t)))
  "Face for HVM variables."
  :group 'hvm-faces)

(defface hvm-function-face
  '((t (:inherit font-lock-function-name-face :italic t)))
  "Face for HVM functions (e.g., @fn)."
  :group 'hvm-faces)

(defface hvm-constructor-face
  '((t (:inherit font-lock-type-face)))
  "Face for HVM constructors (e.g., #Ctr)."
  :group 'hvm-faces)

(defface hvm-number-face
  '((t (:inherit font-lock-constant-face)))
  "Face for HVM numbers."
  :group 'hvm-faces)

(defface hvm-char-face
  '((t (:inherit font-lock-string-face :italic t)))
  "Face for HVM characters."
  :group 'hvm-faces)

(defface hvm-operator-face
  '((t (:inherit font-lock-builtin-face)))
  "Face for HVM operators."
  :group 'hvm-faces)

(defface hvm-symbols-face
  '((t (:inherit font-lock-builtin-face)))
  "Face for HVM symbols (~ @ ! λ)."
  :group 'hvm-faces)

(defface hvm-supdup-face
  '((t (:inherit font-lock-variable-name-face :italic t)))
  "Face for HVM superpositions and duplications (e.g., &L, !x)."
  :group 'hvm-faces)

(defface hvm-datatype-face
  '((t (:inherit font-lock-type-face)))
  "Face for HVM datatypes (e.g., Nat in `data Nat`)."
  :group 'hvm-faces)

(defface hvm-comment-face
  '((t (:inherit font-lock-comment-face :italic t)))
  "Face for HVM comments."
  :group 'hvm-faces)

(defface hvm-delimiters-face
  '((t (:inherit default)))
  "Face for HVM parentheses and braces."
  :group 'hvm-faces)

;; Define HVM keywords, operators, and prefixes
(defvar hvm-keywords
  '("Era" "Lam" "Sup" "App" "Op2" "Ctr" "Ref" "Mat" "Let" "Dup"
    "If" "Chr" "U32" "Str" "Nil" "Cons" "ADT" "data" "import"
    "package" "module" "where" "infix")
  "HVM keywords.")

(defvar hvm-operators
  '("+" "-" "*" "/" "%" "=" "<" ">" "<<" ">>" "<=" ">=" "^" "|")
  "HVM operators.")

(defvar hvm-prefixes
  '("&" "!")
  "HVM prefixes for superpositions and duplications.")

;; Convert lists to regex patterns
(defvar hvm-keywords-regexp
  (regexp-opt hvm-keywords 'words)
  "Regexp for HVM keywords.")

(defvar hvm-keywords-adjusted-regexp
  (concat "\\(^\\|[^#]\\)" hvm-keywords-regexp)
  "Regexp for HVM keywords, adjusted to not match inside constructors.")

(defvar hvm-operators-regexp
  (regexp-opt hvm-operators t)
  "Regexp for HVM operators.")

(defvar hvm-prefixes-regexp
  (concat "\\(" (regexp-opt hvm-prefixes t) "\\)[A-Za-z0-9_]+")
  "Regexp for HVM superpositions and duplications with prefixes like & or !.")

(defvar hvm-constructor-regexp
  "#[A-Za-z0-9_]+\\|#\."
  "Regexp for HVM constructors (e.g., #Nil, #Cons, #.).")

(defvar hvm-datatype-regexp
  "\\<data\\s-+\\([A-Za-z0-9_]+\\)"
  "Regexp for HVM datatype names after `data` (e.g., Nat in `data Nat`).")

(defvar hvm-function-regexp
  "@\\([a-zA-Z0-9_]+\\)"
  "Regexp for HVM functions (e.g., @fn, highlighting the identifier after @).")

(defvar hvm-erasure-regexp
  "\\*"
  "Regexp for HVM erasure symbol (*).")

(defvar hvm-symbols-regexp
  "[~@!λ]"
  "Regexp for HVM symbols ({ } ( ) ~ @ ! λ).")

(defvar hvm-number-regexp
  "\\<\\d+\\>"
  "Regexp for HVM numeric literals (e.g., 123).")

(defvar hvm-char-regexp
  "'\\([^']\\|\\\\'\\)'"
  "Regexp for HVM character literals (e.g., `a`).")

(defvar hvm-comment-regexp
  "//.*$"
  "Regexp for HVM comments (// and everything after).")

(defvar hvm-variable-regexp
  "\\<\\([a-zA-Z][A-Za-z0-9_]*\\)\\>"
  "Regexp for HVM variables (plain identifiers like `a`, `foo`).")

(defvar hvm-delimiters-regexp
  "[{}()[\\]]\\|}[{]\\|)\\("
  "Regexp for HVM delimiter pairs (e.g., { }, ( ), [ ]).")

(defvar hvm-font-lock-keywords
  `(;; Symbols: { } ( ) ~ @ ! λ (highest priority)
    (,hvm-symbols-regexp . 'hvm-symbols-face)
    ;; Adjusted Keywords to not match inside constructors
    (,hvm-keywords-adjusted-regexp 2 'hvm-keyword-face)
    ;; Constructors (e.g., #Nil, #Cons, #.)
    (,hvm-constructor-regexp . 'hvm-constructor-face)
    ;; Type names after 'data'
    (,hvm-datatype-regexp 1 'hvm-datatype-face)
    ;; Operators
    (,hvm-operators-regexp . 'hvm-operator-face)
    ;; Superpositions and duplications (e.g., &L, !x)
    (,hvm-prefixes-regexp . 'hvm-supdup-face)
    ;; Functions: @fn (highlight only the identifier after @)
    (,hvm-function-regexp 1 'hvm-function-face)
    ;; Erasure: *
    (,hvm-erasure-regexp . 'hvm-operator-face)
    ;; Numbers: 123
    (,hvm-number-regexp . 'hvm-number-face)
    ;; Characters: 'a'
    (,hvm-char-regexp . 'hvm-char-face)
    ;; Comments: // and everything after
    (,hvm-comment-regexp . 'hvm-comment-face)
    ;; Variables (plain identifiers like 'a', 'foo')
    (,hvm-variable-regexp . 'hvm-variable-face)
    (,hvm-delimiters-regexp . 'hvm-delimiters-face))
  "Keyword highlighting for HVM mode.")

;;;###autoload
(define-derived-mode hvm-mode prog-mode "HVM3"
  "Major mode for editing HVM3 files."
  :syntax-table hvm-mode-syntax-table
  (setq font-lock-defaults '(hvm-font-lock-keywords))
  (setq comment-start "//")
  (setq comment-end ""))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.hvm\\'" . hvm-mode))

(provide 'hvm-mode)

;;; hvm-mode.el ends here
