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

;; Define custom faces with exact colors from the palette
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

(defface hvm-lambda-face
  '((t (:inherit font-lock-builtin-face)))
  "Face for HVM lambda symbols."
  :group 'hvm-faces)

(defface hvm-super-face
  '((t (:inherit font-lock-variable-name-face :italic t)))
  "Face for HVM superpositions (e.g., &L)."
  :group 'hvm-faces)

(defface hvm-match-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for HVM match expressions."
  :group 'hvm-faces)

(defface hvm-comment-face
  '((t (:inherit font-lock-comment-face :italic t)))
  "Face for HVM comments."
  :group 'hvm-faces)

(defface hvm-parens-face
  '((t (:inherit default)))
  "Face for HVM parentheses and braces."
  :group 'hvm-faces)

;; Define HVM keywords, operators, and variable prefixes
(defvar hvm-keywords
  '("Era" "Lam" "Sup" "App" "Op2" "Ctr" "Ref" "Mat" "Let" "Dup"
    "If" "Chr" "U32" "Str" "Nil" "Cons" "ADT" "data" "import"
    "package" "module" "where" "infix")
  "HVM keywords.")

(defvar hvm-operators
  '(">>=" "=" "!" "<<" ">>" "||" "&&" "==" "/=" ">=" "<="
    "-" "+" "*" "/" "%" "^" "|" "~" "λ")
  "HVM operators.")

(defvar hvm-variables
  '("$" "&")
  "HVM variable prefixes.")

;; Convert lists to regex patterns
(defvar hvm-keywords-regexp
  (regexp-opt hvm-keywords 'words)
  "Regexp for HVM keywords.")

(defvar hvm-operators-regexp
  (regexp-opt hvm-operators t)
  "Regexp for HVM operators.")

(defvar hvm-variables-regexp
  (concat "\\(" (regexp-opt hvm-variables t) "\\)[A-Za-z0-9_]+")
  "Regexp for HVM variables with prefixes like $ or &.")

(defvar hvm-font-lock-keywords
  `(;; Adjusted Keywords to not match inside constructors
    (,(concat "\\(^\\|[^#]\\)" hvm-keywords-regexp) 2 'hvm-keyword-face)
    ;; Constructors (e.g., #Nil, #Cons, #.)
    ("#[A-Za-z0-9_]+\\|#\." . 'hvm-constructor-face)
    ;; Type names after 'data'
    ("\\<data\\s-+\\([A-Za-z0-9_]+\\)" 1 'hvm-match-face)
    ;; Operators
    (,hvm-operators-regexp . 'hvm-operator-face)
    ;; Variables with prefixes (e.g., $x, &L)
    (,hvm-variables-regexp . 'hvm-variable-face)
    ;; Functions: @fn
    ("@[a-zA-Z0-9_]+" . 'hvm-function-face)
    ;; Erasure: *
    ("\\*" . 'hvm-operator-face)
    ;; Lambda: λ or \ (space after \)
    ("λ\\|\\\\ " . 'hvm-lambda-face)
    ;; Superposition: &L
    ("&[A-Za-z0-9]+" . 'hvm-super-face)
    ;; Duplication: !
    ("!" . 'hvm-operator-face)
    ;; Match: ~
    ("~" . 'hvm-match-face)
    ;; Numbers: 123
    ("\\<\\d+\\>" . 'hvm-number-face)
    ;; Characters: 'a'
    ("'\\([^']\\|\\\\'\\)'" . 'hvm-char-face)
    ;; Comments: // and everything after
    ("//.*$" . 'hvm-comment-face))
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
