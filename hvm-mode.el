;;; hvm-mode.el --- Major mode for editing HVM3 files with syntax highlighting and keybindings -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;; Syntax highlighting for HVM3 language with keybindings for running HVM commands.

;;; Code:

;; Syntax table
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

(defface hvm-sup-face
  '((t (:inherit font-lock-variable-name-face :italic t)))
  "Face for HVM superpositions and duplications (e.g., &L, ! &n{x0 x1} = y)."
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

(defface hvm-let-bindings-face
  '((t (:inherit font-lock-builtin-face)))
  "Face for HVM let bindings"
  :group 'hvm-faces)

;; Define HVM keywords and operators
(defvar hvm-keywords
  '("data" "import")
  "HVM keywords.")

(defvar hvm-operators
  '("+" "-" "*" "/" "%" "=" "!" "&" "|" "^" "<" ">" "<<" "<=" ">>" ">=")
  "HVM operators.")

;; Convert lists to regex patterns
(defvar hvm-keywords-regexp
  (regexp-opt hvm-keywords 'words)
  "Regexp for HVM keywords.")

(defvar hvm-keywords-adjusted-regexp
  (concat "\\(^\\|[^#]\\)" hvm-keywords-regexp)
  "Regexp for HVM keywords, adjusted to not match inside constructors.")

(defvar hvm-operators-regexp
  (concat (regexp-opt hvm-operators t) "\\s-+")
  "Regexp for HVM operators, requiring a space after the operator.")

(defvar hvm-prefixes-regexp
  "\\(&\\)\\([0-9]+\\|{\\)"
  "Regexp for HVM sup and dup with & followed by a number or {.")

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
  (regexp-opt '("*") t)
  "Regexp for HVM erasure symbol (*).")

(defvar hvm-symbols-regexp
  "[~@λ]"
  "Regexp for HVM symbols (~ @ λ).")

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

(defvar hvm-let-bindings-regexp
  "\\(!\\^?\\|!!\\)\\s-"
  "Regexp for HVM let bindings (!, !!, !^) followed by whitespace.")

;; Font-lock keywords
(defvar hvm-font-lock-keywords
  `(;; Symbols: ~ @ λ (highest priority)
    (,hvm-symbols-regexp . 'hvm-symbols-face)
    ;; Adjusted Keywords to not match inside constructors
    (,hvm-keywords-adjusted-regexp 2 'hvm-keyword-face)
    ;; Constructors (e.g., #Nil, #Cons, #.)
    (,hvm-constructor-regexp . 'hvm-constructor-face)
    ;; Type names after 'data'
    (,hvm-datatype-regexp 1 'hvm-datatype-face)
    ;; Superpositions and duplications (e.g., &L, !x)
    (,hvm-prefixes-regexp . 'hvm-sup-face)
    ;; Operators
    (,hvm-operators-regexp . 'hvm-operator-face)
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

;; Helper functions for HVM commands
(defun hvm--get-current-file ()
  "Get the current HVM file name."
  (if (buffer-file-name)
      (file-name-nondirectory (buffer-file-name))
    (error "No file associated with this buffer")))

(defun hvm-help ()
  "Run `hvm help`."
  (interactive)
  (shell-command "hvm help"))

(defun hvm-run ()
  "Run `hvm run <file>`."
  (interactive)
  (shell-command (concat "hvm run " (shell-quote-argument (hvm--get-current-file)))))

(defun hvm-run-type ()
  "Run `hvm run <file> -t`."
  (interactive)
  (shell-command (concat "hvm run " (shell-quote-argument (hvm--get-current-file)) " -t")))

(defun hvm-run-compiled ()
  "Run `hvm run <file> -c`."
  (interactive)
  (shell-command (concat "hvm run " (shell-quote-argument (hvm--get-current-file)) " -c")))

(defun hvm-run-collapse ()
  "Run `hvm run <file> -C`."
  (interactive)
  (shell-command (concat "hvm run " (shell-quote-argument (hvm--get-current-file)) " -C")))

(defun hvm-run-collapse-1 ()
  "Run `hvm run <file> -C1`."
  (interactive)
  (shell-command (concat "hvm run " (shell-quote-argument (hvm--get-current-file)) " -C1")))

(defun hvm-run-stats ()
  "Run `hvm run <file> -s`."
  (interactive)
  (shell-command (concat "hvm run " (shell-quote-argument (hvm--get-current-file)) " -s")))

(defun hvm-run-debug ()
  "Run `hvm run <file> -d`."
  (interactive)
  (shell-command (concat "hvm run " (shell-quote-argument (hvm--get-current-file)) " -d")))

(defun hvm-run-no-quotes ()
  "Run `hvm run <file> -Q`."
  (interactive)
  (shell-command (concat "hvm run " (shell-quote-argument (hvm--get-current-file)) " -Q")))

(defun hvm-run-compiled-collapse ()
  "Run `hvm run <file> -c -C`."
  (interactive)
  (shell-command (concat "hvm run " (shell-quote-argument (hvm--get-current-file)) " -c -C")))

(defun hvm-run-compiled-stats ()
  "Run `hvm run <file> -c -s`."
  (interactive)
  (shell-command (concat "hvm run " (shell-quote-argument (hvm--get-current-file)) " -c -s")))

(defun hvm-run-compiled-debug ()
  "Run `hvm run <file> -c -d`."
  (interactive)
  (shell-command (concat "hvm run " (shell-quote-argument (hvm--get-current-file)) " -c -d")))

(defun hvm-run-find-next ()
  "Run `hvm run <file> -c -C1 -s`."
  (interactive)
  (shell-command (concat "hvm run " (shell-quote-argument (hvm--get-current-file)) " -c -C1 -s")))

;; Keymap for hvm-mode
(defvar hvm-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Base commands
    (define-key map (kbd "C-c h") 'hvm-help)              ;; hvm help
    (define-key map (kbd "C-c r") 'hvm-run)               ;; hvm run <file>
    ;; Single flags
    (define-key map (kbd "C-c t") 'hvm-run-type)          ;; -t (type)
    (define-key map (kbd "C-c c") 'hvm-run-compiled)      ;; -c (compiled)
    (define-key map (kbd "C-c C") 'hvm-run-collapse)      ;; -C (collapse)
    (define-key map (kbd "C-c 1") 'hvm-run-collapse-1)    ;; -C1 (collapse with N=1)
    (define-key map (kbd "C-c s") 'hvm-run-stats)         ;; -s (stats)
    (define-key map (kbd "C-c d") 'hvm-run-debug)         ;; -d (debug)
    (define-key map (kbd "C-c q") 'hvm-run-no-quotes)     ;; -Q (no quotes)
    ;; Common combinations
    (define-key map (kbd "C-c C-c") 'hvm-run-compiled-collapse)  ;; -c -C
    (define-key map (kbd "C-c C-s") 'hvm-run-compiled-stats)     ;; -c -s
    (define-key map (kbd "C-c C-d") 'hvm-run-compiled-debug)     ;; -c -d
    ;; Custom keybind
    (define-key map (kbd "C-c C-l") 'hvm-run-find-next)   ;; Custom: -c -C1 -s
    map)
  "Keymap for `hvm-mode'.")

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
