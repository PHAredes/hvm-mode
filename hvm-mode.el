;;; hvm-mode.el --- Major mode for editing HVM3 files with syntax highlighting and keybindings -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;; Syntax highlighting for HVM3 language with keybindings for running HVM commands.

;;; Code:

(require 'eri)
(require 'compile)
(require 'rainbow-delimiters)

;; Syntax table
(defvar hvm-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?/ ". 12" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  "Syntax table for `hvm-mode'.")

;; Define custom faces (unchanged from original)
(defface hvm-keyword-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for HVM keywords."
  :group 'hvm-faces)

(defface hvm-variable-face
  '((t (:inherit font-lock-variable-name-face :italic t)))
  "Face for HVM variables."
  :group 'hvm-faces)

(defface hvm-function-face
  '((t (:inherit font-lock-function-name-face)))
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
  "Face for HVM symbols (~ @ λ)."
  :group 'hvm-faces)

(defface hvm-sup-face
  '((t (:inherit font-lock-variable-name-face :italic t)))
  "Face for HVM superpositions and duplications (e.g., &, !&)."
  :group 'hvm-faces)

(defface hvm-datatype-face
  '((t (:inherit font-lock-type-face)))
  "Face for HVM datatypes (e.g., Nat in `data Nat`)."
  :group 'hvm-faces)

(defface hvm-comment-face
  '((t (:inherit font-lock-comment-face :italic t)))
  "Face for HVM comments."
  :group 'hvm-faces)

(defface hvm-let-bindings-face
  '((t (:inherit font-lock-builtin-face)))
  "Face for HVM let bindings."
  :group 'hvm-faces)

;; Define HVM keywords and operators
(defvar hvm-keywords
  '("data" "import")
  "HVM keywords.")

(defvar hvm-operators
  '("+" "-" "*" "/" "%" "=" "!" "&" "|" "^" "<" ">" "<<" "<=" ">>" ">=" "==")
  "HVM operators, including compound operators like ==.")

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

(defvar hvm-sup-regexp
  "\\(&\\)"
  "Regexp for HVM superpositions (&).")

(defvar hvm-dup-regexp
  "\\(!&\\)"
  "Regexp for HVM duplications (!&).")

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

(defvar hvm-let-bindings-regexp
  "\\(!\\^?\\|!!\\)\\s-"
  "Regexp for HVM let bindings (!, !!, !^) followed by whitespace.")

;; Font-lock keywords
(defvar hvm-font-lock-keywords
  `(;; Duplications: !&
    (,hvm-dup-regexp . 'hvm-sup-face)
    ;; Superpositions: &
    (,hvm-sup-regexp . 'hvm-sup-face)
    ;; Symbols: ~ @ λ
    (,hvm-symbols-regexp . 'hvm-symbols-face)
    ;; Keywords: data import
    (,hvm-keywords-adjusted-regexp 2 'hvm-keyword-face)
    ;; Constructors: #Name
    (,hvm-constructor-regexp . 'hvm-constructor-face)
    ;; Datatype names: after data
    (,hvm-datatype-regexp 1 'hvm-datatype-face)
    ;; Let bindings: ! !! !^ followed by space
    (,hvm-let-bindings-regexp . 'hvm-let-bindings-face)
    ;; Operators: + - * etc. followed by space
    (,hvm-operators-regexp . 'hvm-operator-face)
    ;; Functions: @fn (highlight identifier after @)
    (,hvm-function-regexp 1 'hvm-function-face)
    ;; Erasure: *
    (,hvm-erasure-regexp . 'hvm-operator-face)
    ;; Numbers: 123
    (,hvm-number-regexp . 'hvm-number-face)
    ;; Characters: 'a'
    (,hvm-char-regexp . 'hvm-char-face)
    ;; Comments: //
    (,hvm-comment-regexp . 'hvm-comment-face)
    ;; Variables: identifiers
    (,hvm-variable-regexp . 'hvm-variable-face))
  "Keyword highlighting for HVM mode.")

;; Customizable options for HVM flags
(defcustom hvm-use-stats t
  "If non-nil, include the -s flag (show statistics) in all HVM run commands."
  :type 'boolean
  :group 'hvm-mode)

(defcustom hvm-use-compiled t
  "If non-nil, include the -c flag (compiled mode) in all HVM run commands."
  :type 'boolean
  :group 'hvm-mode)

;; Disable asking about saving buffers before compilation
(setq compilation-ask-about-save nil)

;; Function to display compilation buffer
(defun hvm--display-compilation-right (buffer &optional _)
  "Display BUFFER in a side windows, about 40% of the frame width."
  (let ((existing-window (get-buffer-window buffer t)) ; Check all frames
        (window))
    (if existing-window
        ;; If the buffer is already displayed, reuse that window
        (setq window existing-window)
      ;; Otherwise, create a new window on the right
      (setq window (split-window-right (round (* (frame-width) -0.40))))
      (set-window-buffer window buffer)
      (set-window-dedicated-p window t)) ; Prevent other buffers from using this window
    (with-selected-window window
      (fit-window-to-buffer window nil nil nil (round (* (frame-width) 0.40))))
    window))

;; Hook this into the compilation buffer display
(add-to-list 'display-buffer-alist '("\\*hvm-output\\*" (hvm--display-compilation-right)))

;; Core function to run HVM commands in a compilation buffer
(defun hvm--run-command (command &optional extra-flags)
  "Run COMMAND in a compilation buffer, optionally with EXTRA-FLAGS."
  (let* ((base-flags (concat (when hvm-use-compiled " -c")
                             (when hvm-use-stats " -s")))
         (full-command (concat command base-flags (when extra-flags (concat " " extra-flags))))
         (compilation-buffer-name-function (lambda (_mode) "*hvm-output*"))
         (compilation-skip-to-next-location t))
    (compilation-start full-command 'compilation-mode nil t)))

;; Helper function to get the current file name
;; Modified hvm--get-current-file to support literate HVM files
(defun hvm--get-current-file ()
  "Get the effective file to use for HVM commands.
For regular HVM files (*.hvm), return the base name.
For literate HVM files (*.hvm.md), extract HVM code blocks to a temporary file
and return its full path."
  (let ((file (buffer-file-name)))
    (if (and file (string-match-p "\\.hvm\\.md\\'" file))
        (let ((code (hvm--extract-hvm-code)))
          (let ((temp-file (make-temp-file "hvm-literate-" nil ".hvm")))
            (with-temp-file temp-file
              (insert code))
            temp-file))
      (if file
          (file-name-nondirectory file)
        (error "No file associated with this buffer")))))

;; HVM command functions
(defun hvm-help ()
  "Run `hvm help` in a compilation buffer."
  (interactive)
  (hvm--run-command "hvm help"))

(defun hvm-run ()
  "Run `hvm run <file>` in a compilation buffer."
  (interactive)
  (hvm--run-command (concat "hvm run " (shell-quote-argument (hvm--get-current-file)))))

(defun hvm-run-typecheck ()
  "Run `hvm run <file> -t` in a compilation buffer to check types."
  (interactive)
  (hvm--run-command (concat "hvm run " (shell-quote-argument (hvm--get-current-file))) "-t"))

(defun hvm-run-collapse ()
  "Run `hvm run <file> -C` in a compilation buffer to collapse results."
  (interactive)
  (hvm--run-command (concat "hvm run " (shell-quote-argument (hvm--get-current-file))) "-C"))

(defun hvm-run-collapse-first ()
  "Run `hvm run <file> -C1` to collapse the first result."
  (interactive)
  (hvm--run-command (concat "hvm run " (shell-quote-argument (hvm--get-current-file))) "-C1"))

(defun hvm-run-debug ()
  "Run `hvm run <file> -d` in a compilation buffer with debug output."
  (interactive)
  (hvm--run-command (concat "hvm run " (shell-quote-argument (hvm--get-current-file))) "-d"))

(defun hvm-run-no-quotes ()
  "Run `hvm run <file> -Q` in a compilation buffer without quotes."
  (interactive)
  (hvm--run-command (concat "hvm run " (shell-quote-argument (hvm--get-current-file))) "-Q"))

(defun hvm-run-compiled-debug ()
  "Run `hvm run <file> -d` in a compilation buffer with debug."
  (interactive)
  (hvm--run-command (concat "hvm run " (shell-quote-argument (hvm--get-current-file))) "-d"))

(defun hvm-run-find-first ()
  "Run `hvm run <file> -C1` in a compilation buffer to find first result."
  (interactive)
  (hvm--run-command (concat "hvm run " (shell-quote-argument (hvm--get-current-file))) "-C1"))

(defun hvm-run-with-flags (flags)
  "Run `hvm run <file>` with user-specified FLAGS in a compilation buffer."
  (interactive "sEnter HVM flags (e.g., -t -C1): ")
  (hvm--run-command (concat "hvm run " (shell-quote-argument (hvm--get-current-file))) flags))

;; Function to extract HVM code blocks from the current buffer's content
(defun hvm--extract-hvm-code ()
  "Extract HVM code blocks from the current buffer's content.
Collects all lines within ```hvm ... ``` blocks, excluding the fences,
and returns them as a single string with preserved newlines."
  (let ((lines (split-string (buffer-string) "\n"))
        (code "")
        (in-block nil))
    (dolist (line lines)
      (cond
       ((and (not in-block) (string-equal line "```hvm"))
        (setq in-block t))
       ((and in-block (string-equal line "```"))
        (setq in-block nil))
       (in-block
        (setq code (concat code line "\n")))))
    code))

;; Keymap for hvm-mode
(defvar hvm-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Base commands (Agda-like)
    (define-key map (kbd "C-c C-;") 'hvm-run)         ;; hvm run (load/run) no flags
    (define-key map (kbd "C-c h") 'hvm-help)          ;; hvm help

    ;; Single flags (inspired by Agda conventions)
    (define-key map (kbd "C-c C-t") 'hvm-run-typecheck) ;; -t (type checking)
    (define-key map (kbd "C-c C-l") 'hvm-run-collapse)  ;; -C (collapse/reduce)
    (define-key map (kbd "C-c C-d") 'hvm-run-debug)     ;; -d (debug)
    (define-key map (kbd "C-c C-q") 'hvm-run-no-quotes) ;; -Q (no quotes)

    ;; Custom commands
    (define-key map (kbd "C-c C-c") 'hvm-run-find-first)       ;; -C1 (find)
    (define-key map (kbd "C-c C-x") 'hvm-run-with-flags)       ;; Prompt for flags

    ;; ERI indentation bindings
    (define-key map (kbd "TAB") 'eri-indent)                     ;; Indent
    (define-key map (kbd "S-<iso-lefttab>") 'eri-indent-reverse) ;; Reverse indent
    (define-key map (kbd "S-<lefttab>") 'eri-indent-reverse)     ;; Reverse indent
    (define-key map (kbd "S-<tab>") 'eri-indent-reverse)         ;; Reverse indent
    map)
  "Keymap for `hvm-mode`")

;;;###autoload
(define-derived-mode hvm-mode prog-mode "HVM3"
  "Major mode for editing HVM3 files."
  :syntax-table hvm-mode-syntax-table
  (setq font-lock-defaults '(hvm-font-lock-keywords))
  (setq comment-start "//")
  (setq comment-end "")
  (rainbow-delimiters-mode 1))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.hvm\\'" . hvm-mode))
(add-to-list 'auto-mode-alist '("\\.hvm\\.md\\'" . hvm-mode))

(provide 'hvm-mode)

;;; hvm-mode.el ends here
