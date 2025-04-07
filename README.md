# HVM Mode

HVM Mode is a major mode for editing HVM3 files with syntax coloring, keybindings for running HVM commands, indentation and a primitive literate programming alike.

## Description

HVM Mode is designed to provide a convenient and efficient way to edit HVM3 files in Emacs. It provides syntax highlighting, keybindings for interacting with the HVM interpreter, and indentation support via the ERI library. It also supports literate HVM files (`.hvm.md`) by extracting code blocks for execution, in a very initial stage for literate programming.

## Features

- **Syntax Highlighting**:  
  Highlights various HVM3 syntax elements, including:  
  - Keywords (`data`, `import`)  
  - Variables (e.g., `foo`, `x`)  
  - Functions (e.g., `@fn`)  
  - Constructors (e.g., `#Cons`, `#Nil`)  
  - Numbers (e.g., `123`)  
  - Characters (e.g., `'a'`)  
  - Operators (e.g., `+`, `-`, `==`)  
  - Symbols (e.g., `~`, `@`, `λ`)  
  - Superpositions and duplications (e.g., `&{a0 a1}`, `!&{a0 a1} = foo`)  
  - Datatypes (e.g., `Nat` in `data Nat`)  
  - Comments (e.g., `// comment`)  
  - Let bindings (e.g., `!`, `!!`, `!^`)  

- **Keybindings**:  
  Provides shortcuts for running HVM commands in a compilation buffer:  
  - `C-c C-;` : Run `hvm run <file>` (load and run the current file)  
  - `C-c h`   : Run `hvm help`  
  - `C-c C-t` : Run `hvm run <file> -t` (type checking)  
  - `C-c C-l` : Run `hvm run <file> -C` (collapse/reduce)  
  - `C-c C-d` : Run `hvm run <file> -d` (debug)  
  - `C-c C-q` : Run `hvm run <file> -Q` (no quotes)  
  - `C-c C-c` : Run `hvm run <file> -C1` (find first result)  
  - `C-c C-x` : Prompt for custom flags and run `hvm run <file>` with those flags  

- **Indentation**:  
  Uses the ERI library for indentation:  
  - `TAB` : Indent  
  - `S-<iso-lefttab>`, `S-<lefttab>`, `S-<tab>` : Reverse indent  

- **Support for Literate HVM Files**:  
  Handles `.hvm.md` files by extracting HVM code blocks enclosed in ```` ```hvm ```` and ```` ``` ```` fences, allowing execution of embedded HVM code.  

- **Compilation Integration**:  
  Runs HVM commands in a dedicated `*hvm-output*` compilation buffer, displayed in a side window (approximately 40% of frame width) for easy interaction and output review.  

## Techniques Used

- **Regular Expressions**:  
  Utilizes `regexp-opt` and custom regex patterns to match HVM3 syntax elements for highlighting (e.g., `hvm-keywords-regexp`, `hvm-operators-regexp`).  

- **Font Locking**:  
  Defines custom faces (e.g., `hvm-keyword-face`, `hvm-function-face`) and applies them via `hvm-font-lock-keywords` for syntax highlighting.  

- **Keybindings**:  
  Implements a keymap (`hvm-mode-map`) with Agda-inspired bindings for running HVM commands and managing indentation.  

- **Derived Modes**:  
  Builds on `prog-mode` using `define-derived-mode` to create a tailored editing environment for HVM3.  

- **Buffer Management**:  
  Customizes compilation buffer display with `hvm--display-compilation-right` for a user-friendly experience.  

## Non-Obvious Technologies or Libraries

- **ERI**:  
  The ERI library provides indentation functionality, with `eri-indent` and `eri-indent-reverse` bound to `TAB` and shift-tab variants, respectively.  

- **Rainbow Delimiters**:  
  Enables `rainbow-delimiters-mode` to colorize nested parentheses, improving code readability.  

- **Compilation Mode**:  
  Leverages Emacs’ `compilation-mode` to run HVM commands and display output, with custom buffer naming and window management.  

## Project Structure

```markdown
.
└── hvm-mode.el

```

The hvm-mode.el file contains the full implementation of HVM Mode.

## Installation

To install HVM Mode, follow these steps:

```bash
git clone https://github.com/PHAredes/hvm-mode.git ~/.emacs.d/lisp/hvm-mode
```

Add this to your init.el or .emacs.el:

```elisp
(add-to-list 'load-path "~/.emacs.d/lisp/hvm-mode")
(require 'hvm-mode)
```

## Usage

Once installed, HVM Mode activates automatically for files with .hvm or .hvm.md extensions. Use the keybindings listed above to run HVM commands directly from Emacs.

For literate HVM files (.hvm.md), code blocks enclosed in ```hvm and ``` fences are extracted and treated as HVM code for execution.

The output of HVM commands appears in the *hvm-output* buffer, displayed in a side window for convenient review.

Customization
HVM Mode offers the following customizable options:

- (hvm-use-status <value>), default: t 
If non-nil, includes the -s flag (show statistics) in all HVM run commands
- (hvm-use-compiled <value>), default: t
If non-nil, includes the -c flag (compiled mode) in all HVM run commands

## Contribution

To contribute, fork the repository and submit a pull request with your changes. Adhere to [Emacs Lisp coding](https://www.gnu.org/software/emacs/manual/html_node/elisp/Coding-Conventions.html) conventions and test your modifications thoroughly.

## License
HVM Mode is licensed under the [GPLv3](/LICENSE).

## Acknowledgments
Thanks to the authors of the ERI library, the Rainbow Delimiters package, and the Emacs community for their contributions to Emacs Lisp development.
