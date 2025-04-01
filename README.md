# HVM Mode
HVM Mode is a major mode for editing HVM3 files with syntax highlighting and keybindings for running HVM commands.

## Description
HVM Mode is designed to provide a convenient and efficient way to edit HVM3 files in Emacs. It provides syntax highlighting, keybindings, and indentation to make it easier to work with HVM3 code.

## Techniques Used
The code uses various techniques, including:
* **Regular Expressions**: To match HVM keywords, operators, and other syntax elements. For example, the `hvm-keywords-regexp` variable uses the [regexp-opt](https://www.gnu.org/software/emacs/manual/html_node/elisp/Regexp-Functions.html) function to create a regular expression that matches any of the HVM keywords.
* **Font Locking**: To highlight HVM syntax elements, such as keywords, operators, and comments. The `hvm-font-lock-keywords` variable defines the font locking rules for HVM mode.
* **Keybindings**: To provide convenient shortcuts for running HVM commands. The `hvm-mode-map` variable defines the keymap for HVM mode, which includes bindings for running HVM commands, such as `C-c C-f` for `hvm run <file>`.
* **Derived Modes**: To create a new major mode that inherits from `prog-mode`. The `define-derived-mode` function is used to define HVM mode.

## Non-Obvious Technologies or Libraries
The code uses the following non-obvious technologies or libraries:
* **ERI**: The code uses the ERI library, which provides indentation functions for Emacs. The `eri-indent` and `eri-indent-reverse` functions are used to provide indentation bindings for HVM mode.

## Project Structure
The project structure is as follows:
```markdown
.
└── hvm-mode.el
```
The `hvm-mode.el` file contains the implementation of HVM mode.

## Functionality
The HVM mode provides the following functionalities:
* **Syntax Highlighting**: Highlights HVM syntax elements, such as keywords, operators, and comments.
* **Keybindings**: Provides convenient shortcuts for running HVM commands, such as:
	+ `C-c C-f` for `hvm run <file>`
	+ `C-c C-t` for `hvm run <file> -t`
	+ `C-c C-c` for `hvm run <file> -c`
	+ `C-c C-r` for `hvm run <file> -C`
	+ `C-c C-1` for `hvm run <file> -C1`
	+ `C-c C-s` for `hvm run <file> -s`
	+ `C-c C-d` for `hvm run <file> -d`
	+ `C-c C-q` for `hvm run <file> -Q`
* **Indentation**: Provides indentation bindings using the ERI library.


## Installation
To install the HVM mode, follow these steps:

1. **Clone the repository**: Run the following command in your terminal:
```bash
git clone https://github.com/PHAredes/hvm-mode.git ~/.emacs.d/lisp/hvm-mode
```

2. **Add to Emacs configuration**: Add the following Elisp code to your `init.el` file:
```elisp
(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'hvm-mode)
(add-to-list 'auto-mode-alist '("\\.hvm\\'" . hvm-mode))
```
This will load the HVM mode and enable it for files with the `.hvm` extension.

## Contribution
To contribute to the HVM mode, please fork the repository and submit a pull request with your changes. Make sure to follow the [Emacs Lisp coding conventions](https://www.gnu.org/software/emacs/manual/html_node/elisp/Coding-Conventions.html) and test your changes thoroughly.

## License
The HVM mode is licensed under the [GPLv3](https://www.gnu.org/licenses/gpl-3.0.html) license.

## Acknowledgments
The HVM mode is built on top of the Emacs Lisp programming language and the ERI library. We would like to thank the authors of these projects for their hard work and dedication.
