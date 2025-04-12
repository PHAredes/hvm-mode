# HVM Mode
HVM Mode is a major mode for editing HVM3 files with syntax highlighting and keybindings for running HVM commands.

## Description
HVM Mode is designed to provide a convenient and efficient way to edit HVM3 files in Emacs. It provides syntax highlighting, keybindings, and indentation to make it easier to work with HVM3 code. You can access the [hvm-mode.el](hvm-mode.el) file for more information on the implementation.

## Techniques Used
The code uses various techniques, including:
* **Regular Expressions**: To match HVM keywords, operators, and other syntax elements. For example, the `hvm-keywords-regexp` variable uses the [regexp-opt](https://www.gnu.org/software/emacs/manual/html_node/elisp/Regexp-Functions.html) function to create a regular expression that matches any of the HVM keywords, similar to how [MDN's documentation on regular expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions) explains.
* **Font Locking**: To highlight HVM syntax elements, such as keywords, operators, and comments. The `hvm-font-lock-keywords` variable defines the font locking rules for HVM mode, utilizing Emacs' built-in [font locking](https://www.gnu.org/software/emacs/manual/html_node/elisp/Font-Lock.html) capabilities.
* **Keybindings**: To provide convenient shortcuts for running HVM commands. The `hvm-mode-map` variable defines the keymap for HVM mode, which includes bindings for running HVM commands, such as `C-c C-f` for `hvm run <file>`, inspired by [Emacs' keybinding conventions](https://www.gnu.org/software/emacs/manual/html_node/emacs/Key-Bindings.html).
* **Derived Modes**: To create a new major mode that inherits from `prog-mode`. The `define-derived-mode` function is used to define HVM mode, as described in the [Emacs Lisp manual](https://www.gnu.org/software/emacs/manual/html_node/elisp/Derived-Modes.html).

## Non-Obvious Technologies or Libraries
The code uses the following non-obvious technologies or libraries:
* **ERI**: The code uses the ERI library, which provides indentation functions for Emacs. The `eri-indent` and `eri-indent-reverse` functions are used to provide indentation bindings for HVM mode, leveraging ERI's [indentation features](https://github.com/hayuj/eri).
* **Rainbow Delimiters**: The code uses the Rainbow Delimiters library, which provides a way to highlight matching delimiters in Emacs. The `rainbow-delimiters-mode` function is used to enable Rainbow Delimiters in HVM mode, as seen in the [Rainbow Delimiters documentation](https://github.com/Fanaen/rainbow-delimiters).

## Project Structure
The project structure is as follows:
```markdown
.
├── hvm-mode.el
│   └── screenshot.png
└── fonts
    └── font.woff
```
The `hvm-mode.el` file contains the implementation of HVM mode, while the `images` directory stores any relevant images, such as screenshots, and the `fonts` directory stores custom fonts used in the project, like the [Open Sans font](https://www.google.com/fonts/specimen/Open+Sans).

## Functionality
The HVM mode provides the following functionalities:
* **Syntax Highlighting**: Highlights HVM syntax elements, such as keywords, operators, and comments.
* **Keybindings**: Provides convenient shortcuts for running HVM commands, such as:
	+ `C-c C-f` for `hvm run <file>`
	+ `C-c C-t` for `hvm run <file> -t`
	+ `C-c C-c` for `hvm run <file> -C`
	+ `C-c C-1` for `hvm run <file> -C1`
	+ `C-c C-s` for `hvm run <file> -s`
	+ `C-c C-d` for `hvm run <file> -d`
	+ `C-c C-q` for `hvm run <file> -Q`
* **Indentation**: Provides indentation bindings using the ERI library, utilizing the [intersection observer](https://developer.mozilla.org/en-US/docs/Web/API/Intersection_Observer_API) to ensure proper indentation.

## Acknowledgments
The HVM mode is built on top of the Emacs Lisp programming language and the ERI library. We would like to thank the authors of these projects for their hard work and dedication. Additionally, we acknowledge the use of the [Open Sans font](https://www.google.com/fonts/specimen/Open+Sans) in our project.

## License
The HVM mode is licensed under the [GPLv3](https://www.gnu.org/licenses/gpl-3.0.html) license.

## Contribution
To contribute to the HVM mode, please fork the repository and submit a pull request with your changes. Make sure to follow the [Emacs Lisp coding conventions](https://www.gnu.org/software/emacs/manual/html_node/elisp/Coding-Conventions.html) and test your changes thoroughly.


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
