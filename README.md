# What is this repo

Something you probably don't care about. This is my portable Emacs
configuration, synchronized between all my machines.  It is only intended to
work with Emacs 24 running on Linux and OS X, including in -nw mode.

I mostly care about adding IDE-level features to Emacs for the programming
languages I use every day: C++, JavaScript, Python, Ruby and various Lisps. If
you are looking for a good generic Emacs configuration to start with, you might
want to check these ones:
* Prelude: https://github.com/bbatsov/prelude
* Steve Purcell's: https://github.com/purcell/emacs.d

## Content

* C++, including a working CEDET and an experimental LLVM/Clang-based indexing
  (see https://github.com/Andersbakken/rtags)
* JavaScript
* Clojure
* Markdown / Org / Ido / Magit / Autocomplete
* A few themes and Powerline

## Modules

The root file is `init.el`. It loads a number of modules which can be
individually enabled or disabled.

```lisp
;;; Uncomment the modules you'd like to use and restart Emacs afterwards,
;;; or evaluate the require expression with M-C-x.

(require 'init-prolog)      ; environment; must be loaded first
(require 'init-ui)          ; fonts, menubar, syntax highlighting etc.
(require 'init-user-prefs)  ; backup files, trailing spaces...
(require 'init-keyboard)    ; key bindings
(require 'init-util)        ; utilities like match paren, bookmarks...
(require 'init-extensions)  ; minor modes like CUA, 80 col, FIXME etc.
(require 'init-ido)
(require 'init-markdown)
(require 'init-org)
(require 'init-osx)
(require 'init-cpp)
(require 'init-autocomplete)
(require 'init-yasnippet)
;; (require 'init-cedet.el)
(require 'init-rtags)
(require 'init-javascript)
(require 'init-clojure)
```

If a file `init_local.el` exists, it will be loaded at the end; you can use this
as a mechanism to load machine-specific extensions.

## Keymap

### Global

Keybinding         | Description
-------------------|------------------------------------------------------------
<kbd>ESC</kbd>     | Quit command; equivalent to <bkd>C-g</kbd>.
<kbd>C-z</kbd>     | Undo! (`undo`).
<kbd>C-\`</kbd>    | `kill-this-buffer` (faster than <kbd>C-x k</kbd>).
<kbd>C-ESC</kbd>   | `delete-other-windows` (just keep the current window).
<kbd>C-c C-f</kbd> | Open recent file (completes open file with <kbd>C-x C-f</kbd>).
<kbd>M-g</kbd>     | `goto-line` (prompts for a line number).
<kbd>C-+</kbd>     | Increase font size (`text-scale-increase`).
<kbd>C--</kbd>     | Decrease font size (`text-scale-decrease`).
<kbd>M-C-l</kbd>   | Switch back and forth between the 2 top buffers (from XEmacs).
<kbd>M-ARROW</kbd> | Move the focus between visible buffers (faster than <kbd>C-x o</kbd>).

Editing any file:

Keybinding         | Description
-------------------|------------------------------------------------------------
<kbd>S-ENTER</kbd> | Return and indent.
<kbd>M-BCKSP</kbd> | `backward-kill-word`.
<kbd>C-&#124;<kbd> | Toggle 80-column ruler (fill column indicator).
<kbd>C-\\</kbd>    | Delete spaces after cursor (`delete-horizontal-space-forward`).
<kbd>C-c C-d</kbd> | Duplicate line.

Git:

Keybinding         | Description
-------------------|------------------------------------------------------------
<kbd>C-x g</bkd>   | Open git status mode (`magit-status`).

### C++

Keybinding         | Description
-------------------|------------------------------------------------------------
<kbd>C-TAB</bkd>   | Alternate between header file and source file.
<kbd>C-c ;</kbd>   | Rename variable under cursor (non-RTags).

BDE style:

Keybinding         | Description
-------------------|------------------------------------------------------------
<kbd<C-&gt;</kbd>  | Insert text after cursor to the right (for return, lock etc.).
<kbd>C-c =</bkd>   | Insert class definition header.
<kbd>C-c -</kbd>   | Insert class implementation header.


## Using Rtags

TBD.
