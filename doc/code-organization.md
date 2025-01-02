# Code Organization

## Directories

The source code is in these directories:

* Main directory: *init.el*.
* *modules*: the configuration's modules, which all start with "init-".
* *themes*: the configuration's visual themes.
* *elpa*: packages loaded from melpa.
* *extensions*: third party packages not present in melpa, included in source
  code.
* *snippets*: snippet files for YASnippets.

Note: *vendor* should be removed (code should move to *extensions*).

## init.el

[init.el](../init.el) is
the entry point. It includes all other files using the `require`
feature. Any "required" module file must end with a `provide`
statement. The name of the file and the symbol in `provide` and
`require` must all be identical, otherwise Emacs can't find it.

If a file *before-init.el* is present, it is loaded at the very beginning. If a
file *after-init.el* is present, it is loaded at the very end.

If a file *prefs.el* is present, it is used to override the default
configuration preferences defined in *modules/init-prefs.el*. This allows for
tweaking a few things in the configuration without risk of conflicts during git
pull. For example you can put something like this in your *prefs.el*:

```lisp
;;; List of font names and sizes, in order of preference. Emacs will pick
;;; the first one that exists.
(setq exordium-preferred-fonts '(("Monospace" . 120)
                                 ("Mono" . 120)))

;;; If you don't want to current line to be highlighted:
(setq exordium-line-mode nil)

;;; Choose another theme:
(setq exordium-theme 'zenburn)
```

## Modules

### Lib, environment and preferences

* [init-lib.el](../modules/init-lib.el)
  defines utility functions used by other modules: things like file loading,
  string manipulation etc.
* [init-environment.el](../modules/init-environment.el)
  defines environment global variables, such as "are we on OSX".
* [init-prefs.el](../modules/init-prefs.el)
  defines preferences variables for all modules. It can be edited to change a
  preference, or alternatively you can create a file *prefs.el* (in
  the main directory) to override the variables in *init-prefs.el*.

### Look and feel

* [init-look-and-feel.el](../modules/init-look-and-feel.el):
  * Basic UI of Emacs. Fonts, frame size, tool bar, menu bar, scroll bar,
    cursor, font lock, electric stuff etc.
  * Keybindings: zoom, navigate between buffers etc.
  * Basic behavior: delete trailing spaces upon save, no backup files, prefer
    spaces over tabs.
* [init-util.el](../modules/init-util.el):
  utility functions and keys like goto matching parenthese, duplicate a line,
  delete words, display an 80 column ruler (FCI), expand the region.
* [init-highlight.el](../modules/init-highlight.el):
  Highlight the current line and the symbol under the cursor. The latter can
  be done automatically after a small delay (turned off by default), or on
  demand using a key (C-c C-space by default).

### Usability

* [init-ido.el](../modules/init-ido.el):
  configures IDO and find recent files.
* [init-helm.el](../modules/init-helm.el):
  configures Helm.
* [init-autocomplete.el](../modules/init-autocomplete.el):
  configures autocomplete with default sources.
* [init-company.el](../modules/init-company.el):
  configures company mode with default sources.
* [init-projectile.el](../modules/init-projectile.el):
  configures Projectile.
* [init-helm-projectile.el](../modules/init-helm-projectile.el):
  configures Helm when used with Projectile.
* [init-git.el](../modules/init-git.el):
  everything git-related.

### Modes

* [init-markdown.el](../modules/init-markdown.el)
  Utilities for markdown mode.
* [init-org.el](../modules/init-org.el)
  Utilities for org-mode.

### OS X

* [init-osx.el](../modules/init-osx.el)

### C++

* [init-cpp.el](../modules/init-cpp.el):
  basic C++ features such as swap between header and implementation.
* [init-bde-style.el](../modules/init-bde-style.el):
  an attempt to make the BDE style fit easier into Emacs.
* [init-yassnippet.el](../modules/init-yasnippet.el):
  configuration of YASnippet.

### Other languages

* [init-javascript.el](../modules/init-javascript.el)
* [init-python.el](../modules/init-python.el)
* [init-clojure.el](../modules/init-clojure.el)

## Themes

Work in progress. There are 3 themes:

* *Tomorrow*, wich comes with several variations (default theme is Tomorrow
  Night)
* *Monokai*
* *Solarized* dark and light.
