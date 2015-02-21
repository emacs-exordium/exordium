# Code Organization

## Directories

The source code is in these directories:

* Main directory: *init.el* and the optional files *init-local-before.el*,
  *init-local-after.el* and *init-local-prefs.el*.
* *modules*: the configuration's modules, which all start with "init-".
* *themes*: the configuration's visual themes.
* *elpa*: packages loaded from melpa.
* *extensions*: third party packages not present in melpa, included in source
  code.
* *snippets*: snippet files for YASnippets.

Note: *vendor* should be removed (code should move to *extensions*).

## init.el

[init.el](https://raw.github.com/philippe-grenet/dot.emacs/master/init.el) is
the entry point. It includes all other files using the ```require```
feature. Any "required" module file must end with a ```provide```
statement. The name of the file and the symbol in ```provide``` and
```require``` must all be identical, otherwise Emacs can't find it.

If a file *init-local-before.el* is present, it is loaded at the beginning. If
a file *init-local-after.el* is present, it is loaded at the end.

If a file *init-local-prefs.el* is present, it is used to override the default
configuration preferences in *modules/init-prefs.el*. (This allow for tweaking
the configuration without risk of conflicts during git pull).

## Modules

### Lib, environment and preferences

* [init-lib.el](https://raw.github.com/philippe-grenet/dot.emacs/master/modules/init-lib.el)
  defines utility functions used by other modules: things like file loading,
  string manipulation etc.
* [init-environment.el](https://raw.github.com/philippe-grenet/dot.emacs/master/modules/init-environment.el)
  defines environment global variables, such as "are we on OSX".
* [init-prefs.el](https://raw.github.com/philippe-grenet/dot.emacs/master/modules/init-prefs.el)
  defines preferences variables for all modules. It can be edited to change a
  preference, or alternatively you can create a file *init-local-prefs.el* (in
  the main directory) to override the variables in *init-prefs.el*.

### Look and feel

* [init-ui.el](https://raw.github.com/philippe-grenet/dot.emacs/master/modules/init-ui.el):
  basic UI of Emacs. Fonts, frame size, tool bar, menu bar, scroll bar, cursor,
  font lock, etc.
* [init-behavior.el](https://raw.github.com/philippe-grenet/dot.emacs/master/modules/init-behavior.el):
  basic behavior: delete trailing spaces upon save, no backup files, prefer spaces over tabs.
* [init-keyboard.el](https://raw.github.com/philippe-grenet/dot.emacs/master/modules/init-keyboard.el):
  minor modes and custom non-mode-specific keys. Includes things like
  auto-pairs (automatically close parentheses), winmove, CUA, zoom, and keys to
  navigate between buffers.
* [init-util.el](https://raw.github.com/philippe-grenet/dot.emacs/master/modules/init-util.el):
  utility functions and keys like goto matching parenthese, duplicate a line,
  delete words, display an 80 column ruler (FCI), expand the region.

### Usability

* [init-ido.el](https://raw.github.com/philippe-grenet/dot.emacs/master/modules/init-ido.el):
  configures IDO and find recent files.
* [init-autocomplete.el](https://raw.github.com/philippe-grenet/dot.emacs/master/modules/init-autocomplete.el):
  configures autocomplete with default sources (non-RTags).
* [init-helm-projectile.el](https://raw.github.com/philippe-grenet/dot.emacs/master/modules/init-helm-projectile.el):
  configures Helm and Projectile.
* [init-git.el](https://raw.github.com/philippe-grenet/dot.emacs/master/modules/init-git.el):
  everything git-related.

### Modes

* [init-markdown.el](https://raw.github.com/philippe-grenet/dot.emacs/master/modules/init-markdown.el)
* [init-org.el](https://raw.github.com/philippe-grenet/dot.emacs/master/modules/init-org.el)

### OS X

* [init-osx.el](https://raw.github.com/philippe-grenet/dot.emacs/master/modules/init-osx.el)

### C++

* [init-cpp.el](https://raw.github.com/philippe-grenet/dot.emacs/master/modules/init-cpp.el):
  basic C++ features such as swap between header and implementation.
* [init-bde-style.el](https://raw.github.com/philippe-grenet/dot.emacs/master/modules/init-bde-style.el):
  an attempt to make the BDE style fit easier into Emacs.
* [init-yassnippet.el](https://raw.github.com/philippe-grenet/dot.emacs/master/modules/init-yasnippet.el):
  configuration of YASnippet.
* [init-cedet.el](https://raw.github.com/philippe-grenet/dot.emacs/master/modules/init-cedet.el):
  configuration of CEDET (not used anymore and should probably be removed).
* [init-rtags.el](https://raw.github.com/philippe-grenet/dot.emacs/master/modules/init-rtags.el):
  configuration of RTags.
* [init-header-autocomplete.el](https://raw.github.com/philippe-grenet/dot.emacs/master/modules/init-header-autocomplete.el):
  experimental autocomplete for header file names (which RTags does not do).

### Other languages

* [init-javascript.el](https://raw.github.com/philippe-grenet/dot.emacs/master/modules/init-javascript.el)
* [init-python.el](https://raw.github.com/philippe-grenet/dot.emacs/master/modules/init-python.el)
* [init-clojure.el](https://raw.github.com/philippe-grenet/dot.emacs/master/modules/init-clojure.el)

## Themes

Work in progress. There are 3 themes:

* *Tomorrow*, wich comes with several variations (default theme is Tomorrow
  Night)
* *Monokai*
* *Solarized* dark and light.
