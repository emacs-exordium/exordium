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

* C++, including a working CEDET and an experimental LLVM/Clang-based indexing.
* JavaScript.
* Clojure.
* Markdown / Org / Ido / Magit / Autocomplete / Etc.
* A few themes and Powerline.

## Modules

The root file is `init.el`. It loads a number of modules in the `modules`
directory which can be individually enabled or disabled.

```lisp
;;; Uncomment the modules you'd like to use and restart Emacs afterwards,
;;; or evaluate the require expression with M-C-x.

(require 'init-prolog)      ; must be loaded first
(require 'environment)      ; environment variables
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
<kbd>C-&#124;</kbd> | Toggle 80-column ruler (fill column indicator).
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

BDE style (see https://github.com/bloomberg/bde):

Keybinding         | Description
-------------------|------------------------------------------------------------
<kbd>C-&gt;</kbd>  | Insert text after cursor to the right (for return, lock etc.).
<kbd>C-c =</bkd>   | Insert class definition header.
<kbd>C-c -</kbd>   | Insert class implementation header.

## Using Rtags

Rtags (see https://github.com/Andersbakken/rtags) is a LLVM-based C++ indexer
which provides a deamon called "rdm" to maintain an in-memory index, and a
command-line client called "rc". Rtag uses a single index for all symbols, but
it allows loading and unloading projects.

To use it, first start the deamon:

#+BEGIN_SRC
$ rdm
#+END_SRC

It starts by reading the saved indexes in `~/.rtags` if any. By default it logs
to the console (use `--help` to see all its options).

Then you need to tell it how to compile your project with Clang, by creating a
compilation database in a file named `compile_commands.json` (see
http://clang.llvm.org/docs/JSONCompilationDatabase.html).

The first thing to do is to define the clang command to use to compile a single
file in your project. First set up a few variables in `init_local.el`:

* `*rtags-clang-command-prefix*`: default is `/usr/bin/clang++ -Irelative`.
* `*rtags-clang-command-suffix*`: default is `-c -o`.

You probably also need to set up the include paths e.g. the `-I`
directives. You can set the variable `*rtags-clang-include-projects*` to the
included directories, but since those are likely to be project-specific it is
better to create a file `compile_includes` in your project root dir.  Set the
content of this file to the list of directories to include, like for example:

```
/home/phil/work/bde/groups/bsl
/home/phil/work/bde/groups/bdl
```

You only need to indicate top-level directories because we will scan
sub-directories recursively from that list. You can set up a list of
sub-directories to exclude with variable `*rtags-clang-exclude-directories*`;
the default is `'("/group" "/doc" "/package" "/test")`. If you don't want to
set absolute paths, define a prefix with variable
`*rtags-clang-include-dir-prefix*` to something like `"/home/phil/work"`.

Next, generate the compilation database with M-x
`rtags-create-compilation-database`. It will prompt for a directory: use the
root directory of your project. It will then scan for any ".cpp" file
recursively and include the directives to compile this component in the
generated file `compile_commands.json`. Note that the file is overriden if it
exists.

Note that if your project's root directory does not have a ".git" or ".svn"
subdirectory, you also need to tell Rtags where the project root by creating a
file `.rtags-config` at the root directory, with this content:

```
project: /path/to/project
```

Once the compilation database is ready, load it with `rc`:

#+BEGIN_SRC
$ cd /where/your/compilation/db/is
$ rc -J
#+END_SRC

Check the output of `rdm` for any compilation errors and adjust your
compilation database accordingly. Rerun `rc -J` to reload the compilation
database.

If you use multiple projects, you can list the loaded projects with `rc -w` and
switch to a new project with `rc -w <proj>` (a regex). You can unload a project
with `rc -W <proj>`.

Now comes the good part:

Keybinding         | Description
-------------------|------------------------------------------------------------
<kbd>F3</kbd>      | Jump to symbol definition (also <kbd>C-x r .</kbd>).
<kbd>F4</kbd>      | Find references to symbol (also <kbd>C-x r ,</kbd>).
<kbd>C-x r &gt;</kbd> | Same as <kbd>F3</kbd> but prompts for symbol name.
<kbd>C-x r &lt;</kbd> | Same as <kbd>F4</kbd> but prompts for symbol name.
<kbd>C-x r R</kbd> | Rename symbol.
<kbd>M-C-g</kbd>   | Ido-like select symbol in file.
<kbd>C-x r v</kbd> | Find symbol virtual functions.
<kbd>M-[</kbd>     | Go back to previous location.
<kbd>M-]</kbd>     | Go forward to next location.

Useful functions:
* `rtags-find-file`: jump to file by name (full or partial).
* `rtags-print-cursorinfo`: print debugging info about symbol at point.
* `rtags-print-dependencies`: show all include files (recursively).
* `rtags-diagnostics`: starts an async process to receive warnings or errors
  from clang; integrates with flymake to put highlighting on code with warnings
  and errors.
