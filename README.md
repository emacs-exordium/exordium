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
(require 'init-environment) ; environment variables
(require 'init-ui)          ; fonts, menubar, syntax highlighting etc.
(require 'init-user-prefs)  ; backup files, trailing spaces...
(require 'init-keyboard)    ; key bindings
(require 'init-util)        ; utilities like match paren, bookmarks...
(require 'init-extensions)  ; minor modes like CUA, 80 col, etc.

;;; Usability
(require 'init-ido)
(require 'init-autocomplete)

;;; Major modes
(require 'init-markdown)
(require 'init-org)

;;; Themes
(if *environment-nw*
    (set-face-background 'highlight nil)
  ;; Using Emacs with GUI:
  (require 'init-themes)
  (require 'init-powerline))

;;; OS-specific things
(require 'init-osx)

;;; C++
(require 'init-cpp)
(require 'init-bde-style)
(require 'init-header-autocomplete)
(require 'init-yasnippet)
;; (require 'init-cedet.el)
(require 'init-rtags)

;;; Other programming languages
(require 'init-javascript)
(require 'init-clojure)

;;; Local extensions
(when (file-exists-p "~/.emacs.d/init_local.el")
  (load "~/.emacs.d/init_local.el"))
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
<kbd>C-c h</kbd>   | Open anything (with Helm).

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

### Start rdm

Rtags (see https://github.com/Andersbakken/rtags) is a LLVM-based C++ indexer
which provides a deamon called "rdm" to maintain an in-memory index, and a
command-line client called "rc". Rtag uses a single index for all symbols, but
it allows for loading and unloading projects individually.

To use it, first start the deamon:

```bash
$ rdm
```

This will start the deamon on the foreground, using a number of threads that is
function of the number of CPUs on the local machine. It starts by reading the
saved project indices in `~/.rtags` if any. By default it logs to the console
but you can make it log to a file instead. There are many options; use `--help`
to see the list.  You can also create a file `~/.rdmrc` containing the default
command line arguments.

Note that rdm may crash while trying to index a file. If it does, it will retry
a few times and then give up with the file it cannot parse.

### Set up your project

If the project root directory does not contain a .git or .svn repo, create a
file `.rtags-config` in the root directory with the specified content:

```
project: /path/to/project
```

Then you need to tell rdm how to compile your project, by creating a
compilation database in a file named `compile_commands.json` (see
http://clang.llvm.org/docs/JSONCompilationDatabase.html). The compilation
database contains one entry for each file to compile, like the following
(simplified for clarity):

```javascript
{ "directory": "/home/phil/workspaces/foo/",
  "command":   "/usr/bin/clang++ -Irelative
                -I/home/phil/workspaces/bde/groups/bsl/bsl+stdhdrs
                -I/home/phil/workspaces/bde/groups/bsl/bslma
                -I/home/phil/workspaces/bde/groups/bsl/bsls
                -c -o bar.o bar.cpp",
   "file":      "bar.cpp" }
```

You can generate this compilation database with the command M-x
`create-compilation-database`. But before you do, it needs a little help:
you need to tell it what `clang++` command to use to compile any file, with all
the `-I` directives that are necessary for your project.

First, if your project depends on external includes, you may want to create a
file `init_includes` in the project root directory, containing the paths for
the included libraries that your project depends on (one path per line). For
example:

```
bde/groups/bdl
bde/groups/bsl
```

Next you may want to set a few variables in your `init_local.el`:

```lisp
(when (featurep 'init-rtags)
  ;; Prefix for any path in init_includes, if you want to use relative paths
  ;; (default value is "")
  (setq *rtags-clang-include-dir-prefix* "/home/phil/workspaces/"))
  ;; List of regex to exclude from the include paths
  (setq *rtags-clang-exclude-directories*
        '("00deps" "/group$" "/doc$" "/package$" "/test$"))
  ;; Compilation command prefix (that is the default value):
  (setq *rtags-clang-command-prefix* "/usr/bin/clang++ -Irelative ")
  ;; Compilation command suffix (that is the default value):
  (setq *rtags-clang-command-suffix* " -c -o ")
```

The function `create-compilation-database` will prompt for a project directory
name, and then it will scan recursively any .cpp file in your project to create
an entry for that file in the compilation database. The compile command will
use include directives for any directory in your project as well as any
directory mentionned in `compile_includes` if any such file is present. Note
that it will recursively add an include directive for any subdirectory. Also
note that the compilation database file is silently overriden if it exists.

Once the compilation database is ready, load it with `rc`:

```bash
$ cd /where/your/compilation/db/is
$ rc -J
```

Check the output of rdm for any compilation errors and adjust your compilation
database accordingly. Rerun `rc -J` to reload the compilation database.

The rdm deamon should automatically re-compile any file you edit in Emacs as
soon as you save it (it uses inotify).

If you use multiple projects, you can list the loaded projects with `rc -w` and
switch to a new project with `rc -w <proj>` (a regex). You can unload a project
with `rc -W <proj>`.

### Using the index

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

## Header file autocomplete

This module sets up autocomplete for `#include` header files in C++ mode. It
reuses the file `compile_includes` mentioned above.

It is currently a bit redundant from Rtags. You need to define the following
variables in `init_local.el`:

```lisp
(when (featurep 'init-header-autocomplete)
  ;; Prefix for any path in init_includes, if you want to use relative paths
  ;; (default value is "")
  (setq *header-ac-include-dir-prefix* "/home/phil/workspaces/"))
  ;; List of subdirs to exclude from the include paths
  ;; (that is the default value):
  (setq *header-ac-exclude-directories* '("/group" "/doc" "/package" "/test"))
```

Then set up the autocomplete for your project with the command M-x
`set-header-autocomplete`. It should work for any subdirectory within your
project or within any path mentionned in the compile includes file. You can
display the list of include directories it uses with M-x
`show-header-autocomplete`.

Note that currently the setting is not saved and will be lost any time you
restart Emacs.
