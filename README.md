# What is this repo

Something you probably don't care about. This is my portable Emacs
configuration, synchronized between all my machines.  It is only intended to
work with Emacs 24 running on Linux and OS X, including in -nw mode.

I mostly care about adding IDE-level features to Emacs for the programming
languages I use every day: C++, JavaScript, Python, Ruby and various Lisps. If
you are looking for a good generic Emacs configuration to start with, you might
want to check these links:
* [Emacs Prelude](https://github.com/bbatsov/prelude);
* [Steve Purcell's config](https://github.com/purcell/emacs.d);
* [Awesome Emacs](https://github.com/emacs-tw/awesome-emacs).

## Content

Programming:

* C++: a working [CEDET](http://cedet.sourceforge.net).
* C++: [RTags](https://github.com/Andersbakken/rtags), a LLVM/Clang-based code
  indexer.
* C++: style for [BDE](https://github.com/bloomberg/bde).
* JavaScript: [js2-mode](http://www.emacswiki.org/emacs/Js2Mode).
* Clojure: [Cider](https://github.com/clojure-emacs/cider) and
  [Lein](http://leiningen.org).
* [Auto Complete](https://github.com/auto-complete/auto-complete).
* [YASnippet](https://github.com/capitaomorte/yasnippet), a template system.
* [Magit](http://magit.github.io).

Other:

* Markdown: [markdown-mode](http://www.emacswiki.org/emacs/MarkdownMode).
* Org: [org-mode](http://orgmode.org).
* [IDO](http://www.emacswiki.org/emacs/InteractivelyDoThings) (built-in).
* [Windmove](http://www.emacswiki.org/emacs/WindMove): move between windows
  with Shift-Meta-arrows.
* [Expand Region](https://github.com/magnars/expand-region.el): increase
  selected region by semantic units.
* [Fill Column Indicator](http://www.emacswiki.org/emacs/FillColumnIndicator).
* A few themes such as
  [Tomorrow Night](https://github.com/chriskempson/tomorrow-theme) and
  [PowerLine](http://www.emacswiki.org/emacs/PowerLine).

## Modules

The root file is `init.el`. It loads a number of modules in the `modules`
directory which can be individually enabled or disabled.

```lisp
;;; Uncomment the modules you'd like to use and restart Emacs afterwards,
;;; or evaluate the require expression with M-C-x.

(require 'init-prolog)      ; must be loaded first
(require 'init-environment) ; environment variables

;;; Local preferences (fonts, frame size etc.)
(require 'init-prefs)       ; defines variables that init-local-prefs can override
(when (file-exists-p "~/.emacs.d/init-local-prefs.el")
  (load "~/.emacs.d/init-local-prefs.el"))

;;; Look and feel
(require 'init-ui)          ; fonts, menubar, syntax highlighting etc.
(require 'init-behavior)    ; backup files, trailing spaces...
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
(when *environment-osx*
  (require 'init-osx))

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
(when (file-exists-p "~/.emacs.d/init-local.el")
  (load "~/.emacs.d/init-local.el"))
```

## Machine-local preferences

Two files can be added to customize the configuration for the local machine
(they are ignored in git):

* If a file `~/.emacs.d/init-local-prefs.el` exists, it is loaded at the
  beginning. It is used to override the default values of fonts, window size
  etc. The preferences variables that may be overriden are defined in
  `~/.emacs.d/modules/init-prefs.el` (see that module for details).
* If a file `~/.emacs.d/init-local.el` exists, it will be loaded at the end;
  you can use this as a mechanism to load machine-specific extensions.

## Keymap

### Global

Keybinding           | Description
---------------------|---------------------------------------------------------
<kbd>ESC</kbd>       | Quit command/dismiss window/abort autocomplete. It is equivalent to <bkd>C-g</kbd>.
<kbd>C-z</kbd>       | Undo! (`undo`).
<kbd>C-\`</kbd>      | `kill-this-buffer` (faster than <kbd>C-x k</kbd>).
<kbd>C-ESC</kbd>     | `delete-other-windows` (just keep the current window).
<kbd>C-x C-r</kbd>   | Open recent file (completes open file with <kbd>C-x C-f</kbd>).
<kbd>M-g</kbd>       | `goto-line` (prompts for a line number).
<kbd>C-+</kbd>       | Increase font size (`text-scale-increase`).
<kbd>C--</kbd>       | Decrease font size (`text-scale-decrease`).
<kbd>M-C-l</kbd>     | Switch back and forth between the 2 top buffers (from XEmacs).
<kbd>M-S-ARROW</kbd> | Move the focus between visible buffers (faster than <kbd>C-x o</kbd>).

Editing any file:

Keybinding          | Description
--------------------|----------------------------------------------------------
<kbd>RETURN</kbd>   | Return and indent by defaut; use <kbd>S-RETURN</kbd> for just return.
<kbd>M-BCKSP</kbd>  | `backward-kill-word` (e.g. the opposite of <kbd>M-d</kbd> `kill-word`).
<kbd>C-\\</kbd>     | Delete spaces after cursor (`delete-horizontal-space-forward`).
<kbd>C-BCKSP</kbd>  | Delete spaces before cursor (`delete-horizontal-space-backward`).
<kbd>M-\\</kbd>     | Delete all spaces around cursor.
<kbd>C-c C-d</kbd>  | Duplicate line.
<kbd>C-=</kbd>      | Expand region.
<kbd>C-&#124;</kbd> | Toggle 80-column ruler (fill column indicator).
<kbd>C-c C-s</kbd>  | Push point onto position stack.
<kbd>C-c C-b</kbd>  | Pop point from position stack.

Auto-complete:

Keybinding         | Description
-------------------|-----------------------------------------------------------
<kbd>M-/</bkd>     | Force trigger auto-complete.
<kbd>ESC</kbd>     | Abort auto-complete.

Git:

Keybinding         | Description
-------------------|-----------------------------------------------------------
<kbd>C-x g</bkd>   | Open git status mode (`magit-status`).

Helm (work in progress):

Keybinding         | Description
-------------------|-----------------------------------------------------------
<kbd>C-c h</kbd>   | Open anything.

### C++

Keybinding         | Description
-------------------|-----------------------------------------------------------
<kbd>C-TAB</bkd>   | Alternate between header file and source file.
<kbd>C-c ;</kbd>   | Rename variable under cursor (non-RTags).

[BDE](https://github.com/bloomberg/bde) style:

Keybinding         | Description
-------------------|-----------------------------------------------------------
<kbd>C-&gt;</kbd>  | Right-align text after cursor (for // return, // lock etc.).
<kbd>C-c =</bkd>   | Insert class definition header.
<kbd>C-c -</kbd>   | Insert class implementation header.

## Using RTags

### Start rdm

[RTags](https://github.com/Andersbakken/rtags) is a LLVM-based C++ indexer
which provides a deamon called "rdm" that maintain an in-memory index, and a
command-line client called "rc". RTags uses a single index for all symbols, but
it allows for loading and unloading projects individually.

To use it, first start the deamon:

```bash
$ rdm
```

This will start the deamon on the foreground, using a number of concurrent "rp"
jobs that is function of the number of CPUs on the local machine. It starts by
reading the saved project indices in `~/.rtags` if any. By default it logs to
the console but you can make it log to a file instead. There are many options;
use `--help` to see the list.  You can also create a file `~/.rdmrc` containing
the default command line arguments.

Alternatively you can run rdm as an Emacs subprocess: M-x `rtags-start-rdm`,
with logs going into a buffer. Stop it with M-x `rtags-quit-rdm`.

### Controlling rdm

Command            | Description
-------------------|-----------------------------------------------------------
`rc -w`            | List projects in the index.
`rc -w proj`       | Switch to project "proj" (a regex).
`rc -W proj`       | Unload and delete project "proj".
`rc -J`            | Reload the compilation DB from the current directory.
`rc --find-project-root /path/to/sourcefile.cpp` | Print what it determines to be the correct project root.
`rc -T sourcefile.cpp` | Say whether this file is indexed or not.
`rc -q`            | Shutdown rdm.

Note that a job may crash while trying to index a file. If it does, rdm will
retry a few times and then give up with the file it cannot parse.

### Setting up your project

If the project root directory does not contain a `.git` or `.svn` repo, you
need to create a file `.rtags-config` in the root directory with the specified
content:

```
project: /path/to/project
```

Then you need to tell rdm how to compile your project, by creating a
[compilation database](http://clang.llvm.org/docs/JSONCompilationDatabase.html)
in a file named `compile_commands.json`. The compilation database contains one
entry for each file to compile, like the following (simplified for clarity):

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
`rtags-create-compilation-database`. But before you do, it needs a little help:
you need to tell it what `clang++` command to use to compile any file, with all
the `-I` directives that are necessary for your project.

The command uses a file `compile_includes` in the project root dir, which
specifies how to generate `compilation_database.json` for your project. It is a
simple text file indicating where are all the source files and all the include
files. The "src" directives indicate where to find the source files to put in
the index (each of them will have its own entry in the compilation
database). The "include" directives indicate additional "-I" includes in the
clang command line. Both are recursive: any path will be scanned for source
files or subdirectories; however the "exclude" directives indicate what
patterns (regex) to exclude when scanning the "src" and "include"
paths. Finally the "excludesrc" directive is used to specify patterns (regex)
of source files names to exclude.

Note that all directives except "src" are optional. Also note that paths are
either absolute or relative to the project root. Here is an example:

```
  # 'compile_includes' file for project foo
  # Patterns to exclude in -I directives and while looking for sources:
  exclude /test$
  exclude /doc$
  exclude /group$
  exclude /package$

  # Where are the source files (there could be multiple directories).
  # We will scan recursively any subdirectories that do not match any
  # 'exclude' regex.
  src .

  # What to put in -I directives (in addition to the source files above).
  # We will scan recursively any subdirectories that do not match any
  # 'exclude' regex.
  include /Users/phil/Code/cpp/include/bsl
  include /Users/phil/Code/cpp/include/bdl

  # If any file name pattern must be excluded from the "src" files, use
  # the "excludesrc" directive. For example this will exclude all test
  # drivers:
  excludesrc \.t\.cpp$
```

In addition, the creation of a compilation database uses these variables:

Variable                            | Description
------------------------------------|------------------------------------------
`*rtags-compile-includes-base-dir*` | Set this to your workspace path if you want to use relative paths in `compile_includes` that are not relative to the project's root directory (the default).
`*rtags-clang-command-prefix*`      | Default is "/usr/bin/clang++ -Irelative" (note that RTags ignores the clang++ command because it uses libclang).
`*rtags-clang-command-suffix*`      | Default is "-c -o".

Once you have created the `compile_includes` file, run the command M-x
`rtags-create-compilation-database`. It will:

* Prompt for the project root dir;
* Read the `compile_includes` file;
* Scan all source dirs and include dirs according to what the file says;
* Create `compilation_database.json` (note: it overwrites it without asking);
* Ask if you want to reload it (if rdm is running as an Emacs subprocess).

You can reload the compilation database manually with `rc`:

```bash
$ cd /where/your/compilation/db/is
$ rc -J
```

Check the output of rdm for any compilation errors and adjust your compilation
database accordingly.

The rdm deamon should automatically re-compile any file you edit in Emacs as
soon as you save it or otherwise touch it.

### Using the index

While RTags uses <kbd>C-x r</kbd> as default prefix, this configuration uses
<kbd>C-c r</kbd> instead because it it less crowded. It also adds a few keys.

Navigating keys:

Keybinding                           | Description
-------------------------------------|-----------------------------------------
<kbd>F3</kbd>, <kbd>C-c r .</kbd>    | Jump to symbol definition.
<kbd>F4</kbd>, <kbd>C-c r ,</kbd>    | Find references to symbol.
<kbd>C-c r &gt;</kbd>                | Find symbol (prompts for symbol name).
<kbd>C-c r &lt;</kbd>                | Find references (prompts for symbol name).
<kbd>C-c r v</kbd>                   | Find all implementations of virtual function.
<kbd>M-C-g</kbd>, <kbd>C-c r I</kbd> | Imenu-like find symbol in file.
<kbd>C-c r T</kbd>                   | Display tag list in a window on the left side.
<kbd>C-c r ;</kbd>                   | `rtags-find-file` using partial name (non IDO).

Any navigation is recorded onto a stack, so it is easy to go back and forth:

Keybinding                             | Description
---------------------------------------|---------------------------------------
<kbd>M-LEFT</kbd>, <kbd>C-c r [</kbd>  | Go back to previous location.
<kbd>M-RIGHT</kbd>, <kbd>C-c r ]</kbd> | Go forward to next location.

Refactoring:

Keybinding         | Description
-------------------|-----------------------------------------------------------
<kbd>C-c r R</kbd> | Rename symbol.

Control:

Keybinding         | Description
-------------------|-----------------------------------------------------------
<kbd>C-c r p</kbd> | Switch project.
<kbd>C-c r e</kbd> | Reparse file, e.g. recompile.

Debugging utilities:

Keybinding         | Description
-------------------|-----------------------------------------------------------
<kbd>C-c r l</kbd> | Show the rdm log buffer.
<kbd>C-c r U</kbd> | Show what rdm knows about a symbol.
<kbd>C-c r P</kbd> | Show all includes for the current file.
<kbd>C-c r T</kbd> | Show the tag list for the current file.

### Using flymake

The function `rtags-diagnostics` bound to <kbd>C-c r D</kbd> starts an async
process to receive compilation warnings and errors from rdm. They are displayed
into diagnostics buffer which works with flymake to put highlighting on code
with warnings and errors. You can:

* Click on the highlighted symbol in your code to view the error message
* Click on the error line in the diagnostics buffer to jump to the error location.

![RTags diagnostics](https://raw.github.com/philippe-grenet/dot.emacs/master/doc/rtags_diagnostics.png)

Keybinding         | Description
-------------------|-----------------------------------------------------------
<kbd>C-c r D</kbd> | Run `rtags-diagnostics` if it wasn't and force reparsing of current buffer.
<kbd>C-c r d</kbd> | Show the diagnostics buffer without force reparsing (<kbd>ESC</kbd> to dismiss it).
<kbd>C-c r F</kbd> | Fix the error using Clang's "did you mean" (try it with "inft x;")
<kbd>C-c r Q</kbd> | `rtags-stop-diagnostics` stop the async process.

Other functions:
* `rtags-next-diag` goes to the next problem.
* `rtags-clear-diagnostics` clears any error or warning overlay.

### Autocomplete

**This is experimental for now**.

Set the variable `*init-rtags-auto-complete*` to true in your
`init-local-prefs.el` to turn on RTags as the source for auto-complete in C/C++
mode. It will work when you activate the diagnostics mode with <kbd>C-c r
D</kbd>. Note that this variable makes RTags be the *only* source for
auto-complete in C/C++ mode, e.g. all other classic sources such as names in
open buffers are disabled. The reasoning being that surely Clang must be more
accurate.

One issue is that auto-complete triggers rdm very often. Also, since
auto-complete is linked to the diagnostics mode, it ends up displaying errors
in the code you are typing all the time, which is distracting.

## Header file autocomplete

**TODO work in progress**. This is a temporary solution until autocomplete uses
rdm as a source.

This module sets up autocomplete for `#include` header files in C++ mode. It
reuses the file `compile_includes` mentioned above.

It is currently a bit redundant from RTags. You need to define the following
variables in `init_local.el`:

```lisp
(when (featurep 'init-header-autocomplete)
  ;; Prefix for any path in init_includes, if you want to use relative paths
  ;; (default value is "")
  (setq *header-ac-include-dir-prefix* "/home/phil/workspaces/")
  ;; List of regex to exclude from the include paths
  (setq *header-ac-exclude-directories*
        '("/group$" "/doc$" "/package$" "/test$")))
```

Then set up the autocomplete for your project with the command M-x
`set-header-autocomplete`. It should work for any subdirectory within your
project or within any path mentionned in the compile includes file. You can
display the list of include directories it uses with M-x
`show-header-autocomplete`.

Note that currently the setting is not saved and will be lost any time you
restart Emacs.

## Configuration profiling

M-x `emacs-init-time` shows the time Emacs took to start. You can profile the
configuration using this command (this example is for OS X):

```bash
$ Applications/Emacs.app/Contents/MacOS/Emacs -Q -l ~/.emacs.d/extensions/profile-dotemacs.el -f profile-dotemacs

```
