# What is this repo

A portable Emacs configuration focused on programming with C++, Lisp and and
scripting languages (JavaScript, Python and Ruby). It is only intended to work
with Emacs 24 running on Linux and OS X, including in -nw mode. It is called
"init" (because I lack imagination). Here is
[what it looks like](https://github.com/philippe-grenet/dot.emacs/blob/master/doc/gallery.md).

If you are looking for a good generic Emacs configuration to start with, you
might want to check these links:
[Emacs Prelude](https://github.com/bbatsov/prelude),
[Steve Purcell's config](https://github.com/purcell/emacs.d),
[Awesome Emacs](https://github.com/emacs-tw/awesome-emacs).

**Table Of Contents**

* [Packages](#packages)
* [Installation](#installation)
* [Modules](#modules)
* [Customization](#customization)
* [Keymap](#keymap)
* [Helm and Projectile](#helm-and-projectile)
* [RTags](#rtags)
* [Bugs](#bugs)
* [Configuration Profiling](#configuration-profiling)

## Packages

Programming:

* C++: [RTags](https://github.com/Andersbakken/rtags), a LLVM/Clang-based code
  indexer.
* C++: formatting keys for the C++ [BDE](https://github.com/bloomberg/bde) style.
* JavaScript: [js2-mode](http://www.emacswiki.org/emacs/Js2Mode).
* Clojure: [Cider](https://github.com/clojure-emacs/cider) and
  [Lein](http://leiningen.org).
* [Auto Complete](https://github.com/auto-complete/auto-complete).
* [YASnippet](https://github.com/capitaomorte/yasnippet), a template system.
* [Magit](http://magit.github.io).

Other:

* Markdown: [markdown-mode](http://jblevins.org/projects/markdown-mode/).
* Org: [org-mode](http://orgmode.org).
* [IDO](http://www.emacswiki.org/emacs/InteractivelyDoThings) "Interactively DO
  things", a built-in package for supercharging Emacs's completion engine when
  opening files.
* [Helm](http://tuhdo.github.io/helm-intro.html) an equivalent of IDO.
* [Projectile](http://batsov.com/projectile) a project-based file management
  tool.
* [Windmove](http://www.emacswiki.org/emacs/WindMove): move between windows
  with Meta-arrows.
* [Expand Region](https://github.com/magnars/expand-region.el): increase
  selected region by semantic units.
* [Fill Column Indicator](http://www.emacswiki.org/emacs/FillColumnIndicator):
  80 character column marker.
* [Project Explorer](https://github.com/sabof/project-explorer): directory
  tree.
* A few themes such as
  [Tomorrow Night](https://github.com/chriskempson/tomorrow-theme) and
  [PowerLine](http://www.emacswiki.org/emacs/PowerLine).

## Installation

Backup any `.emacs` file or `.emacs.d` directory you may have, then clone this
repo:

```bash
$ git clone https://github.com/philippe-grenet/dot.emacs.git ~/.emacs.d
```

Emacs will download and compile the missing packages from Melpa the first
time. It does not update them afterwards (you have to do this with M-x
`package-list-packages`).

To update the config, do M-x `update-config`. This will pull from github and
recompile everything.

## Modules

The root file is `init.el`. It looks like this:

```lisp
;;; 1. Load before-init.el if it exists
(when (file-exists-p "~/.emacs.d/before-init.el")
  (load "~/.emacs.d/before-init.el"))

;;; 2. Define the list of Melpa packages that we need, and load any missing
;;; one. Note that they are NOT updated automatically.

;;; 3. Local preferences: load prefs.el if it exists.
(require 'init-prefs)       ; defines variables that prefs.el can override
(when (file-exists-p "~/.emacs.d/prefs.el")
  (load "~/.emacs.d/prefs.el"))

;;; 4. Load the "modules" in ~/.emacs.d/modules. See below.

;;; 5. Load the default theme in ~/.emacs.d/themes.

;;; 6. Load after-init.el if it exists
(when (file-exists-p "~/.emacs.d/after-init.el")
  (load "~/.emacs.d/after-init.el"))
```

Modules can be individually commented out if needed:

```lisp
;;; Uncomment the modules you'd like to use and restart Emacs afterwards,
;;; or evaluate the require expression with M-C-x.

;;; Look and feel
(require 'init-look-and-feel)   ; fonts, UI, keybindings, saving files etc.
(require 'init-linum)           ; line numbers

;;; Usability
(require 'init-util)            ; utilities like match paren, bookmarks...
(require 'init-ido)             ; supercharged completion engine
(require 'init-autocomplete)    ; auto-completion
(when *init-helm-projectile*    ; find files anywhere in project
  (require 'init-helm-projectile))

;;; Magit and git gutter
(require 'init-git)

;;; Themes
(if *environment-nw*
    (set-face-background 'highlight nil)
  ;; Using Emacs with GUI:
  (require 'init-themes)
  (require 'init-powerline))

;;; Shell mode
(require 'init-shell)

;;; Major modes
(require 'init-markdown)
(require 'init-org)
(require 'init-xml)

;;; OS-specific things
(when *environment-osx*
  (require 'init-osx))

;;; Etc.
```

If you are looking for a specific feature or key binding,
[this page](https://github.com/philippe-grenet/dot.emacs/blob/master/doc/code-organization.md)
explains the code organization. Each module starts with a commentary including
all key bindings.

## Customization

3 files can be added in your directory `~/.emacs.d` to customize the
configuration for the local machine (they are ignored in git):

File name        | Usage
-----------------|-------------------------------------------------------
`prefs.el`       | Loaded before any module. Use it to override fonts, window size etc. See [init-prefs.el](https://raw.github.com/philippe-grenet/dot.emacs/master/modules/init-prefs.el).
`before-init.el` | Loaded before anything else. Use it to set up the http proxy for instance.
`after-init.el`  | Loaded after everything else. Use it to load machine-specific extensions.

`modules/init-prefs.el` defines the preferences that can be changed in your
`prefs.el`. For example, your `prefs.el` could contain:

```lisp
;; Fonts + size in order of preference. First available one will be picked.
(setq *init-prefered-fonts* '("Monospace" . 120) ("Mono" . 120))

;; Default Emacs frame size in chars
(setq *init-preferred-frame-width* 120
*init-preferred-frame-height* 65)

;; Show line numbers (default t)
(setq *init-display-line-numbers* t)

;; Highlight current line (default t)
(setq *init-line-mode* t)

;; Don't set ESC key to C-g (quit/abort)
(setq *init-keyboard-escape* nil)

;; Disable electric-pair (automatically inserts a closing parenthese,
;; curly brace, etc.)
(setq *init-enable-electric-pair-mode* nil)

;; Available themes (default tomorrow-night):
;; - tomorrow-night, tomorrow-night-bright, tomorrow-night-blue,
;;   tomorrow-night-eighties, tomorrow-day
;; - solarized-dark, solarized-light
;; - monokai
(setq *init-theme* 'solarized-light)

;; Don't use powerline (may cause Emacs to crash on startup sometimes)
(setq *init-enable-powerline* nil)
```

There are more options, see
[init-prefs.el](https://raw.github.com/philippe-grenet/dot.emacs/master/modules/init-prefs.el).

## keymap

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
<kbd>M-ARROW</kbd>   | Move the focus between visible buffers (faster than <kbd>C-x o</kbd>).

Editing any file:

Keybinding          | Description
--------------------|----------------------------------------------------------
<kbd>RETURN</kbd>   | Return and indent by defaut; use <kbd>S-RETURN</kbd> for just return.
<kbd>M-BCKSP</kbd>  | `backward-kill-word` (e.g. the opposite of <kbd>M-d</kbd> `kill-word`).
<kbd>C-\\</kbd>     | Delete spaces after cursor (`delete-horizontal-space-forward`).
<kbd>C-BCKSP</kbd>  | Delete spaces before cursor (`delete-horizontal-space-backward`).
<kbd>M-\\</kbd>     | Delete all spaces around cursor.
<kbd>C-c d</kbd>    | Duplicate line.
<kbd>C-=</kbd>      | Expand region.
<kbd>C-&#124;</kbd> | Toggle 80-column ruler (fill column indicator).

Navigation:

Keybinding          | Description
--------------------|----------------------------------------------------------
<kbd>C-x C-\\</kbd> | Goto last change in buffer. Repeat to go to the second most recent edit, etc.
<kbd>C-x C-&#124;</kbd> | Goto last change in reverse direction (e.g. add shift).
<kbd>C-c s</kbd>    | Push point onto position stack (e.g. bookmarks).
<kbd>C-c b</kbd>    | Pop point from position stack.

Project Explorer:

Keybinding          | Description
--------------------|----------------------------------------------------------
<kbd>C-c e</kbd>    | Open project explorer on the left side. <kbd>q</kbd> to quit. <kbd>s</kbd> to change directory. <kbd>TAB</kbd> to toggle folding, <kbd>S-TAB</kbd> to fold all. <kbd>RETURN</kbd> open file. <kbd>w</kbd> Show path and copy it to clipboard.

Auto-complete:

Keybinding         | Description
-------------------|-----------------------------------------------------------
<kbd>C-.</bkd>     | Force trigger auto-complete.
<kbd>ESC</kbd>     | Abort auto-complete.

Git:

Keybinding            | Description
----------------------|-----------------------------------------------------------
<kbd>C-c g s</bkd>    | Open git status mode (`magit-status`).
<kbd>C-c g l</bkd>    | Open git log mode (`magit-log`).
<kbd>C-c g down</bkd> | Goto next hunk in buffer.
<kbd>C-c g up</bkd>   | Goto previous hunk in buffer.
<kbd>C-c g d</bkd>    | Diff the current hunk.
<kbd>C-c g r</bkd>    | Revert the current hunk (ask for confirmation before)

### C++

Keybinding         | Description
-------------------|-----------------------------------------------------------
<kbd>C-TAB</kbd>   | Alternate between header file and source file.
<kbd>C-c ;</kbd>   | Rename variable under cursor (non-RTags).

[BDE](https://github.com/bloomberg/bde) style:

Keybinding         | Description
-------------------|-----------------------------------------------------------
<kbd>C-c a</kbd>   | Align function arguments (in signature).
<kbd>C-c f</kbd>   | Align function arguments (in function call).
<kbd>C-c m</kbd>   | Align class members (region must be selected).
<kbd>C-&gt;</kbd>  | Right-align end-of-line comment or text after cursor.
<kbd>C-c i</kbd>   | Insert redundant #include guard.
<kbd>C-c =</kbd>   | Insert class definition header.
<kbd>C-c -</kbd>   | Insert class implementation header.

### Snippets

YASnippet is only enabled for C++ mode currently. Snippets are stored in
`~/.emacs.d/snippets/c++-mode` and the trigger key is <kbd>C-c y</kbd>. Here are
[the snippets](https://github.com/philippe-grenet/dot.emacs/blob/master/doc/snippets.md).

Note that variable `*bde-component-author*` defines the default author for a
header file template (see `modules/init-yasnippet.el`).

## Helm and Projectile

Projectile allows for defining "projects" e.g. collections of files. Once a
project is defined it provides many keys to find files, grep in all files
etc. Projectile maintains an index of files for each project it knows about;
this list is created by scanning the project root directory. The main usage is
to jump to a file using a partial name without having to remember in which
directory it is, but it also supports grep/ack and replace in
project. Projectile works with Helm or IDO, so you can use either one with
different keys.

A `.git` repo or a `.projectile` file defines the root directory of a
project. Even if you have a git repo, you can create a `.projectile` file at
the root of your project: it allows for filtering out the files you don't care
about, such as binaries, scripts etc. For example, suppose you have a workspace
directory containing among other things the BDE library and a project "bar";
you could create `.projectile` file like this:

```
+/bde/groups
+/bar
-.*
-*.md
-*.cap
-*.mem
-*.dep
-*.opts
-*.defs
```

The plus sign is used to ignore everything except specific directories;
alternatively the minus sign is used to indicate what directories to ignore,
starting at the root of the project. The minus sign can also ignore file
patterns. If both directories to keep and ignore are specified, the directories
to keep first apply, restricting what files are considered. The paths and
patterns to ignore are then applied to that set. Refer to the
[Projectile](https://github.com/bbatsov/projectile) documentation for details.

Now you need to teach Projectile where your projects are. You can do that by:

1\. Simply opening the project's root dir in dired, and then pretending to
  search a file with Helm (<kbd>C-c h</kbd>), or switching project with IDO
  (<kbd>C-c p p</kbd>). The top of the Helm buffer should show the list of
  projects including yours.

2\. Sometimes method 1 works for the current session, but then Projectile
  forgets your projects as soon as you restart Emacs. In this case try to
  restart Emacs in the project's root directory for Projectile to find it. You
  should only need to do this one time for each project, after that it is
  cached.

3\. The brute force method which is guaranteed to work is to edit the file
  where Projectile saves the list of projects. This file is
  `~/.emacs.d/projectile-bookmarks.eld`. Since it is constantly written by
  Emacs itself, you need to exit emacs and restart it with `emacs -Q` (so that
  Projectile does not run). Edit the file, save and restart Emacs
  normally. Here is my bookmark file:

```
("/bb/mbig7/mbig2387/workspaces/rsp/" "/bb/mbig7/mbig2387/workspaces/si-core/" "/home12/pgrenet/.emacs.d/" "/bb/mbig7/mbig2387/workspaces/bsl-internal/" "/bb/mbig7/mbig2387/workspaces/bde-core/")
```

To open a file with Projectile using Helm, type <kbd>C-c h</kbd>. This will
display the Helm buffer, displaying initially just the list of projects at the
top. Choose your project with the arrow keys and press enter; Helm will now
display all indexed files in the project. Start typing a partial name to narrow
the selection until you find what you were looking for.

<kbd>C-c p C-h</kbd> displays the list of keys for Projectile. Below are the
most important ones.

Keybinding           | Description
---------------------|-----------------------------------------------------------
<kbd>C-c h</kbd>     | Helm: find file in current project
<kbd>C-c C-h</kbd>   | Same but first select project
<kbd>C-c p p</kbd>   | IDO: switch project
<kbd>C-c p f</kbd>   | IDO: find file in current project
<kbd>C-c p s g</kbd> | Grep in current project
<kbd>C-c p s a</kbd> | Same but using ack
<kbd>C-c p r</kbd>   | Interactive query-replace on all files in project

See [Projectile](https://github.com/bbatsov/projectile) doc for other keys.

## RTags

### Start rdm

[RTags](https://github.com/Andersbakken/rtags) is a LLVM-based C++ indexer
which provides a deamon called "rdm" that maintains an in-memory index, and a
command-line client called "rc". RTags uses a single index for all symbols, but
it allows for loading and unloading projects individually.

To use it, first start the deamon:

```bash
$ rdm
```

This will start the deamon on the foreground, using a number of concurrent "rp"
jobs that is function of the number of CPUs on the local machine. It starts by
reading the saved project indices in `~/.rtags` if any. By default it logs to
the console but you can make it log to a file instead with `-L file` or make it
silent with `-S`. There are many options; use `--help` to see the list.  You
can also create a file `~/.rdmrc` containing the default command line
arguments.

Alternatively you can run rdm as an Emacs subprocess: M-x `rtags-start-rdm`,
with logs going into a buffer. Stop it with M-x `rtags-quit-rdm`.

### Controlling rdm

rdm stores project indices into a directory `~/.rtags` and reloads them as
needed. rc and rdm communicate with each other using a socket file `~/.rdm`.

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

Navigation keys:

Keybinding                           | Description
-------------------------------------|-----------------------------------------
<kbd>M-.</kbd>, <kbd>C-c r .</kbd>   | Jump to symbol definition.
<kbd>M-,</kbd>, <kbd>C-c r ,</kbd>   | Find references to symbol.
<kbd>C-c r &gt;</kbd>                | Find symbol (prompts for symbol name).
<kbd>C-c r &lt;</kbd>                | Find references (prompts for symbol name).
<kbd>C-c r v</kbd>                   | Find all implementations of virtual function.
<kbd>M-C-g</kbd>                     | Find symbol in file usign Helm.
<kbd>C-c r T</kbd>                   | Display tag list in a window on the left side.
<kbd>C-c r ;</kbd>                   | `rtags-find-file` using partial name (non IDO).

Any navigation is recorded onto a stack, so it is easy to go back and forth:

Keybinding                          | Description
------------------------------------|---------------------------------------
<kbd>C-{</kbd>, <kbd>C-c r [</kbd>  | Go back to previous location.
<kbd>C-}</kbd>, <kbd>C-c r ]</kbd>  | Go forward to next location.

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

Keybinding            | Description
----------------------|-----------------------------------------------------------
<kbd>C-c r D</kbd>    | Run `rtags-diagnostics` if it wasn't and force reparsing of current buffer.
<kbd>C-c r d</kbd>    | Show/hide the diagnostics buffer without force reparsing.
<kbd>C-c r DOWN</kbd> | Goto next problem.
<kbd>C-c r UP</kbd>   | Goto previous problem.
<kbd>C-c r F</kbd>    | Fix the error using Clang's "did you mean" (try it with "inft x;")
<kbd>C-c r Q</kbd>    | `rtags-stop-diagnostics` stop the async process.

Other functions:
* `rtags-next-diag` goes to the next problem.
* `rtags-clear-diagnostics` clears any error or warning overlay.

### Autocomplete

You can use RTags as source for auto-complete suggestions. Notes:
* This feature makes RTags be the *only* source for auto-complete in C/C++
  mode, e.g. all other classic sources such as names in open buffers are
  disabled. The reasoning being that surely Clang must be more accurate.
* This feature requires RTags diagnostics to be turned on.

To enable it automatically, set the variable `*init-rtags-auto-complete*` to
true in your `prefs.el`. Note that auto-complete for `#include` header files
does not work in this case, because it does not know what project you are in.

To enable it manually, type <kbd>C-c r A</kbd>. This will take effect for all
C++ files you open from that point. This key also sets auto-complete for the
`#include` header files in the current project.

Possible issues:
* There might be a graphical glitch in the auto-complete popup if the Emacs
  window is too small. Just enlarge the window a bit if this happens.
* It's a tiny bit slow and it may trigger rdm a bit often.
* Auto-complete for header files does not understand when you are switching
  project.

## Bugs

* Powerline may cause Emacs to crash on startup because of a race condition
  inside Emacs. One trick to fix it is to make powerline be the last thing you
  enable in your config. For this, add `(setq *init-enabled-powerline* nil)` in
  your `pref.el`, and add `(require 'init-powerline)` in your
  `after-init.el`. If this still does not work, keep Powerline disabled and add
  this function in your `after-init.el`:

```lisp
(defun powerline ()
  "Enable powerline. On some platforms you may have to click somewhere
to make it display"
  (interactive)
  (require 'init-powerline)
  (redraw-display))
```

* Sometimes a random bug may occur that displays this error:
  `fringe-helper-modification-func: Invalid search bound (wrong side of
  point)`. I'm pretty sure this is a bug in `git-gutter-fringe` which display
  git diff icons in the left-side fringe. There are two ways to work around it:
  either add `(setq *init-git-gutter* nil)` if your `prefs.el` to disable this
  feature entirely, or add `(setq *init-git-gutter-non-fringe* t)` in your
  `prefs.el` to display git diffs on the left side of line numbers, e.g. not in
  the fringe. Note that the latter disables the highighting of the current line
  number for now.

## Configuration profiling

M-x `emacs-init-time` shows the time Emacs took to start. You can profile the
configuration using this command (this example is for OS X):

```bash
$ Applications/Emacs.app/Contents/MacOS/Emacs -Q -l ~/.emacs.d/extensions/profile-dotemacs.el -f profile-dotemacs

```
