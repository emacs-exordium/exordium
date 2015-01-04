# What is this repo

Something you probably don't care about. This is my portable Emacs
configuration, synchronized between all my machines.  It is only intended to
work with Emacs 24 running on Linux and OS X, including in -nw mode. Here is
[what it looks like](https://github.com/philippe-grenet/dot.emacs/blob/master/doc/gallery.md).

I mostly care about adding IDE-level features to Emacs for the programming
languages I use: C++, JavaScript, Python, Ruby and various Lisps. If
you are looking for a good generic Emacs configuration to start with, you might
want to check these links:
* [Emacs Prelude](https://github.com/bbatsov/prelude);
* [Steve Purcell's config](https://github.com/purcell/emacs.d);
* [Awesome Emacs](https://github.com/emacs-tw/awesome-emacs).

## Content

Programming:

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

* Markdown: [markdown-mode](http://jblevins.org/projects/markdown-mode/).
* Org: [org-mode](http://orgmode.org).
* [IDO](http://www.emacswiki.org/emacs/InteractivelyDoThings) (built-in).
* [Helm](http://tuhdo.github.io/helm-intro.html) an equivalent of IDO.
* [Projectile](http://batsov.com/projectile) a project-based file management
  tool.
* [Windmove](http://www.emacswiki.org/emacs/WindMove): move between windows
  with Shift-Meta-arrows.
* [Expand Region](https://github.com/magnars/expand-region.el): increase
  selected region by semantic units.
* [Fill Column Indicator](http://www.emacswiki.org/emacs/FillColumnIndicator):
  80 character column marker.
* [Project Explorer](https://github.com/sabof/project-explorer): directory
  tree.
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

Keybinding         | Description
-------------------|-----------------------------------------------------------
<kbd>C-c g</bkd>   | Open git status mode (`magit-status`).

### Helm / Projectile

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

Keybinding          | Description
--------------------|-----------------------------------------------------------
<kbd>C-c h</kbd>    | Helm: find file in any project
<kbd>C-c p p</kbd>  | IDO: switch project
<kbd>C-c p f</kbd>  | IDO: find file in current project
<kbd>C-c p g</kbd> or <kbd>C-c p s g</kbd> | Search in project with grep
<kbd>C-c p s a</kbd> | Search in project with ack
<kbd>C-c p r</kbd>   | Interactive query-replace on all files in project

See [Projectile](https://github.com/bbatsov/projectile) doc for other keys.

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
<kbd>C-&gt;</kbd>  | Right-align end-of-line comment or text after cursor.
<kbd>C-c i</kbd>   | Insert redundant #include guard.
<kbd>C-c =</kbd>   | Insert class definition header.
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

*This is a work in progress.*

Set the variable `*init-rtags-auto-complete*` to true in your
`init-local-prefs.el` to turn on RTags as the source for auto-complete in C/C++
mode. It will work when you activate the diagnostics mode with <kbd>C-c r
D</kbd>. Note that this variable makes RTags be the *only* source for
auto-complete in C/C++ mode, e.g. all other classic sources such as names in
open buffers are disabled. The reasoning being that surely Clang must be more
accurate.

**Issues:** auto-complete triggers rdm very often. Also, since auto-complete is
linked to the diagnostics mode, it ends up displaying errors in the code you
are typing all the time, which may be distracting because the RTags
error/warning/quick-fix faces are intentionally very loud.

## Header file autocomplete

*This is a work in progress.*

This module sets up autocomplete for `#include` header files in C++ mode. It
reuses the file `compile_includes` mentioned above. It is independent of RTags
Diagnostics (e.g. you don't need to run diagnostics).

Header auto-complete currently reuses the same variables that store the
content of `compile_includes`. These variables are loaded with either:

* M-x `rtags-create-compilation-database`,
* Or M-x `rtags-load-compile-includes-file` which is faster and is in fact
  called by the former (it just reloads `compile_includes`).

After calling one of these functions, header auto--complete should work for any
C++ file you open in your project.

**Issues:** these variables are nil when Emacs starts. Also, they are not
updated if you switch the RTags project: you would need to re-run
`rtags-load-compile-includes-file` again. (This should be fixed once we have a
real concept of project).

## Configuration profiling

M-x `emacs-init-time` shows the time Emacs took to start. You can profile the
configuration using this command (this example is for OS X):

```bash
$ Applications/Emacs.app/Contents/MacOS/Emacs -Q -l ~/.emacs.d/extensions/profile-dotemacs.el -f profile-dotemacs

```
