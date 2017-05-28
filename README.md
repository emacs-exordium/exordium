![Exordium](https://raw.github.com/philippe-grenet/exordium/master/doc/Exordium.png)

# What is this repo

A portable Emacs configuration focused on adding IDE-level features for C++ and
Lisp/Clojure programming. It is only intended to work with Emacs version 24.4
and above on Linux and OS X including in -nw mode, but it should work on
Windows as well. It is modular and customizable. It is not a starter kit, it is
a hacker kit.

If you are looking for a good generic Emacs configuration to start with, you
might want to check these links:
[Emacs Prelude](https://github.com/bbatsov/prelude),
[Steve Purcell's config](https://github.com/purcell/emacs.d),
[Spacemacs](https://github.com/syl20bnr/spacemacs),
[Awesome Emacs](https://github.com/emacs-tw/awesome-emacs).

**Table Of Contents**

* [Features](#features)
* [Installation](#installation)
* [Keymap](#keymap)
* [Projectile](#projectile)
* [Git](#git)
* C++
  * [Utilities](#utilities)
  * [Snippets](#snippets)
  * [RTags](#rtags)
* [Lisp](#lisp)
* [Markdown](#markdown)
* [Customization](#customization)
* [Troubleshooting](#troubleshooting)

## Features

* Usability: [IDO](http://www.emacswiki.org/emacs/InteractivelyDoThings)
  (completion engine, turned on by default);
  [Helm](http://tuhdo.github.io/helm-intro.html) (an alternative to IDO);
  [Auto Complete](https://github.com/auto-complete/auto-complete) and
  [Company](http://http://company-mode.github.io) (completion engines)
  [Expand Region](https://github.com/magnars/expand-region.el) (increase
  selected region by semantic units);
  [Fill Column Indicator](http://www.emacswiki.org/emacs/FillColumnIndicator)
  (80-character column marker);
  [Project Explorer](https://github.com/sabof/project-explorer) (directory
  tree); [Avy](https://github.com/abo-abo/avy) (jump to visible text in 2 or 3
  key-strokes); [ace-window](https://github.com/abo-abo/ace-window) (quick jump
  between windows).
* Projects: [Projectile](http://batsov.com/projectile) (project-based file
  management tool).
* Git: [Magit](http://magit.github.io) (git UI);
  [Git Gutter](https://github.com/syohex/emacs-git-gutter) (diffs in buffer).
* C++:
  * [RTags](https://github.com/Andersbakken/rtags): a LLVM/Clang-based code
    indexer providing goto definition, find references, refactoring,
    compilation errors in buffer, auto-complete etc.
  * Formatting keys and snippets for the
    [BDE](https://github.com/bloomberg/bde) code style.
  * [include-what-you-use](https://include-what-you-use.org): a LLVM/Clang-based
    tool for use with clang to analyze #includes in C and C++ source files.
* JavaScript: [JS2-mode](http://www.emacswiki.org/emacs/Js2Mode).
* Clojure: [Cider](https://github.com/clojure-emacs/cider) and
  [Lein](http://leiningen.org).
* Eye candy: a few themes that do not look like an "angry fruit salad", and
  [PowerLine](http://www.emacswiki.org/emacs/PowerLine).

## Installation

Backup any `.emacs` file or `.emacs.d` directory you may have, and then clone
this repo:

```bash
$ git clone https://github.com/philippe-grenet/exordium.git ~/.emacs.d
```

The first time you start Emacs it will download and compile the required
packages, which may take a couple of minutes. If your machine is behind a proxy
server, you should create a file `.emacs.d/before-init.el` with the address of
the proxy before you start Emacs:

```lisp
(setq url-proxy-services
      '(("no_proxy" . "^\\(localhost\\|10.*\\)")
        ("http"     . "<proxy-name>:<proxy-port>")
        ("https"    . "<proxy-name>:<proxy-port>")))
```

You can also clone the repository elsewhere, and use the `init-test.sh` script
to try out exordium. `init-test.sh` will run your default emacs with the
current directory as your user-emacs-directory, loading `init.el`, and no other
init files.

### Updates

To update Exordium, do <kbd>M-x update-config</kbd>. This command pulls from
Github and recompiles everything. Restart Emacs after that.

Melpa packages are not updated automatically: you can do it with <kbd>M-x
package-list-packages</kbd>, then <kbd>U</kbd> then <kbd>x</kbd>.

### Files

The main file is `init.el`: it load the modules from the `modules` subdirectory
and the default theme from the `themes` subdirectory. The `extensions`
subdirectory is used for third-party plugins that are not available in Melpa.

3 files can be added in your directory `~/.emacs.d` to customize the
configuration for the local machine (these files are not tracked by git):

File name        | Usage
-----------------|-------------------------------------------------------
`prefs.el`       | Loaded before any module. The module [init-prefs.el](https://github.com/philippe-grenet/exordium/blob/master/modules/init-prefs.el) defines a number of customization variables for fonts, theme etc. `prefs.el` is where you should override any of these variables.
`before-init.el` | Loaded before anything else. Use it to set up the HTTP proxy for instance.
`after-init.el`  | Loaded after everything else. This is where you should add your own features.

You can also have local modules in a directory `~/.emacs.d/local`.

See the [Customization](#customization) section below for more details.

## Keymap

General:

Keybinding           | Description
---------------------|---------------------------------------------------------
<kbd>C-z</kbd>       | Undo! (`undo`).
<kbd>C-\`</kbd>      | `kill-this-buffer` (faster than <kbd>C-x k</kbd>).
<kbd>C-x C-r</kbd>   | Open recent file (completes open file with <kbd>C-x C-f</kbd>).
<kbd>M-g</kbd>       | `goto-line` (prompts for a line number).
<kbd>C-+</kbd>       | Increase font size (`text-scale-increase`).
<kbd>C--</kbd>       | Decrease font size (`text-scale-decrease`).
<kbd>M-C-l</kbd>     | Switch back and forth between the 2 top buffers (from XEmacs).
<kbd>C-c C-SPC</kbd> | Toggle highlight of the symbol under the cursor (up to 4 different symbols using different colors).

Editing:

Keybinding          | Description
--------------------|----------------------------------------------------------
<kbd>RETURN</kbd>   | Return and indent by default; use <kbd>S-RETURN</kbd> for just return.
<kbd>M-BCKSP</kbd>  | `backward-kill-word` (e.g. the opposite of <kbd>M-d</kbd> `kill-word`).
<kbd>C-\\</kbd>     | Delete spaces after cursor (`delete-horizontal-space-forward`).
<kbd>C-BCKSP</kbd>  | Delete spaces before cursor (`delete-horizontal-space-backward`).
<kbd>M-\\</kbd>     | Delete all spaces around cursor.
<kbd>M-LEFT</kbd> and <kbd>M-RIGHT</kbd> | Move cursor by semantic units (use <kbd>C-LEFT</kbd> and <kbd>C-RIGHT</kbd> to move by words).
<kbd>C-c d</kbd>    | Duplicate line.
<kbd>C-=</kbd>      | Expand region by semantic units.
<kbd>M-C-=</kbd>    | Contract region by semantic units.
<kbd>C-&#124;</kbd> | Toggle the 80-column ruler (fill column indicator).

Navigation:

Keybinding                         | Description
-----------------------------------|----------------------------------------------------------
<kbd>C-x C-\\</kbd>                | Goto last change in buffer. Repeat to go to the second most recent edit, etc.
<kbd>C-x C-/</kbd>                 | Goto last change in reverse direction.
<kbd>C-c j</kbd> or <kbd>C-'</kbd> | Goto visible word or subword (`avy-goto-word-or-subword-1`). It first asks for the first character of the word, then annotates all words starting with that character with a unique touch-type friendly code.
<kbd>C-c s</kbd>                   | Push point onto position stack (e.g. bookmarks).
<kbd>C-c b</kbd>                   | Pop point from position stack.

Window manager:

Keybinding              | Description
------------------------|----------------------------------------------------------
<kbd>C-c ARROW</kbd>    | Move cursor between windows.
<kbd>C-c S-ARROW</kbd>  | Move the windows themselves.
<kbd>M-p NUMBER</kbd>   | Jump to the specified window number using [ace-window](https://github.com/abo-abo/ace-window). If you only have 2 windows, cycle between them.

Auto-complete/Company:

Keybinding         | Description
-------------------|-----------------------------------------------------------
<kbd>C-.</kbd>     | Force trigger auto-complete/company-complete.
<kbd>ESC</kbd>     | Abort auto-complete/company-complete.

Tip: if you are looking for a particular key and you know it starts with a
given prefix, type the prefix followed by <kbd>C-h</kbd>: Emacs will display
the list of keys starting with that prefix. For example <kbd>C-c C-h</kbd> lists
all the keys starting with <kbd>C-c</kbd>.

## Projectile

[Projectile](https://github.com/bbatsov/projectile) adds the notion of
"projects" to Emacs. It provides many keys to find files within a project, grep
in all files etc. Projectile maintains an index of files for each project it
knows about; this list is created by scanning the project root directory. The
main usage is to jump to a file using a partial name without having to remember
in which directory it is, but it also supports grep/ack and replace in
project. Projectile works with Helm or IDO, so you can use either one with
different keys. Alternatively, you can always use Helm by setting
`exordium-helm-everywhere` to true.

Here is an example: <kbd>C-c h</kbd> shows the list of buffers and files in the
current project using Helm; to find a file you just need to type a few letters and the
list shrinks as it performs fuzzy matching:

![Helm](https://raw.github.com/philippe-grenet/exordium/master/doc/helm.png)

### Setting up projects

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

1\. Simply opening the project's root dir in Dired, and then pretending to
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

### Using Projectile

The name of the current project is displayed in the mode line, between square
brackets. There are only 2 keys to remember:

* To open a file in the current project using Helm, type <kbd>C-c h</kbd>. This
  will display the Helm buffer. Start typing for a partial name to narrow the
  selection until you find what you were looking for. Note that it performs
  fuzzy matching.
* If you want to open a file from a different project, type <kbd>C-c M-h</kbd>
  instead. The Helm buffer will initially display just the list of
  projects. Choose your project and press enter; Helm will now display all
  indexed files in that project.

<kbd>C-c p C-h</kbd> displays the list of keys for Projectile. Below are the
most important ones.

Keybinding                             | Description
---------------------------------------|--------------------------------------------------
<kbd>C-c h</kbd>                       | Find file in current project with helm
<kbd>C-c M-h</kbd> or <kbd>C-c H</kbd> | Same, but first select project
<kbd>C-c p p</kbd>                     | IDO: switch project (alternative: Helm)
<kbd>C-c p f</kbd>                     | IDO: find file in current project (alternative Helm)
<kbd>C-c p s g</kbd>                   | Grep in current project
<kbd>C-c p s a</kbd>                   | Same but using ack
<kbd>C-c p r</kbd>                     | Interactive query-replace on all files in project
<kbd>C-c p i</kbd>                     | Invalidate the cache

See [Projectile](https://github.com/bbatsov/projectile) documentation for other
keys.

### Project Explorer

Projectile is linked with
[Project Explorer](https://github.com/sabof/project-explorer) which displays
the project directory structure on the left side:

![Project Explorer](https://raw.github.com/philippe-grenet/exordium/master/doc/project_explorer.png)

Keybinding          | Description
--------------------|----------------------------------------------------------
<kbd>C-c e</kbd>    | Open project explorer on the left side.

With the cursor in the Project Explorer window, you can use these keys:
<kbd>q</kbd> to quit. <kbd>s</kbd> to change directory. <kbd>TAB</kbd> to
toggle folding, <kbd>S-TAB</kbd> to fold all. <kbd>RETURN</kbd> open
file. <kbd>w</kbd> Show path and copy it to clipboard.

### Helm

Helm can be set up as a primary completion and selection narrowing framework
for most commonly used functions. You can achieve that by setting
`exordium-helm-everywhere` to true. The following keys will use Helm:

Keybinding          | Description
--------------------|----------------------------------------------------------
<kbd>C-c p p</kbd>  | Select project and open file with projectile.
<kbd>C-c p f</kbd>  | Open file with projectile.
<kbd>C-x C-r</kbd>  | Open recent file.
<kbd>M-x</kbd>      | Execute command.
<kbd>M-y</kbd>      | Select yank pop.
<kbd>C-x b</kbd>    | Switch buffer.
<kbd>C-x C-f</kbd>  | Find file.

#### Other Helm tools

Helm is a pretty good when you need quickly scan search results. The commands below
will start different search modes. By default, they will use symbol under the point.
However if it is not there just start typing text: the Helm window shows all
matching lines, and you can jump from one to another using the arrow keys.

Some of them will use  [Helm Swoop](https://github.com/ShingoFukuyama/helm-swoop) while
the reminder will use [Silver Searcher](https://github.com/ggreer/the_silver_searcher).
The latter, abbreviated `Ag`, being substitute to `grep` and `ack` has support for regular
expressions.

* <kbd>C-S-a</kbd>: Ag search for text in current projectile project.
* <kbd>C-S-s</kbd> or <kbd>M-x helm-swoop</kbd>: Swoop search for text in current buffer.
* <kbd>C-S-d</kbd>: Ag search for text, but ask for directory to start first.
* <kbd>C-S-f</kbd>: Ag search for text in current buffer (similar to Swoop).
* <kbd>C-S-r</kbd>: Ag search starting from project root.
* <kbd>M-x helm-multiple-swoop-all</kbd>: Swoop search within all buffers.

## Git

All git-related keys use prefix <kbd>C-c g</kbd> plus one more key. For example
<kbd>C-c g s</kbd> runs [Magit](http://magit.github.io) status:

![magit](https://raw.github.com/philippe-grenet/exordium/master/doc/magit.png)

The bottom window shows the current git status. Use the <kbd>tab</kbd> key on
any file to fold or unfold its diff. Use the <kbd>s</kbd> key to stage or
unstage a file, and the capital <kbd>S</kbd> to stage all of them. Use the
<kbd>k</kbd> key to revert a file. Type <kbd>c</kbd> twice to commit; it will
ask for the commit message (<kbd>C-c C-c</kbd> to close the window and
commit). Finally the <kbd>q</kbd> key quits magit. There are a lot of other
keys which are described [here](http://daemianmack.com/magit-cheatsheet.html).

The screenshot above also shows
[git gutter](https://github.com/syohex/emacs-git-gutter) in the top buffer. Git
gutter displays a git diff of the file in the left-side fringe (you can
[customize](#customization) it). Git gutter defines a few keys for navigating
between hunks, diffing and reverting.

Magit keys:

Keybinding            | Description
----------------------|-----------------------------------------------------------
<kbd>C-c g s</kbd>    | Open git status (`magit-status`).
<kbd>C-c g l</kbd>    | Open git log (`magit-log`).
<kbd>C-c g f</kbd>    | Open git file log (`magit-file-log`).
<kbd>C-c g b</kbd>    | Toggles git blame mode on and off (`magit-blame-mode`).
<kbd>C-c g g</kbd>    | Git grep (`vc-git-grep`).

Git gutter keys:

Keybinding            | Description
----------------------|-----------------------------------------------------------
<kbd>C-c g down</kbd> | Goto next hunk in buffer (`git-gutter:next-hunk`).
<kbd>C-c g up</kbd>   | Goto previous hunk in buffer (`git-gutter:previous-hunk`).
<kbd>C-c g d</kbd>    | Diff the current hunk (`git-gutter:popup-diff`).
<kbd>C-c g r</kbd>    | Revert the current hunk after confirmation (`git-gutter:revert-hunk`).

Git Timemachine key   | Description
----------------------|-----------------------------------------------------------
<kbd>C-c g t</kbd>    | Enter the git time machine (`git-timemachine-toggle`)

## C++

### Utilities

Keybinding           | Description
---------------------|-----------------------------------------------------------
<kbd>C-TAB</kbd>     | Alternate between header file and source file.
<kbd>C-u C-TAB</kbd> | Alternate between source/header file and BDE test driver.
<kbd>C-c ;</kbd>     | Rename variable under cursor (but see also RTags, which is a better solution).

Keys for formatting code according to the
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

[YASnippet](https://github.com/capitaomorte/yasnippet) is a template system
which replaces a keyword by a template after you hit the trigger key. YASnippet
is only enabled for C++ mode currently. The trigger key is set to <kbd>C-c
y</kbd> because the default TAB key is already way overused between intention
and auto-complete/company-complete. You can easily use a function key if you prefer
by adding this in your `after-init.el`:

```lisp
(define-key yas-minor-mode-map (kbd "<f2>") 'yas-expand)
```

Snippets are stored in `~/.emacs.d/snippets/c++-mode`. Here are
[the snippets](https://github.com/philippe-grenet/exordium/blob/master/doc/snippets.md).

Note that variable `*bde-component-author*` defines the default author for a
header file template (see `modules/init-yasnippet.el`). You can set it to your
name in `after-init.el`.

### RTags

[RTags](https://github.com/Andersbakken/rtags) is a LLVM-based C++ indexer
which provides a daemon called "rdm" that maintains a persistent (memory
mapped) file-based index. The client for "rdm" is command-line client called
"rc". RTags uses a single index for all symbols, but it allows for loading and
unloading projects individually.

The rdm daemon knows how to compile your project with a CLang
[compilation database](http://clang.llvm.org/docs/JSONCompilationDatabase.html),
which is a file named `compile_commands.json`. The compilation database contains one
entry for each file to compile, like the following (simplified for clarity):

```
{ "directory": "/home/phil/workspaces/foo/",
  "command":   "/usr/bin/clang++
                -D_POSIX_PTHREAD_SEMANTICS -D_REENTRANT
                -I/home/phil/workspaces/bde/groups/bsl/bsl+stdhdrs
                -I/home/phil/workspaces/bde/groups/bsl/bslma
                -I/home/phil/workspaces/bde/groups/bsl/bsls
                -c -o bar.o bar.cpp",
   "file":      "bar.cpp" }
```

Basically the compilation database contains the list of files to compile and
the exact command to compile them. There are several ways to generate this
file:

* RTags provides compiler wrapper scripts which tell rdm to parse and index
  each compilation unit before it gets compiled. While this is the easiest way
  (all you need to do is to build), the inconvenient is that you need to build
  before you can use the latest index, and any unused header won't be indexed.
* You can build with CMake: it generates a compilation database for you each
  time you build.
* Exordium provides a command to generate the compilation database by scanning
  source directories. It requires you to write a simple text file indicating
  where these source directories are.

The first thing you need to do is to build and install RTags: refer to the
RTags documentation. The sections below explain how to use it.

#### Using RTags from the shell

First start the daemon:

```bash
$ rdm
```

This will start the daemon on the foreground, using a number of concurrent "rp"
jobs that is function of the number of CPUs on the local machine. By default it
logs to the console but you can make it log to a file instead with `-L file` or
make it silent with `-S`. There are many options; use `--help` to see the list.

RTags stores project indices into a directory `~/.rtags` by default, and
reloads them as needed. It watches for file changes using *inotify* and
refreshes the index automatically. Note that you can change the location of the
`.rtags` directory with a `~/.rdmrc` file; it is recommended to store it into a
local SSD drive and avoid NFS-mounted directories.

By default rc and rdm communicate with each other using a socket file `~/.rdm`,
but there are other ways: refer to the RTags documentation.

The main commands are:

Command            | Description
-------------------|-----------------------------------------------------------
`rc -w`            | List projects in the index.
`rc -w proj`       | Switch to project "proj" (a regex).
`rc -W proj`       | Unload and delete project "proj".
`rc -J .`          | Reload the compilation DB from the current directory.
`rc --find-project-root /path/to/sourcefile.cpp` | Print what it determines to be the correct project root.
`rc -T sourcefile.cpp` | Say whether this file is indexed or not.
`rc -q`            | Shutdown rdm.

Note that a job may crash while trying to index a file. If it does, rdm will
retry a few times and then give up with the file it cannot parse.

#### Using RTags from Emacs

Alternatively you can run rdm as an Emacs subprocess. The logs will go into a
buffer (in color!).

Command                    | Description
---------------------------|---------------------------------------------------
<kbd>M-x rtags-start</kbd> | Start rdm and RTags diagnostics.
<kbd>M-x rtags-stop</kbd>  | Stop rdm and Rtags diagnostics.

#### CMake projects

If your project compiles with CMake, you're in luck: CMake generates this
compilation database for you every time you build. Adding this line in your
`~/.emacs.d/prefs.el` will make RTags work automagically:

```lisp
(setq exordium-rtags-cmake t)
```

In addition you may set the following variables:

* Exordium assumes that your build directory is named like `cmake.bld/<arch>`,
  relative to the project root, where `<arch>` is the `uname` of your OS. If
  this is not the case you can change it like so in `~/.emacs.d/prefs.el`:

    ```lisp
    (setq exordium-rtags-cmake-build-dir "build")
    ```

* Exordium runs rdm with no argument by default. You can add arguments by
  setting this variable in `~/.emacs.d/prefs.el`:

    ```lisp
    (setq exordium-rtags-rdm-args
          "--isystem /opt/bb/lib64/clang/3.6.2/include -DBAS_NOBBENV")
    ```

You can also specify where the build directory is using a `.rtags` file at the
root of your project with a content like `build /path/to/my/build`; it takes
precedence over `exordium-rtags-cmake-build-dir`.

Exordium will automatically detect if your project is CMake-enabled when you
open a C++ file, by looking for `CMakeLists.txt` files along the path from the
root of your project to the location of the file you open (your project must be
a git repo). If this is a CMake project, Exordium will start rdm if it is not
running, and ask rdm to index the project using the CMake-generated compilation
database in the build directory. If the project was already indexed, it is
simply reloaded and RTags commands work immediately. Otherwise rdm compiles the
index, and you can see the progress with <kbd>C-c r l</kbd> (rerun to dismiss).

If you need to add or remove components from your project, just rebuild it
(e.g. "make" in the build directory) and CMake will update the compilation
database accordingly. Because rdm watches for changes to the compilation
database file, it will pick up the changes automatically.

#### Non-CMake projects

You can generate the compilation database with the command
<kbd>M-x rtags-create-compilation-database</kbd>. But before you do, it needs
a little help: you need to tell it what `clang++` command to use to compile
any file, with all the `-I` directives that are necessary for your project.

The command uses a file `compile_includes` in the project root directory, which
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

  # Where are the source files (there could be multiple directories).
  # We will scan recursively any subdirectories that do not match any
  # 'exclude' regex.
  src .

  # What to put in -I directives (in addition to the source files above).
  # We will scan recursively any subdirectories that do not match any
  # 'exclude' regex.
  include /Users/phil/Code/cpp/include/bsl
  include /Users/phil/Code/cpp/include/bdl

  # Optional: patterns to exclude in -I directives and while looking for
  # sources. Here we explicitly don't want to index the tests subdir:
  exclude /test$

  # Optional: if any file name pattern must be excluded from the "src" files,
  use the "excludesrc" directive. For example this will exclude all test
  # drivers (extension .t.cpp):
  excludesrc \.t\.cpp$

  # Optional: -D macros, if any:
  macro BDE_BUILD_TARGET_SAFE
```

In addition, the creation of a compilation database uses these variables:

Variable                            | Description
------------------------------------|------------------------------------------
`rtags-compile-includes-base-dir  ` | Set this to your workspace path if you want to use relative paths in `compile_includes` that are not relative to the project's root directory (the default).
`rtags-clang-command-prefix  `      | Default is "/usr/bin/clang++ -Irelative" (note that RTags ignores the clang++ command because it uses libclang).
`rtags-clang-command-suffix  `      | Default is "-c -o".

Once you have created the `compile_includes` file, run the command <kbd>M-x
rtags-create-compilation-database</kbd>. It will:

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

The rdm daemon should automatically re-compile any file you edit in Emacs as
soon as you save it or otherwise touch it.

#### Using the index

While RTags uses <kbd>C-x r</kbd> as default prefix, this configuration uses
<kbd>C-c r</kbd> instead because it it less crowded. It also adds a few keys
such as <kbd>M-C-g</kbd> to display the list of symbols from the current buffer
using Helm:

![Rtags Helm](https://raw.github.com/philippe-grenet/exordium/master/doc/rtags_helm.png)

Navigation keys:

Keybinding                           | Description
-------------------------------------|-----------------------------------------
<kbd>M-.</kbd> or <kbd>C-c r .</kbd> | Jump to symbol definition. With prefix: in other window.
<kbd>M-,</kbd> or <kbd>C-c r ,</kbd> | Find references to symbol.
<kbd>C-c r &gt;</kbd>                | Find symbol (prompts for symbol name).
<kbd>C-c r &lt;</kbd>                | Find references (prompts for symbol name).
<kbd>C-c r v</kbd>                   | Find all implementations of virtual function.
<kbd>C-c r S</kbd>                   | Show symbol summary in tooltip (`rtags-display-summary`).
<kbd>M-C-g</kbd>                     | Find symbol in file using Helm.
<kbd>C-c r ;</kbd>                   | `rtags-find-file` using partial name (non IDO).

Any navigation is recorded onto a stack, so it is easy to go back and forth:

Keybinding                                   | Description
---------------------------------------------|---------------------------------
<kbd>C-c r LEFT</kbd> or <kbd>C-c r [</kbd>  | Go back to previous location.
<kbd>C-c r RIGHT</kbd> or <kbd>C-c r ]</kbd> | Go forward to next location.

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
<kbd>C-c r l</kbd> | Show/hide the rdm log buffer.
<kbd>C-c r U</kbd> | Show what rdm knows about a symbol.
<kbd>C-c r P</kbd> | Show all includes for the current file.
<kbd>C-c r T</kbd> | Show the tag list for the current file.

#### Using Flymake

"Rtags diagnostics" is a way to get compilation warnings and errors from rdm,
and display them using Flymake in buffers. It is enabled by default if you run
rdm from Emacs. Otherwise you can turn it on manually with `M-x
rtags-diagnostics` bound to <kbd>C-c r D</kbd>.

By default Powerline displays the name of the buffer in
green if the project compiles and in red if there are errors:

![RTags diagnostics](https://raw.github.com/philippe-grenet/exordium/master/doc/rtags_diagnostics.png)

Click on the highlighted symbol in your code to view the error message. Click
on the error line in the diagnostics buffer to jump to the error location.

Keybinding            | Description
----------------------|-----------------------------------------------------------
<kbd>C-c r D</kbd>    | Run `rtags-diagnostics` if it wasn't and force reparsing of current buffer.
<kbd>C-c r d</kbd>    | Show/hide the diagnostics buffer without force reparsing.
<kbd>C-c r DOWN</kbd> | Goto next problem (`rtags-next-diag`).
<kbd>C-c r UP</kbd>   | Goto previous problem.
<kbd>C-c r F</kbd>    | Fix the error using Clang's "did you mean" (try it with "inft x;")
<kbd>C-c r c</kbd>    | Clears all errors and warnings (`rtags-clear-diagnostics`)
<kbd>C-c r Q</kbd>    | `rtags-stop-diagnostics` stop the async process.

#### Autocomplete

You can use RTags as source for auto-complete suggestions. Notes:

* This feature makes RTags be the *only* source for auto-complete in C/C++
  mode, e.g. all other classic sources such as names in open buffers are
  disabled. The reasoning being that surely Clang must be more accurate.
* This feature requires RTags diagnostics to be turned on.

To enable it automatically, set the variable `exordium-rtags-auto-complete` to
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

#### Company

As an alternative to `auto-complete` you can choose `company-mode`. You can
do that by setting `exordium-complete-mode` to `:company`. It will use
RTags as a completion engine when `rdm` is started.

### include-what-you-use

[include-what-you-use](https://include-what-you-use.org) is a tool for use
with clang to analyze `#include`s in C and C++ source files. The main goal of
`include-what-you-use` is to remove superfluous `#include`s. It does this both
by figuring out what `#include`s are not actually needed for this file (for
both `.cpp` and `.h` files), and suggesting fixes to `#include`s with
forward-declares when possible. Please note that since this tool is not 100%
accurate, Exordium support does not include automatic file modification.
Instead it provides support to spawn the process and capture the suggestions
in a diagnostic buffer that can be later checked by human.

Similarly to RTags, `include-what-you-use` relies on the compilation database
to be available in `compile_commands.json` file. It uses
`exordium-rtags-cmake-build-dir` to locate the compilation database for the
current project.

On top of that two variables are available to customise the behavior:

* `exordium-iwyu-filter-args`: a list of arguments that should be taken out of
  the `include-what-you-use` invocation. This is useful, when project's
  compilation database contains arguments specific to the compiler and those
  arguments are not supported by LLVM/Clang.
* `exordium-iwyu-filter-args`: a list of arguments that should be passed to the
  `include-what-you-use` executable. This becomes useful, i.e., when LLVM/Clang
  is not installed in the system directory and extra includes has to be passed.

Exordium defines the following keybindings:

Keybinding            | Description
----------------------|-----------------------------------------------------------
<kbd>C-c w e</kbd>    | Run `include-what-you-use` for the current buffer.
<kbd>C-c w d</kbd>    | Show/hide the diagnostics buffer without force reparsing.
<kbd>g</kbd>          | Reparse recent file (in `IWYU-mode` buffer).

## Lisp

Coming soon: Emacs Lisp, Common Lisp and Clojure.

## Markdown

<kbd>M-x impatient-markdown-mode</kbd> starts a minor mode that provides a live
preview of a markdown buffer in your favorite web browser. The web page is
updated as you type (this feature is implemented with the
[impatient mode](https://github.com/skeeto/impatient-mode)). Before you can use
it, you need to set the variable `markdown-command` to the command to execute
to render a markdown file into HTML.  For example, to use the GitHub command,
clone [markup](https://github.com/github/markup) and set `markdown-command` to
the path of `bin/github-markup` in your `after-init.el`.  Other options include
Pandoc or RedCarpet.

Note that [markdown-mode](http://jblevins.org/projects/markdown-mode/) itself
provides a few keys to render into HTML, and it does not need an external
renderer to be installed:

| Key                  | Behavior                                   |
| -------------------- | ------------------------------------------ |
| <kbd>C-c C-c v</kbd> | Render and open in web browser             |
| <kbd>C-c C-c m</kbd> | Render and open in another buffer          |
| <kbd>C-c C-c l</kbd> | Live preview in EWW (the internal browser) |

Another interesting feature is <kbd>M-x orgtbl-mode</kbd>, a minor mode for
editing tables: it works like org tables but it uses the GitHub-flavored
format. Use the tab key to switch to the next cell and reformat the whole
table.

## Customization

The main file of the configuration is `init.el`. It looks like this:

```lisp
;;; 1. Load all before-init.el files. The ~/.emacs.d/before-init.el
;;; comes first (if exists), followed by any existing before-init.el
;;; file from all ~/.emacs.d/taps/subdirs.
(dolist (tapped-file exordium-tapped-before-init-files)
  (load tapped-file))

;;; 2. Define the list of Melpa packages that we need, and load any missing
;;; one. Note that they are NOT updated automatically.

;;; 3. Local preferences: load all prefs.el. The ~/.emacs.d/prefs.el
;;; comes first (if exists), followed by any existing prefs.el
;;; file from all ~/.emacs.d/taps/subdirs.
(require 'init-prefs)       ; defines variables that prefs.el can override
(dolist (tapped-file exordium-tapped-prefs-files)
  (load tapped-file))

;;; 4. Load the "modules" in ~/.emacs.d/modules. See below.

;;; 5. Load the default theme in ~/.emacs.d/themes.

;;; 6. Load all after-init.el files.The ~/.emacs.d/after-init.el
;;; comes first (if exists), followed by any existing after-init.el
;;; file from all ~/.emacs.d/taps/subdirs.
(dolist (tapped-file exordium-tapped-after-init-files)
  (load tapped-file))
```

Modules can be individually commented out if needed:

```lisp
;;; Uncomment the modules you'd like to use and restart Emacs afterwards,
;;; or evaluate the require expression with M-C-x.

;;; Look and feel
(require 'init-look-and-feel)   ; fonts, UI, keybindings, saving files etc.
(require 'init-linum)           ; line numbers

;;; Usability
(require 'init-window-manager)  ; navigate between windows
(require 'init-util)            ; utilities like match paren, bookmarks...
(require 'init-ido)             ; supercharged completion engine
(require 'init-highlight)       ; highlighting current line, symbol under point
(cond ((eq exordium-complete-mode :auto-complete)
       (require 'init-autocomplete)) ; auto-completion (see below for RTags AC)
      ((eq exordium-complete-mode :company)
       (require 'init-company))) ; company mode (rtags are on by default)
(when exordium-helm-projectile  ; find files anywhere in project
  (require 'init-helm-projectile))
(require 'init-helm)            ; setup helm

;;; Magit and git gutter
(require 'init-git)

;;; Themes
(if exordium-nw
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
[this page](https://github.com/philippe-grenet/exordium/blob/master/doc/code-organization.md)
explains the code organization. Each module starts with a commentary including
all key bindings.

### Local files

3 files can be added in your directory `~/.emacs.d` to customize the
configuration for the local machine:

File name        | Usage
-----------------|-------------------------------------------------------
`prefs.el`       | Loaded before any module. Use it to override fonts, window size etc. See [init-prefs.el](https://github.com/philippe-grenet/exordium/blob/master/modules/init-prefs.el).
`before-init.el` | Loaded before anything else. Use it to set up the http proxy for instance.
`after-init.el`  | Loaded after everything else. Use it to load machine-specific extensions.

### Taps

The idea is inspired by taps from [Homebrew](http://brew.sh). You can clone any
git repository into `~/.emacs.d/taps/` directory (as a subdirectory of the
latter). It is going to be called `tap`. Anything below `taps` subdirectory is
not tracked by Exordium, although each `tap` can be a repository itself,
allowing for tracked customisation.

When Exordium is initialised, it searches for the three special
[Local files](#local-files) in each `tap`. Files are then added to the tapped
lists: before, prefs, and after. `tap`s are searched in alphabetical order of
their respective names in `~/.emacs.d/taps` directory (`string-lessp` to be
exact). This order determines the order of files in each tapped list. This lets
you influence the order of processing within each tapped list, i.e., you can
rename your tapped repositories (clones). The [Local files](#local-files) from
your `~/.emacs.d` are always first in each respective tapped list. Each tapped
list is processed (each file from it is loaded) as a replacement for a
respective [Local file](#local-files).

Exordium-specific emacs functions are WIP.

### Preferences

`modules/init-prefs.el` defines the preferences that can be changed in your
`prefs.el`. For example, your `prefs.el` could contain:

```lisp
;; Fonts + size in order of preference. First available one will be picked.
(setq exordium-preferred-fonts '("Monospace" . 120) ("Mono" . 120))

;; Default Emacs frame size in chars
(setq exordium-preferred-frame-width  120
      exordium-preferred-frame-height 65)

;; Show line numbers (default t)
(setq exordium-display-line-numbers t)

;; Highlight current line (default t)
(setq exordium-line-mode t)

;; Don't set ESC key to C-g (quit/abort)
(setq exordium-keyboard-escape nil)

;; Disable electric-pair (automatically inserts a closing parenthese,
;; curly brace, etc.)
(setq exordium-enable-electric-pair-mode nil)

;; Don't use Powerline (may cause Emacs to crash on startup sometimes)
(setq exordium-enable-powerline nil)
```

There are more options, see
[init-prefs.el](https://github.com/philippe-grenet/exordium/blob/master/modules/init-prefs.el).

### Themes

Exordium includes several themes that are integrated with Powerline and that
should work well in -nw mode. Use <kbd>M-x switch-theme</kbd> to change the
theme. Otherwise, setting your favorite theme in `prefs.el` like the
following will give you this:

```lisp
;; Available themes (default is tomorrow-night):
;; - tomorrow-night, tomorrow-night-bright, tomorrow-night-blue,
;;   tomorrow-night-eighties, tomorrow-day
;; - solarized-dark, solarized-light
;; - monokai
;; - zenburn
;; - material
;; - atom-one
(setq exordium-theme 'material)

;; Powerline theme:
(setq exordium-powerline-theme :wave)
```

![Material](https://raw.github.com/philippe-grenet/exordium/master/doc/material.png)

### Local modules

You can create a directory `~/.emacs.d/local` for your own local modules (this
directory is ignored in git). In that case you should use `require` forms in
`after-init.el` to load them.

Here is an example. Create a file named `~/.emacs.d/local/init-test-local.el`
with this content:

```lisp
;;;; Test local module

(message "**** This test local module is loaded! ****")

(provide 'init-test-local)
```

Then create a file `~/.emacs.d/after-init.el` with this content:

```lisp
;;;; Local stuff

(message "**** after_init ****")

(require 'init-test-local)
```

Restart Emacs. The message buffer should show two lines:

```text
**** after_init ****
**** This test local module is loaded! ****
```

Local modules files can be named anything as long as the file name, the symbol
in `provide` and the symbol in `require` are the same.

## Troubleshooting

### Bugs

* Powerline may cause Emacs to crash on startup because of a race condition
  inside Emacs. A solution is to make it display after one second of idle time
  in order to guarantee that Emacs has finished initializing. For this, add
  `(setq exordium-display-powerline-after-idle-time 1)` in your
  `prefs.el`. Another solution is to enable powerline manually using a function
  like this:

```lisp
(defun powerline ()
  "Enable powerline."
  (interactive)
  (require 'powerline)
  (powerline-set-selected-window)
  (require 'init-powerline)
  (redraw-display))
```

* Sometimes a random bug may occur that displays this error:
  `fringe-helper-modification-func: Invalid search bound (wrong side of
  point)`. I'm pretty sure this is a bug in `git-gutter-fringe` which display
  git diff icons in the left-side fringe.

  There are two ways to work around it: either add `(setq exordium-git-gutter
  nil)` if your `prefs.el` to disable this feature entirely, or add `(setq
  exordium-git-gutter-non-fringe t)` in your `prefs.el` to display git diffs on
  the left side of line numbers, e.g. not in the fringe. Note that the latter
  disables the highlighting of the current line number for now.

* Editing large comment blocks in C++ can be slow as hell. Unfortunately this
  is a problem with
  [CC-mode](http://www.gnu.org/software/emacs/manual/html_node/ccmode/Performance-Issues.html)
  and not with this config. A simple solution is to turn off font lock
  temporarily with <kbd>M-x font-lock-mode</kbd>. Do it again to re-enable font
  lock after you're done editing your large component-level comment.

* `exordium-preferred-fonts` does not work with `emacs --daemon`. This is
  annoying if you start Emacs as a server and then only use `emacsclient`
  afterwards. The problem is that functions like `font-family-list` return nil
  in Emacs server, so there is no good way to verify what fonts are available
  in the server process (believe me I tried). The solution is to put something
  like `(setq default-frame-alist '((font . "Inconsolata-12")))` in your
  `pref.el` (you need to know exactly what font and size you want for
  your local machine). The code below works for both Emacs and `emacsclient`:

```lisp
;; ~/.emacs.d/prefs.el

;; Font and initial frame size
(cond ((daemonp)
       (message "Setting prefs for emacsclient")
       (setq exordium-preferred-frame-width nil
             exordium-preferred-frame-height nil)
       (setq default-frame-alist
             (append '((font   . "Consolas 13")
                       (top    . 0)
                       (left   . 50)
                       (width  . 110)
                       (height . 71))
                     default-frame-alist)))
      (t
       (message "Setting prefs for emacs")
       (setq exordium-preferred-frame-width 110
             exordium-preferred-frame-height 71)
       (setq exordium-preferred-fonts '(("Consolas" . 120)
                                        ("Monaco"   . 120)))))
```

* Sometimes weird bugs may happen after an upgrade or during development on a
  module. Exordium recompiles any `.el` file for which the corresponding `.elc`
  files is older on startup, but does not try to force any `.el` file to be
  compiled.  Two functions are useful in this case: `M-x uncompile-modules`
  removes all `.elc` files (if you restart Emacs it will not compile
  anything). `M-x force-recompile-modules` recompiles everything.

* RTags now uses memory mapped files instead of loading projects into
  memory. It may be slow if your home directory is NFS-mounted, since by
  default the index is stored in `~/.rtags`. The solution is to store the index
  on a local drive, preferably an SSD. You do this by creating a file
  `~/.rdmrc` with a content like this: `--data-dir=/local/drive/.rtags`.

### Configuration profiling

<kbd>M-x emacs-init-time</kbd> shows the time Emacs took to start. You can profile the
configuration using this command (this example is for OS X):

```bash
$ Applications/Emacs.app/Contents/MacOS/Emacs -Q -l ~/.emacs.d/extensions/profile-dotemacs.el -f profile-dotemacs

```
