![Exordium](https://raw.github.com/emacs-exordium/exordium/master/doc/Exordium.png)

# What is this repo

A portable Emacs configuration focused on adding IDE-level features for C++ and
Lisp/Clojure programming. It is only intended to work with Emacs version 27.1
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
  * [Conflict Resolution](#conflict-resolution)
* C++
  * [Utilities](#utilities)
  * [Snippets](#snippets)
  * [Include What You Use](#include-what-you-use)
* [Lisp](#lisp)
* [Markdown](#markdown)
* [LSP](#lsp)
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
  [Treemacs](https://github.com/Alexander-Miller/treemacs) (directory tree);
  [Avy](https://github.com/abo-abo/avy) (jump to visible text in 2 or 3 key-strokes);
  [ace-window](https://github.com/abo-abo/ace-window) (quick jump between windows);
  [helpful](https://github.com/Wilfred/helpful) (a better Emacs *help* buffer);
  [casual](https://github.com/kickingvegas/casual) (A collection of opinionated
  keyboard-driven user interfaces for various built-in Emacs modes);
  [which-key](https://github.com/justbur/emacs-which-key) (display available keybindings);
  [Treesitter]()(Parser-based font lock).
* Projects: [Projectile](http://batsov.com/projectile) (project-based file
  management tool).
* Git: [Magit](http://magit.vc) (git UI);
  [Forge](https://magit.vc/manual/forge/) (work with Git forges);
  [Git Gutter](https://github.com/syohex/emacs-git-gutter) (diffs in buffer);
  [difftastic](https://github.com/pkryger/difftastic.el) (structural diffs).
* C++:
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
$ git clone https://github.com/emacs-exordium/exordium.git ~/.emacs.d
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

Exordium tries to automatically refresh ELPAs package contents to when it may
be necessary. You can force or inhibit such a refresh by starting Emacs with
command line argument `--exordium-force-package-refresh-contents` or
`--exordium-inhibit-package-refresh-contents` respectively.

### Files

The main file is `init.el`: it load the modules from the `modules` subdirectory
and the default theme from the `themes` subdirectory. The `extensions`
subdirectory is used for third-party plugins that are not available in Melpa.

3 files can be added in your directory `~/.emacs.d` to customize the
configuration for the local machine (these files are not tracked by git):

File name        | Usage
-----------------|-------------------------------------------------------
`prefs.el`       | Loaded before any module. The module [init-prefs.el](https://github.com/emacs-exordium/exordium/blob/master/modules/init-prefs.el) defines a number of customization variables for fonts, theme etc. `prefs.el` is where you should override any of these variables.
`before-init.el` | Loaded before anything else. Use it to set up the HTTP proxy for instance.
`after-init.el`  | Loaded after everything else. This is where you should add your own features.

You can also have local modules in a directory `~/.emacs.d/local`.

See the [Customization](#customization) section below for more details.

## Keymap

Exordium uses `bind-key` to set up key bindings, which keeps track of all bindings made. You can display a comprehensive list using <kbd>M-x describe-personal-keybindings</kbd>.

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
<kbd>C-c C-SPC</kbd> | Toggle highlight of the symbol under the cursor (by default, up to 8 different symbols using different colors).

Editing:

Keybinding                | Description
--------------------------|----------------------------------------------------------
<kbd>RET</kbd>            | Return and indent by default; use <kbd>S-RET</kbd> for just return.
<kbd>M-<backspace></kbd>  | `backward-kill-word` (e.g. the opposite of <kbd>M-d</kbd> `kill-word`).
<kbd>C-\\</kbd>           | Delete spaces after cursor (`delete-horizontal-space-forward`).
<kbd>C-<backspace></kbd>  | Delete spaces before cursor (`delete-horizontal-space-backward`).
<kbd>M-\\</kbd>           | Delete all spaces around cursor.
<kbd>M-<left></kbd> and <kbd>M-<right></kbd> | Move cursor by semantic units (use <kbd>C-<left></kbd> and <kbd>C-<right></kbd> to move by words).
<kbd>C-c d</kbd>          | Duplicate line.
<kbd>C-=</kbd>            | Expand region by semantic units.
<kbd>M-C-=</kbd>          | Contract region by semantic units.
<kbd>M-<up></kbd>         | Move region one line up
<kbd>M-<down></kbd>       | Move region one line down
<kbd>C-&#124;</kbd>       | Toggle the 80-column ruler (fill column indicator).

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
<kbd>M-o NUMBER</kbd>   | Jump to the specified window number using [ace-window](https://github.com/abo-abo/ace-window). If you only have 2 windows, cycle between them.

Auto-complete/Company:

Keybinding         | Description
-------------------|-----------------------------------------------------------
<kbd>C-.</kbd>     | Force trigger auto-complete/company-complete.
<kbd>ESC</kbd>     | Abort auto-complete/company-complete.

Tip: if you are looking for a particular key and you know it starts with a
given prefix, type the prefix followed by <kbd>C-h</kbd>: Emacs will display
the list of keys starting with that prefix. For example <kbd>C-c C-h</kbd> lists
all the keys starting with <kbd>C-c</kbd>.

Highlighted symbols:

When a symbol has been highlighted using <kbd>C-C C-SPC</kbd>
(`symbol-overlay-put`) and a point is placed on the highlighted symbol extra
keymap is active that allows for example to navigate and edit withing context
of the symbol.

A few example bindings:

Keybinding                       | Description
-------------------------------- |----------------------------------------------------------
<kbd>M-n</kbd>                   | Jump to next location of highlighted symbol at point.
<kbd>M-p</kbd>                   | Jump to previous location of highlighted symbol at point.
<kbd>M-h</kbd> or <kbd>C-o</kbd> | Display bindings for highlighted symbol at point.

You can change the modifier key (default: <kbd>M</kbd>) by customizing `exordium-highlight-symbol-map-modifier`.

## Help extensions
Exordium provides a few help extensions that enhance default Emacs
configuration presenting user with more information. All of them are enabled by
setting `exordium-help-extensions` to t. Noteably, the following are turned on:

- [which-key](https://github.com/justbur/emacs-which-key) - display available
  keybindings after a short while (for example type <kbd>C-x</kbd> and wait a
  second,
- [casual](https://github.com/kickingvegas/casual) - a collection of
  opinionated keyboard-driven user interfaces for various built-in Emacs modes,
- [helpful](https://github.com/Wilfred/helpful) - an alternative to the
  built-in Emacs help that provides much more contextual information.

Keybinding          | Description
------------------- |----------------------------------------------------------
<kbd>C-o</kbd>      | Display a transient with bindings for a `casual` user of `org-agenda`, `bookmark`, `calc`, `calendar`, `dired`, `ibuffer`, `info`, `isearch`, `rebuilder`, `symbol-overlay` or a Casual EditKit menu.
<kbd>C-h f</kbd>    | Show `helpful` buffer for function, macro, or a special form (`helpful-callable`).
<kbd>C-h F</kbd>    | Show `helpful` buffer for function (`helpful-function`).
<kbd>C-h v</kbd>    | Show `helpful` buffer for variable (`helpful-variable`).
<kbd>C-h C</kbd>    | Show `helpful` buffer for interactive command (`helpful-command`).
<kbd>C-c C-d</kbd>  | Show `helpful` buffer for thing at point, when in `emacs-lisp-mode` (`helpful-thing-at-point`).
<kbd>C-j</kbd>      | Show `helpful` buffer for currently selected candidate, when completing read for `helpful` commands.

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

![Helm](https://raw.github.com/emacs-exordium/exordium/master/doc/helm.png)

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
<kbd>C-c p .</kbd>                     | IDO: find file at point based on context (alternative Helm)
<kbd>C-c p s g</kbd>                   | Grep in current project
<kbd>C-c p s a</kbd>                   | Same but using ack
<kbd>C-c p r</kbd>                     | Interactive query-replace on all files in project
<kbd>C-c p i</kbd>                     | Invalidate the cache

See [Projectile](https://github.com/bbatsov/projectile) documentation for other
keys.

## Helm

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
<kbd>C-x c g</kbd>  | Google suggest.

### Other Helm tools

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
* <kbd>C-S-r</kbd>: ripgrep search for text in current projectile project.
* <kbd>M-x helm-multiple-swoop-all</kbd>: Swoop search within all buffers.

Note that <kbd>C-S-a</kbd> and <kbd>C-S-r</kbd> work also directly from
`helm-projectile-switch-project`. This means that you searching a project with `ag` or `rg`
simply by selecting it in helm and hitting appropriate keybinding.

## Treemacs

[Treemacs](https://github.com/Alexander-Miller/treemacs) is a tree layout file
explorer for Emacs. It is linked with Projectile and Git, and it can display the
project directory structure on the left side:

![Treemacs](https://raw.github.com/emacs-exordium/exordium/master/doc/treemacs.png)

Keybinding          | Description
--------------------|----------------------------------------------------------
<kbd>C-c e</kbd>    | Toggle `treemacs` the current directory.
<kbd>C-c E</kbd>    | Open a projectile project (with a selector).

Treemacs displays the git status of files (added, modified, ignored etc.) using
different faces.

With the cursor in the Treemacs window, you can use <kbd>TAB</kbd> to
open/close directories, <kbd>RET</kbd> to open a file, and <kbd>q</kbd> to
quit. Use <kbd>?</kbd> to view all the available keys. See the documentation of
Treemacs for details.

## Git

All git-related keys use prefix <kbd>C-c g</kbd> plus one more key. For example
<kbd>C-c g s</kbd> runs [Magit](http://magit.vc) status:

![magit](https://raw.github.com/emacs-exordium/exordium/master/doc/magit.png)

The bottom window shows the current git status. Use the <kbd>tab</kbd> key on
any file to fold or unfold its diff. Use the <kbd>s</kbd> key to stage or
unstage a file, and the capital <kbd>S</kbd> to stage all of them. Use the
<kbd>k</kbd> key to revert a file. Type <kbd>c</kbd> twice to commit; it will
ask for the commit message (<kbd>C-c C-c</kbd> to close the window and
commit). Finally the <kbd>q</kbd> key quits magit.

By default running [Magit](http://magit.vc) via global keybindings will start it
in a full screen mode (whole frame). This is controlled by
`exordium-use-magit-fullscreen` preference. You can [customize](#customization) it
to `nil` to get original [Magit](http://magit.vc) behavior.

The screenshot above also shows
[git gutter](https://github.com/syohex/emacs-git-gutter) in the top buffer. Git
gutter displays a git diff of the file in the left-side fringe (you can
[customize](#customization) it). Git gutter defines a few keys for navigating
between hunks, diffing and reverting.

Magit keys:

Keybinding            | Description
----------------------|-----------------------------------------------------------
<kbd>C-c g s</kbd>    | Open git status (`magit-status`).
<kbd>C-c g l</kbd>    | Open git log (`magit-log` or `magit-dired-log` when in `dired-mode`).
<kbd>C-c g f</kbd>    | Open git file log (`magit-file-log`).
<kbd>C-c g b</kbd>    | Toggles git blame mode on and off (`magit-blame-mode`).
<kbd>C-c g c</kbd>    | Clone a repository (`magit-clone`).
<kbd>C-c g g</kbd>    | Git grep (`vc-git-grep`) or (`helm-grep-do-git-grep`).

To perform structural diffs you need to install an external tool
[`difftastic`](https://difftastic.wilfred.me.uk) (see the web page for
details).  Once `difftastic` has been installed, and you are in either
`magit-diff` transient, or `magit-blame` transient, or in
`magit-blame-read-only-mode`:

Keybinding            | Description
----------------------|-----------------------------------------------------------
<kbd>D</kbd>          | Run Difftastic diff (guessing what to diff from context) (`difftastic-magit-diff`).
<kbd>S</kbd>          | Run Difftastic show (`difftastic-magit-show`).

or when you are in `dired-mode`:

Keybinding            | Description
----------------------|-----------------------------------------------------------
<kbd>C-c =</kbd>      | Run Difftastic (`difftastic-dired-diff`).

Forge keys:

Keybinding            | Description
----------------------|-----------------------------------------------------------
<kbd>C-c C-p</kbd>    | in `forge-post-mode`: Preview post with markdown (`exordium-forge-markdown-preview`)
<kbd>C-c C-d</kbd>    | in `forge-post-mode`: Submit current post (a Pull Request) as a draft (`exordium-forge-post-submit-draft`)
<kbd>C-c C-d</kbd>    | in `magit-status-mode` and in `forge-topic-mode`: Mark a Pull Request at point as ready for review (`exordium-forge-mark-ready-for-rewiew`)


Git gutter keys:

Keybinding            | Description
----------------------|-----------------------------------------------------------
<kbd>C-c g down</kbd> | Goto next hunk in buffer (`git-gutter:next-hunk`).
<kbd>C-c g n</kbd> | Goto next hunk in buffer (`git-gutter:next-hunk`).
<kbd>C-c g up</kbd>   | Goto previous hunk in buffer (`git-gutter:previous-hunk`).
<kbd>C-c g p</kbd>   | Goto previous hunk in buffer (`git-gutter:previous-hunk`).
<kbd>C-c g d</kbd>    | Diff the current hunk (`git-gutter:popup-diff`).
<kbd>C-c g r</kbd>    | Revert the current hunk after confirmation (`git-gutter:revert-hunk`).

Git Timemachine key   | Description
----------------------|-----------------------------------------------------------
<kbd>C-c g t</kbd>    | Enter the git time machine (`git-timemachine-toggle`)

### Conflict Resolution

If you use `ediff` to solve merge conflicts you may find the following keys useful
(in `*Ediff Control Panel*`):

ediff key             | Description
----------------------|-----------------------------------------------------------
<kbd>A</kbd>          | Copy buffer A's region followed by buffer B's region to C
<kbd>B</kbd>          | Copy buffer B's region followed by buffer A's region to C

When the variable `exordium-smerge-show-dispatch` is set to `t` (the default) hitting `RET`
on a file with unmerged changes in Magit status buffer will display a SMerge dispatch that
will assist with merge using built-in `smerge-mode`. When all conflicts are resolved,
hitting `C-c C-c` will bring you back to Magit status. Hitting `C-c C-k` will revert the buffer
to last saved changes and will bring you back to Magit status.

The SMerge dispatch is a `transient` command (like most of `magit` and `forge` commands), so it can
always be dismissed with `C-g`.

In addition to keys presented in the dispatch, the following keys are added to SMerge keys:

Keybinding            | Description
----------------------|-----------------------------------------------------------
<kbd>C-c ^ d</kbd> | Show SMerge dispatch  (`exordium-smerge-dispatch`).

## C++

### Utilities

Keybinding           | Description
---------------------|-----------------------------------------------------------
<kbd>C-TAB</kbd>     | Alternate between header file and source file.
<kbd>C-u C-TAB</kbd> | Alternate between source/header file and BDE test driver.
<kbd>C-c ;</kbd>     | Rename variable under cursor

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
(bind-key "<f2>" #'yas-expand yas-minor-mode-map)
```

Snippets are stored in `~/.emacs.d/snippets/c++-mode`. Here are
[the snippets](https://github.com/emacs-exordium/exordium/blob/master/doc/snippets.md).

Note that variable `*bde-component-author*` defines the default author for a
header file template (see `modules/init-yasnippet.el`). You can set it to your
name in `after-init.el`.

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

`include-what-you-use` relies on the compilation database to be available in
`compile_commands.json` file. It searches for one in the following locations:
`cmake.bld/Linux`, `build`, `bld`, `cmake-build`
`cmake-build/linux_64_static_ninja_Debug`,
`cmake-build/linux_64_static_make_Debug`,
`cmake-build/linux_64_static_ninja_Release`, and
`cmake-build/linux_64_static_make_Release`.

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

## LSP
Exordium includes configuration for [`lsp-mode`](https://emacs-lsp.github.io/lsp-mode/) a language server protocol client. The configuration currently supports C++ using clangd. It is enabled with the customization option `exordium-lsp-mode-enable`. The settings that are enabled by default have been in use by some Exordium in anger, but may need tweaking. The authors of lsp-mode also have a policy of enabling new features by default, so that new capabilities are visible and discoverable.

The `lsp-mode` keymap is bound to `C-c l`.

LSP mode is available via TRAMP, and in particular via tramp to docker containers.

LSP mode in the shared Exordium base is stable enough for use, but the configuration details are experimental and subject to change without notification.

## Tree-sitter
Exordium can be configured to use [tree-sitter](https://github.com/tree-sitter/tree-sitter) for parser directed font lock. In emacs versions 29 or greater it uses the new built in  `treesit` mode if emacs was built with treesitter support, otherwise it falls back to using the [tree-sitter](https://github.com/emacs-tree-sitter/elisp-tree-sitter) mode from melpa. In emacs 29, language mode support is via [treesit-auto](https://github.com/renzmann/treesit-auto) and an exordium function that automatically forwards the hook from the new lang-ts-mode modes to the hook for lang-mode, hopefully preserving your customizaations. If treesit is unavailable, [tree-sitter-langs](https://github.com/emacs-tree-sitter/tree-sitter-langs) is used.

The configuration variable is `exordium-treesit-modes-enable` and is disabled by default.

## Customization

The main file of the configuration is `init.el`. It looks like this:

```lisp
;; 1. Load all before-init.el files.  The ~/.emacs.d/before-init.el comes first
;; (if exists), followed by all existing before-init.el files from all
;; ~/.emacs.d/taps/subdirs.
;; Load before init files
(dolist (tapped-file exordium-tapped-before-init-files)
  (message "Loadding tapped before-init file: %s" tapped-file)
  (load (file-name-sans-extension tapped-file)))

;; 2. Define the list of Melpa packages that we need, and load any missing one.
;; Note that they are NOT updated automatically.
;; [...]

;; 3. Local preferences: load all prefs.el.  The ~/.emacs.d/prefs.el comes
;; first (if exists), followed by all existing prefs.el file from all
;; ~/.emacs.d/taps/subdirs.
(dolist (tapped-file exordium-tapped-prefs-files)
  (message "Loadding tapped prefs file: %s" tapped-file)
  (load (file-name-sans-extension tapped-file)))

;; 4. Load the default theme in ~/.emacs.d/themes. See below.

;; 5. Load the "modules" in ~/.emacs.d/modules. See below.

;; 6. Load all after-init.el files.The ~/.emacs.d/after-init.el comes first (if
;; exists), followed by all existing after-init.el file from all
;; ~/.emacs.d/taps/subdirs.
;; Local extensions
(dolist (tapped-file exordium-tapped-after-init-files)
  (message "Loadding tapped after-init file: %s" tapped-file)
  (load (file-name-sans-extension tapped-file)))
```

Modules can be individually commented out if needed:

```lisp
;; Themes
;; Note: use "export TERM=xterm-256color" for emacs -nw
(setq custom-theme-directory exordium-themes-dir)
(exordium-require 'init-progress-bar)

(when exordium-nw
  (set-face-background 'highlight nil))
(when exordium-theme
  (exordium-require 'init-themes))

;; Look and feel
(exordium-require 'init-look-and-feel)     ; fonts, UI, keybindings, saving files etc.
(exordium-require 'init-font-lock)         ; enables/disables font-lock globally.
(exordium-require 'init-linum)             ; line numbers
(when exordium-smooth-scroll
  (exordium-require 'init-smooth-scroll)
  (smooth-scroll-mode 1))                  ; smooth scroll

(update-progress-bar)

;; Usability
(exordium-require 'init-window-manager)   ; navigate between windows
(exordium-require 'init-util)             ; utilities like match paren, bookmarks...
(unless exordium-helm-everywhere
  (exordium-require 'init-ido))           ; supercharged completion engine
(exordium-require 'init-highlight)        ; highlighting current line, symbol under point

(pcase exordium-complete-mode
  (:auto-complete
   (exordium-require 'init-autocomplete))
  (:company
   (exordium-require 'init-company)))     ; completion

(exordium-require 'init-helm)             ; setup helm
(when exordium-projectile
  (exordium-require 'init-projectile))
(when (and exordium-projectile exordium-helm-projectile)
  (exordium-require 'init-helm-projectile))

(when exordium-help-extensions
  (exordium-require 'init-help))           ; extra help

(update-progress-bar)

(exordium-require 'init-dired)            ; enable dired+ and wdired permission editing
(exordium-require 'init-git)              ; Magit and git gutter
(exordium-require 'init-git-visit-diffs)  ; visit diffs in successive narrowed buffers
(exordium-require 'init-forge)            ; Forge
(exordium-require 'init-flb-mode)         ; frame-local buffers

(update-progress-bar)

;; Prog mode
(exordium-require 'init-prog-mode )

;; Shell mode
(exordium-require 'init-prog-mode)

;; Major modes
(exordium-require 'init-markdown)
(exordium-require 'init-org)
(exordium-require 'init-xml)

;; OS-specific things
(when exordium-osx
  (exordium-require 'init-osx))

;; C++
(exordium-require 'init-cpp)
;; Etc.
```

If you are looking for a specific feature or key binding,
[this page](https://github.com/emacs-exordium/exordium/blob/master/doc/code-organization.md)
explains the code organization. Each module starts with a commentary including
all key bindings.

### Local files

3 files can be added in your directory `~/.emacs.d` to customize the
configuration for the local machine:

File name        | Usage
-----------------|-------------------------------------------------------
`prefs.el`       | Loaded before any module. Use it to override fonts, window size etc. See [init-prefs.el](https://github.com/emacs-exordium/exordium/blob/master/modules/init-prefs.el).
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

Note that Exordium doesn't add any directory to a load path. If your tap expects
that it's directory tree is in load path, you can use the following code to
update the load-path:

```lisp
(eval-when-compile
  (unless (featurep 'init-require)
    (load (file-name-concat (locate-user-emacs-file "modules") "init-require"))))
(exordium-require 'init-lib)
(when (bound-and-true-p load-file-name)
  (exordium-add-directory-tree-to-load-path (file-name-directory load-file-name)))
```

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
[init-prefs.el](https://github.com/emacs-exordium/exordium/blob/master/modules/init-prefs.el).

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

![Material](https://raw.github.com/emacs-exordium/exordium/master/doc/material.png)

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

### Configuration profiling

<kbd>M-x use-package-report</kbd> shows the time `use-package` and
`exordium-require` used to load and configure packages.  Note that the times
for `exordium-require` have corresponding `use-package` times included.

<kbd>M-x emacs-init-time</kbd> shows the time Emacs took to start. You can profile the
configuration using this command (this example is for OS X):

```bash
$ Applications/Emacs.app/Contents/MacOS/Emacs -Q -l ~/.emacs.d/extensions/profile-dotemacs.el -f profile-dotemacs

```

It's probably not the best idea to profile your first start as the bulk of the
startup time in such a case is spent on downloading and byte-compiling
packages.
