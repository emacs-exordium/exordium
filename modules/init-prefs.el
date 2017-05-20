;;;; Definition of the variables that prefs.el can override.
;;;
;;; These variables control other init-XXX files, such as init-look-and-feel.el.
;;; You can override them by creating your own file ~/.emacs.d/prefs.el
;;; with content like:
;;;
;;; (setq exordium-preferred-frame-width  110
;;;       exordium-preferred-frame-height 75)
;;;
;;; You can also use M-x `customize' and browse group Local -> Init.

(defgroup exordium nil
  "Customize your Emacs configuration."
  :group 'local)


;;; UI -- see init-look-and-feel.el

(defcustom exordium-preferred-fonts
  '(("Consolas"  . 120)
    ("Monaco"    . 120)
    ("Monospace" . 120)
    ("Mono"      . 120))
  "List of preferred fonts/sizes to use, in decreasing order of
preference. We will use the first one that is available on the
local machine. It is a list of pairs (font-name . font-size). If
nil, we don't set any font. Note that you can get the list of
available font names by evaluating (font-family-list)."
  :group 'exordium
  :type  'sexp)

(defcustom exordium-preferred-frame-width 100
  "Default frame width (number of columns)."
  :group 'exordium
  :type  'integer)

;; Note: a weird bug in OS X makes the title bar black if height is too big
(defcustom exordium-preferred-frame-height 60
  "Default frame height (a number of lines)."
  :group 'exordium
  :type  'integer)

(defcustom exordium-line-mode t
  "Whether the current line is highlighted or not."
  :group 'exordium
  :type  'boolean)

(defcustom exordium-font-lock t
  "Whether font-lock is turned on globally or not."
  :group 'exordium
  :type  'boolean)

(defcustom exordium-scroll-bar t
  "Whether a scroll bar is displayed globally or not"
  :group 'exordium
  :type   'boolean)

;;; Keyboard

(defcustom exordium-keyboard-escape nil
  "Whether the Escape key does keyboard quit e.g. the equivalent of Ctrl-G."
  :group 'exordium
  :type  'boolean)

(defcustom exordium-keyboard-ctrl-z-undo t
  "Whether Ctrl-z is rebound to Undo, like most other applications."
  :group 'exordium
  :type  'boolean)

(defcustom exordium-enable-y-or-n t
  "Use y or n answers instead of full words yes or no"
  :group 'exordium
  :type  'boolean)

(defcustom exordium-enable-newline-and-indent t
  "If t, binds the return key to newline-and-indent, and
shift-return for just newline.  If nil, do the opposite."
  :group 'exordium
  :type  'boolean)

(defcustom exordium-delete-trailing-whitespace t
  "If t, deletes all trailing whitespaces on all lines before
saving a buffer. This is the default because it is generally a
good practice. Set this variable to to nil if you work on legacy
projects that have a lot of trailing whitespaces"
  :group 'exordium
  :type  'boolean)

(defcustom exordium-enable-electric-pair-mode t
  "Enables electric-pair-mode."
  :group 'exordium
  :type  'boolean)

(defcustom exordium-enable-cua-mode nil
  "If set to t, enables CUA mode.
If set to :region, enables CUA rectagular regions.
IF nil, disables CUA completely."
  :group 'exordium
  :type  'symbol)

(defcustom exordium-enable-evil-mode nil
  "If set to t, enables EVIL mode."
  :group 'exordium
  :type  'boolean)

;;; Backup files (e.g. file~)
(defcustom exordium-backup-files nil
  "Enables or disables backup files. Disabled by default, I can't
  stand these annoying files~"
  :group 'exordium
  :type  'boolean)


;;; Line numbers - see init-linum.el

(defcustom exordium-display-line-numbers t
  "Whether line numbers are displayed or not.
Set to nil to disable line numbers.
Set to t or :linum to enable line numbers using package `linum'.
Set to :nlinum to enable line numbers using pakage `nlinum'
which should be more efficient in particular for large buffers."
  :group 'exordium
  :type  'symbol)


;;; Miscellaneous utilities -- see init-util.el

(defcustom exordium-fci-mode :ondemand
  "Controls fill-column-indicator e.g. the 80-character ruler.
:ondemand off by default (you need to hit C-| to toggle it),
:always means it is always on, :prog means on for programming modes
nil means it is disabled."
  :group 'exordium
  :type  'symbol)

(defcustom exordium-fci-use-dashes t
  "If t, use dashes for fill-column-indicator.
If nil, use a plain line."
  :group 'exordium
  :type  'boolean)

(defcustom exordium-fci-fix-autocomplete-bug t
  "Whether fill-column-indicator is temporarily disabled when an
  auto-complete popup is displayed, in order to work around a bug
  that causes the popup to be incorrectly displayed"
  :group 'exordium
  :type  'boolean)

(defcustom exordium-highlight-symbol nil
  "Whether occurrences of the symbol under point gets highlighted
  in the buffer after a few seconds."
  :group 'exordium
  :type  'boolean)

(defcustom exordium-skip-taps-update nil
  "Whether to skip taps update when updating configuration.
If set to nil, each tap need to be updated manually,
i.e., with git pull."
  :group 'exordium
  :type  'boolean)


;;; Smooth scroll - see init-smooth-scroll.el

(defcustom exordium-smooth-scroll nil
  "Whether smooth scroll is enabled or not."
  :group 'exordium
  :type  'boolean)


;;; Autocomplete -- see init-autocomplete.el

(defcustom exordium-complete-mode :auto-complete
  "Slect the completion engine for exordium. Possible values are
  :auto-complete, :company, and nil. Default is :auto-complete. See also
  `exordium-rtags-auto-complete'."
  :group 'exordium
  :type  'symbol)


;;; Themes -- see themes directory

(defcustom exordium-theme 'tomorrow-night
  "Theme to load on startup. Use nil to not load any theme,
otherwise use one of the following:
- tomorrow-day, tomorrow-night, tomorrow-night-bright,
  tomorrow-night-blue, tomorrow-night-eighties
- monokai
- solarized-light, solarized-dark
- zenburn
- material
- atom-one"
  :group 'exordium
  :type  'symbol)

(defcustom exordium-theme-use-loud-rtags-faces nil
  "Whether RTags error/warning/fixit faces are loud or not with
the current theme. It depends on the theme but generally setting
this to t will use reverse video, while setting it to nil will
use underline waves."
  :group 'exordium
  :type  'boolean)

(defcustom exordium-theme-use-big-font t
  "Whether the theme uses big fonts for titles and top-level
  items. This applies to modes like org or markdown. Set it to
  nil if you do not want to have variable-sized font."
  :group 'exordium
  :type  'boolean)

(defcustom exordium-highlight-linum t
  "Whether the current line number is highlighted.
This does not work with nlinum-mode currently."
  :group 'exordium
  :type  'boolean)

(defcustom exordium-enable-powerline t
  "Enables Powerline"
  :group 'exordium
  :type  'boolean)

(defcustom exordium-display-powerline-after-idle-time 0
  "Fix a race/crash at startup related to Powerline that may
  happen on some environments, by displaying Powerline only after
  a specified number of seconds of idle time. Normally 1 second
  is enough."
  :group 'exordium
  :type  'integer)

(defcustom exordium-fix-powerline-osx-bug t
  "Fix the graphical bug with Emacs24.4 on OS-X (the angles in
powerline are not rendered correctly). It makes the colors a bit
washed up, especially with the zenburn theme. See these links:
https://github.com/milkypostman/powerline/issues/54
http://emacsredux.com/blog/2013/08/21/color-themes-redux"
  :group 'exordium
  :type  'boolean)

(defcustom exordium-powerline-theme :angle
  "Drives the shape of Powerline separators, and other things.
Possible values are :angle, :wave"
  :group 'exordium
  :type  'symbol)

(defcustom exordium-powerline-shows-rtags-diagnostics t
  "Whether Powerline shows RTags Diagnostics results. If there
are errors, the buffer name is displayed in red instead of the
default color."
  :group 'exordium
  :type  'boolean)

(defcustom exordium-powerline-enable-icons nil
  "Whether Powerline displays icons for git branches and other
  things."
  :group 'exordium
  :type  'boolean)


;;; Programming

;;; See init-prog.el
(defcustom exordium-spell-check :prog
  "Enables flyspell for prog modes if set to :prog.
Disables flyspell if set to nil."
  :group 'exordium
  :type  'symbol)

;;; See init-yasnippet.el
(defcustom exordium-yasnippet t
  "Whether YASnippet is enabled or not."
  :group 'exordium
  :type  'boolean)

;;; See init-helm-projectile.el
(defcustom exordium-helm-projectile t
  "Whether Helm and Projectile are enabled or not."
  :group 'exordium
  :type  'boolean)

;;; See init-git.el
;;; exordium-git-gutter displays a git diff icon in the left fringe, while
;;; exordium-git-gutter-non-fringe displays the diff icon on the left side of the
;;; line numbers. The latter takes precedence.
(defcustom exordium-git-gutter-non-fringe nil
  "Whether a git status icon is displayed at the very left. Takes
  precedence over exordium-gut-gutter. Note that this option
  disables highlighting the current line number"
  :group 'exordium
  :type  'boolean)

(defcustom exordium-git-gutter t
  "Whether a git status icon is displayed in the left-side fringe or not."
  :group 'exordium
  :type  'boolean)

;;; See init-cpp.el
(defcustom exordium-enable-c++11-keywords :simple
  "Enables syntax highlighting for the new keywords introduced in C++11 if set
to :simple. Enables modern-cpp-font-lock when set to :modern."
  :group 'exordium
  :type  'symbol)

;;; See init-clojure.el
(defcustom exordium-clojure nil
  "Whether the development environment for Clojure is enabled or not."
  :group 'exordium
  :type  'boolean)


;;; RTags

;;; See init-rtags.el
(defcustom exordium-rtags-rdm-args nil
  "Command-line arguments passed to rdm, if needed. This should
be a list of strings."
  :group 'exordium
  :type  'sexp)

(defcustom exordium-rtags-auto-complete nil
  "Whether RTags is used as the source for auto-complete in C++ mode."
  :group 'exordium
  :type  'boolean)

;;; See init-rtags-helm.el
(defcustom exordium-rtags-helm-everywhere t
  "Whether RTags uses Helm to display list of results, rather
  than its own UI"
  :group 'exordium
  :type  'boolean)

;;; see init-rtags-cmake.el
(defcustom exordium-rtags-cmake nil
  "Whether RTags is used automatically for CMake-enabled projects"
  :group 'exordium
  :type  'boolean)

(defcustom exordium-rtags-cmake-build-dir "cmake.bld/<arch>"
  "Relative path of the build directory inside a CMake-enabled
  repo. '<arch>' is replaced by the local machine's architecture."
  :group 'exordium
  :type  'string)

;;; See init-rtags-cdb.el
(defcustom exordium-rtags-source-file-extensions '("*.cpp" "*.c")
  "List of source file extension patterns for creating a
  compilation database using command
  `rtags-create-compilation-database'. Not needed for CMake projects."
  :group 'exordium
  :type  'sexp)


(defcustom exordium-iwyu-filter-args '("-fno-default-inline")
  "List of flags that should be removed from the 'include-what-you-use'.
  When `include-what-you-use' is called, all parameters after the first space
  in JSON value of `command' property are passed. Use this list to remove
  unsupported flags and arguments as well as compiler wrappers that are in
  project's compilation database."
  :group 'exordium
  :type  'sexp)

(defcustom exordium-iwyu-extra-args '()
  "List of extra arguments that should be passed when calling
   `include-what-you-use'. They are passed 'as is' to `include-what-you-use'
   executable."
  :group 'exordium
  :type  'sexp)


;;; See init-helm.el
(defcustom exordium-helm-everywhere nil
  "Whether Helm should be used as a substitute for common key bindings."
  :group 'exordium
  :type  'boolean)

(defcustom exordium-helm-fuzzy-match t
  "Whether Helm should use fuzzy matching for searches."
  :group 'exordium
  :type  'boolean)


;;; Desktop state
(defcustom exordium-desktop nil
  "Whether the desktop state is saved"
  :group 'exordium
  :type  'boolean)


;;; Progress bar
(defcustom exordium-progress-bar nil
  "Whether a progress bar is displayed when Exordium is loaded.
It is disabled by default because it actually makes the
configuration load slower."
  :group 'exordium
  :type  'boolean)


;;; Org mode

(defcustom exordium-enable-org-export t
  "Configure org mode for code export and babel. Setting this to
  nil makes emacs starts a little bit faster, if you don't need
  to use this feature."
  :group 'exordium
  :type  'boolean)

(defcustom exordium-no-org-babel-confirm nil
  "Disable confirmation requests when evaluating code blocks when
  using org-babel. Setting to non-nil could conceivably result in
  accidentally executing code."
  :group 'exordium
  :type  'boolean)

(defcustom exordium-org-export-css nil
  "Export from org mode using a css style sheet, rather than
  inline styles. This allows more control over the appearance, at
  the cost of having to maintain a stylesheet."
  :group 'exordium
  :type  'boolean)

(defcustom exordium-org-export-css-stylesheet ""
  "The stylesheet to use in html based org export. This will be
  loaded into `org-html-head' and exported. For example,

     <link rel=\"stylesheet\" type=\"text/css\" href=\"http://sundev3.dev.bloomberg.com/~sdowney/smd-zenburn.css\" />

  To generate a basic css file that matches your theme, use
  `org-html-htmlize-generate-css' which will create a buffer with
  css definitions for all currently defined faces."
  :group 'exordium
  :type  'string)



(provide 'init-prefs)
