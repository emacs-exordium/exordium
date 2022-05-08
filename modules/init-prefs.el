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

(defcustom exordium-enable-insert-gui-primary-selection nil
  "If set to t, binds Meta-Insert to insert-gui-primary-selection.
This makes it easier to paste text from the Windows clipboard."
  :group 'exordium
  :type  'boolean)

;;; Backup files (e.g. file~)
(defcustom exordium-backup-files nil
  "Enables or disables backup files.
Disabled by default, I can't stand these annoying files~"
  :group 'exordium
  :type  'boolean)

(defcustom exordium-use-magit-fullscreen t
  "If t, magit status and log will fill the whole frame.
The original window configuration will be restored when you quit out of magit."
  :group 'exordium
  :type  'boolean)

(defcustom exordium-smerge-show-dispatch t
  "It t, automatically show `exordium-smerge-dispatch' for unmerged files.
This is when hitting RET on a file with a merge conflict in a
Magit status buffer."
  :group 'exordium
  :type 'boolean)

(defcustom exordium-help-extensions t
  "If t, use help extensions like `which-key' and `helpful-mode'."
  :group 'exordium
  :type 'boolean)


;;; Line numbers - see init-linum.el

(defcustom exordium-display-line-numbers t
  "Whether line numbers are displayed or not.
Set to nil to disable line numbers.  Set to t to enable line numbers.

When buffer matches either of the `exordium-inhibit-line-numbers-modes',
`exordium-inhibit-line-numbers-buffer-size', or
`exordium-inhibit-line-numbers-star-buffers' line numbers WILL NOT be shown."
  :group 'exordium
  :type  'symbol)

(defcustom exordium-inhibit-line-numbers-modes '(Info-mode
                                                 calendar-mode
                                                 compilation-mode
                                                 dired-mode
                                                 eshell-mode
                                                 eww-mode
                                                 help-mode
                                                 image-mode
                                                 iwyu-mode
                                                 magit-mode
                                                 org-mode
                                                 rtags-diagnostics-mode
                                                 rtags-rdm-mode
                                                 shell-mode
                                                 treemacs-mode)
  "List of modes for which line numbers should not be displayed."
  :group 'exordium
  :type 'list)

(defcustom exordium-inhibit-line-numbers-buffer-size nil
  "The maximum buffer size that line numbers should be displayed.
Set to some integer, to show line numbers only for buffers that are smaller
than the specified size.  I.e., `(* 512 1024)' will only display line numbers
that are smaller than 0.5MiB (this is over 6.5k 80 character lines).
Set to nil to enable line numbers even in large buffers."
  :group 'exordium
  :type 'integer)

(defcustom exordium-inhibit-line-numbers-star-buffers nil
  "Controls whether line numbers shold be displayed in buffers that name starts with `*'.
Set to t to don't display line numbers in buffers that name starts with `*'.
Set to nil to display line numbers in buffers that name starts with `*'."
  :group 'exordium
  :type 'boolean)


;;; Miscellaneous utilities -- see init-util.el

(defcustom exordium-fci-mode :ondemand
  "Controls fill-column-indicator e.g. the 80-character ruler.
:ondemand off by default (you need to hit C-| to toggle it),
:always means it is always on, :prog means on for programming modes
nil means it is disabled."
  :group 'exordium
  :type  'symbol)

(defcustom exordium-fci-use-dashes :one
  "If non-nil use dashes for fill-column-indicator.
If nil, use a plain line.  The value can be one of `:one', `:two', `:three',
`:four', or any other key from `exordium-fci-dashes-alist' to use associated
value.  Note that the character will only be used if displayable by a current
font."
  :group 'exordium
  :type  'symbol)

(defcustom exordium-fci-dashes-alist '((:one .   (?\u2758 ?\u2575)) ;; ❘ or ╵
                                       (:two .   (?\u254e))         ;; ╎
                                       (:three . (?\u2506))         ;; ┆
                                       (:four .  (?\u250a)))       ;; ┊
  "The mapping form a symbol to a sequence of characters to be used for dashes.
For each symbol the first displayable character from the sequence will be used
if displayable in current configuration.  If the value of
`exordium-fci-use-dashes' cannot be mapped or the mapped value cannot be
displayed by the current font default will be used."
  :group 'exordium
  :type  'alist)

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
  "Slect the completion engine for exordium.
Possible values are `:auto-complete',`:company', and nil.
Default is `:auto-complete'.  See also `exordium-rtags-auto-complete'."
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

(defcustom exordium-use-variable-pitch nil
  "Allow variable pitch fonts"
  :group 'exordium
  :type 'boolean)

(defcustom exordium-height-minus-1 0.8
  "Font size -1."
  :type 'number
  :group 'exordium)

(defcustom exordium-height-plus-1 1.1
  "Font size +1."
  :type 'number
  :group 'exordium)

(defcustom exordium-height-plus-2 1.2
  "Font size +2."
  :type 'number
  :group 'exordium)

(defcustom exordium-height-plus-3 1.3
  "Font size +3."
  :type 'number
  :group 'exordium)

(defcustom exordium-height-plus-4 1.4
  "Font size +4."
  :type 'number
  :group 'exordium)

(defcustom exordium-height-plus-10 2.0
  "Font size +10."
  :type 'number
  :group 'exordium)



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

(defcustom exordium-yasnippet-author
  (format "%s (%s)" user-full-name user-login-name)
  "Author string for yasnippet expansion"
  :group 'exordium
  :type  'string)


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

(defcustom exordium-git-gutter-for-remote-files nil
  "Whether a git status icon is displayed for the remote files, i.e.
opened in TRAMP mode."
  :group 'exordium
  :type  'boolean)

(defcustom exordium-git-gutter-fringe-style :flat
  "Style for git gutter fringe markers.
:default = unchanged.
:flat = doom-style"
  :group 'exordium
  :type 'symbol)

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
  "Whether RTags is used as the source for auto-complete in C++ mode.
This is considered only when `exordium-complete-mode' is `:auto-complete'."
  :group 'exordium
  :type  'boolean)

(defcustom exordium-rtags-syntax-checker :flymake
  "The syntax checker to be used with rtags. If set to :flycheck the
`flycheck-rtags' will be used. Otherwise, the built-in flymake will be used."
  :group 'exordium
  :type  'symbol)

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




;;; LSP
(defun exordium--string-vector-p (candidate)
  "Returns true if CANDIDATE is a vector data structure and
every element of it is of type string, else nil."
  (and
   (vectorp candidate)
   (seq-every-p #'stringp candidate)))

(define-widget 'exordium-string-vector 'lazy
  "A vector of zero or more elements, every element of which is a string.
Appropriate for any language-specific `defcustom' that needs to
serialize as a JSON array of strings."
  :offset 4
  :tag "Vector"
  :type '(restricted-sexp
          :match-alternatives (exordium--string-vector-p)))

(defcustom exordium-lsp-clangd-executable ["clangd-15" "clangd-14" "clangd-13" "clangd-12" "clangd-11" "clangd-10" "clangd-9" "clangd"]
  "List of executable names to search for to run clangd.
Default is to choose the first that is found via `executable-find'."
  :group 'exordium
  :risky t
  :type 'exordium-string-vector)

(defcustom exordium-lsp-clangd-args '("-j=4" "--background-index" "--log=error" "--clang-tidy")
  "Extra arguments for the clangd executable."
  :group 'exordium
  :risky t
  :type '(repeat string))

(defcustom exordium-enable-which-key t
  "If t, which-key mode will be enabled."
  :group 'exordium
  :type  'boolean)

(defcustom exordium-lsp-ui-doc-enable t
  "If t, exordium-lsp-ui-doc mode will be enabled."
  :group 'exordium
  :type  'boolean)

(defcustom exordium-lsp-ui-sideline-enable t
  "If t, exordium-lsp-ui-sideline mode will be enabled."
  :group 'exordium
  :type  'boolean)

(defcustom exordium-lsp-ui-flycheck-enable t
  "If t, exordium-lsp-ui-flycheck mode will be enabled."
  :group 'exordium
  :type  'boolean)

(defcustom exordium-lsp-ui-peek-enable t
  "If t, exordium-lsp-ui-peek mode will be enabled."
  :group 'exordium
  :type  'boolean)

(defcustom exordium-lsp-keymap-prefix "C-c l"
  "The prefix to bind the lsp keymap to, `kbd' format."
  :group 'exordium
  :type  'string)

(defcustom exordium-lsp-ui-doc-position 'at-point
  "Where to display the doc in LSP mode."
  :group 'exordium
  :type '(choice (const :tag "Top" top)
                 (const :tag "Bottom" bottom)
                 (const :tag "At point" at-point)))

(defcustom exordium-lsp-ui-flycheck-list-position 'bottom
  "Position where `lsp-ui-flycheck-list' will show diagnostics for the workspace."
  :group 'exordium
  :type '(choice (const :tag "Bottom" bottom)
                 (const :tag "Right" right)))

(defcustom exordium-lsp-mode-enable t
  "Enable lsp-mode."
  :group 'exordium
  :type 'boolean)

(provide 'init-prefs)
