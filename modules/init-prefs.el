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
(defcustom exordium-preferred-frame-height 65
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

;;; Keyboard

(defcustom exordium-keyboard-escape t
  "Whether the Escape key does keyboard quit e.g. the equivalent of Ctrl-G."
  :group 'exordium
  :type  'boolean)

(defcustom exordium-enable-newline-and-indent t
  "If t, binds the return key to newline-and-indent, and
shift-return for just newline.  If nil, do the opposite."
  :group 'exordium
  :type  'boolean)

(defcustom exordium-enable-electric-pair-mode t
  "Enables electric-pair-mode."
  :group 'exordium
  :type  'symbol)

(defcustom exordium-enable-cua-mode nil
  "If set to t, enables CUA mode.
If set to :region, enables CUA rectagular regions.
IF nil, disables CUA completely."
  :group 'exordium
  :type  'symbol)

;;; Backup files (e.g. file~)
(defcustom exordium-backup-files nil
  "Enables or disables backup files. Disabled by default, I can't
  stand these annoying files~"
  :group 'exordium
  :type  'symbol)


;;; Line numbers - see init-linum.el

(defcustom exordium-display-line-numbers t
  "Whether line numbers are displayed or not"
  :group 'exordium
  :type  'boolean)


;;; Miscellaneous utilities -- see init-util.el

(defcustom exordium-fci-mode :ondemand
  "Controls fill-column-indicator e.g. the 80-character ruler.
:ondemand off by default (you need to hit C-| to toggle it),
:always means it is always on,
nil means it is disabled."
  :group 'exordium
  :type  'symbol)

(defcustom exordium-fci-use-dashes t
  "If t, use dashes for fill-column-indicator.
If nil, use a plain line."
  :group 'exordium
  :type  'boolean)

(defcustom exordium-highlight-symbol nil
  "Whether occurrences of the symbol under point gets highlighted
  in the buffer after a few seconds."
  :group 'exordium
  :type  'boolean)


;;; Autocomplete -- see init-autocomplete.el

(defcustom exordium-auto-complete t
  "Whether auto-complete is turned on or off by default. See also
  `exordium-rtags-auto-complete'."
  :group 'exordium
  :type  'boolean)


;;; Themes -- see themes directory

(defcustom exordium-theme 'tomorrow-night
  "Theme to load on startup. Use nil to not load any theme,
otherwise use one of the following:
- tomorrow-day, tomorrow-night, tomorrow-night-bright,
  tomorrow-night-blue, tomorrow-night-eighties
- monokai
- solarized-light, solarized-dark
- zenburn"
  :group 'exordium
  :type  'symbol)

(defcustom exordium-highlight-linum t
  "Whether the current line number is highlighted.
This does not work with nlinum-mode currently."
  :group 'exordium
  :type  'boolean)

(defcustom exordium-enable-powerline t
  "Enables Powerline"
  :group 'exordium
  :type  'boolean)

(defcustom exordium-powerline-shows-rtags-diagnostics t
  "Whether Powerline shows RTags Diagnostics results. If there
are errors, the buffer name is displayed in red instead of the
default color."
  :group 'exordium
  :type  'boolean)


;;; Programming

;;; See init-rtags.el
(defcustom exordium-rtags-auto-complete nil
  "Whether RTags is used as the source for auto-complete in C++ mode."
  :group 'exordium
  :type  'boolean)

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

;;; See init-clojure.el
(defcustom exordium-clojure nil
  "Whether the development environment for Clojure is enabled or not."
  :group 'exordium
  :type  'boolean)


;;; Desktop state
(defcustom exordium-desktop nil
  "Whether the desktop state is saved"
  :group 'exordium
  :type  'boolean)


(provide 'init-prefs)
