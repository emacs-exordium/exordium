;;;; Definition of the variables that prefs.el can override.
;;;
;;; These variables control other init-XXX files, such as init-ui.el.
;;; You can override them by creating your own file ~/.emacs.d/prefs.el
;;; with content like:
;;;
;;; (setq *init-preferred-frame-width*  110
;;;       *init-preferred-frame-height* 75)
;;;
;;; You can also use M-x `customize' and browse group Local -> Init.

(defgroup init nil
  "Customize your Emacs configuration."
  :group 'local)


;;; UI -- see init-look-and-feel.el

(defcustom *init-preferred-fonts* '(("Consolas"  . 120)
                                    ("Monaco"    . 120)
                                    ("Monospace" . 120)
                                    ("Mono"      . 120))
  "List of preferred fonts/sizes to use, in decreasing order of
preference. We will use the first one that is available on the
local machine. It is a list of pairs (font-name . font-size). If
nil, we don't set any font. Note that you can get the list of
available font names by evaluating (font-family-list)."
  :group 'init
  :type 'sexp)

(defcustom *init-preferred-frame-width* 100
  "Default frame width (number of columns)."
  :group 'init
  :type 'integer)

;; Note: a weird bug in OS X makes the title bar black if height is too big
(defcustom *init-preferred-frame-height* 65
  "Default frame height (a number of lines)."
  :group 'init
  :type 'integer)

(defcustom *init-display-line-numbers* t
  "Whether line numbers are displayed or not"
  :group 'init
  :type 'boolean)

(defcustom *init-line-mode* t
  "Whether the current line is highlighted or not."
  :group 'init
  :type 'boolean)


;;; Extensions -- see init-extensions.el

(defcustom *init-enable-electric-pair-mode* t
  "Enables electric-pair-mode."
  :group 'init
  :type 'symbol)

(defcustom *init-fci-mode* :ondemand
  "Controls fill-column-indicator e.g. the 80-character ruler.
:ondemand off by default (you need to hit C-| to toggle it),
:always means it is always on,
nil means it is disabled."
  :group 'init
  :type 'symbol)

(defcustom *init-fci-use-dashes* t
  "If t, use dashes for fill-column-indicator.
If nil, use a plain line."
  :group 'init
  :type 'boolean)


;;; Themes -- see themes directory

(defcustom *init-theme* 'tomorrow-night
  "Theme to load on startup. Use nil to not load any theme,
otherwise use one of the following: tomorrow-day, tomorrow-night,
tomorrow-night-bright, tomorrow-night-blue,
tomorrow-night-eighties, monokai, solarized-light,
solarized-dark."
  :group 'init
  :type 'symbol)

(defcustom *init-highlight-linum* t
  "Whether the current line number is highlighted.
This does not work with nlinum-mode currently."
  :group 'init
  :type 'boolean)

(defcustom *init-enable-powerline* t
  "Enables Powerline"
  :group 'init
  :type 'boolean)

(defcustom *init-powerline-shows-rtags-diagnostics* t
  "Whether Powerline shows RTags Diagnostics results. If there
are errors, the buffer name is displayed in red instead of the
default color."
  :group 'init
  :type 'boolean)


;;; Keyboard -- see init-keyboard.el

(defcustom *init-enable-newline-and-indent* t
  "If t, binds the return key to newline-and-indent, and
shift-return for just newline.  If nil, do the opposite."
  :group 'init
  :type 'boolean)

(defcustom *init-enable-cua-mode* nil
  "IF set to t, enables CUA mode.
If set to :region, enables CUA rectagular regions.
IF nil, disables CUA completely."
  :group 'init
  :type 'symbol)

(defcustom *init-keyboard-escape* t
  "Whether the Escape key does keyboard quit e.g. the equivalent of Ctrl-G."
  :group 'init
  :type 'boolean)


;;; Programming

;;; See init-rtags.el
(defcustom *init-rtags-auto-complete* nil
  "Whether RTags is used as the source for auto-complete in C++ mode."
  :group 'init
  :type 'boolean)

;;; See init-yasnippet.el
(defcustom *init-yasnippet* t
  "Whether YASnippet is enabled or not."
  :group 'init
  :type 'boolean)

;;; See init-helm-projectile.el
(defcustom *init-helm-projectile* t
  "Whether Helm and Projectile are enabled or not."
  :group 'init
  :type 'boolean)

;;; See init-git.el
;;; *init-git-gutter* displays a git diff icon in the left fringe, while
;;; *init-git-gutter-non-fringe* displays the diff icon on the left side of the
;;; *line numbers. The latter takes precedence.
(defcustom *init-git-gutter-non-fringe* nil
  "Whether a git status icon is displayed at the very left. Takes
  precedence over *init-gut-gutter*. Note that this option
  disables highlighting the current line number"
  :group 'init
  :type 'boolean)

(defcustom *init-git-gutter* t
  "Whether a git status icon is displayed in the left-side fringe or not."
  :group 'init
  :type 'boolean)

;;; See init-clojure.el
(defcustom *init-clojure* nil
  "Whether the development environment for Clojure is enabled or not."
  :group 'init
  :type 'boolean)


(provide 'init-prefs)
