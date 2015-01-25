;;;; Definition of the variables that init-local-prefs.el can override.
;;;
;;; These variables control other init-XXX files, such as init-ui.el.
;;; You can override them by creating your own file
;;; ~/.emacs.d/init-local-prefs.el with content like:
;;;
;;; (setq *init-preferred-frame-width*  110
;;;       *init-preferred-frame-height* 75)


;;; UI

(defvar *init-preferred-fonts*
  '(("Consolas"  . 120)
    ("Monaco"    . 120)
    ("Monospace" . 120)
    ("Mono"      . 120))
  "List of preferred fonts/sizes to use, in decreasing order of
  preference. We will use the first one that is available on the
  local machine. It is a list of pairs (font-name
  . font-size). If nil, we don't set any font. Note that you can
  get the list of available font names by
  evaluating (font-family-list)")

(defvar *init-preferred-frame-width* 100
  "Default frame width (number of columns)")

;; Note: a weird bug in OS X makes the title bar black if height is too big
(defvar *init-preferred-frame-height* 65
  "Default frame height (a number of lines)")

(defvar *init-display-line-numbers* t
  "Enables display of the column number")

(defvar *init-line-mode* t
  "Highlights the current line")

(defvar *init-theme* 'tomorrow-night
  "Theme to load on startup. Use nil to not load any theme,
  otherwise use one of the following:
  tomorrow-day, tomorrow-night, tomorrow-night-bright, tomorrow-night-blue,
  tomorrow-night-eighties,
  monokai,
  solarized-light, solarized-dark")


;;; Extensions

(defvar *init-enable-electric-pair-mode* t
  "Enables electric-pair-mode")

(defvar *init-fci-mode* :ondemand
  "Controls fill-column-indicator.
:ondemand off by default (you need to hit C-| to toggle it)
:always means it is always on
nil means it is disabled.")

(defvar *init-fci-use-dashes* t
  "If t, use dashes for fill-column-indicator.
If nil, use a plain line.")


;;; Theme

(defvar *init-enable-powerline* t
  "Enables Powerline")


;;; Keyboard

(defvar *init-enable-newline-and-indent* t
  "If t, binds the return key to newline-and-indent, and
  shift-return for just newline.
  If nil, do the opposite")

(defvar *init-enable-cua-mode* nil
  "IF set to t, enables CUA mode.
  If set to :region, enables CUA rectagular regions.
  IF nil, disables CUA completely")

(defvar *init-keyboard-escape* t
  "Enables ESC key for keyboard quit")


;;; C++

(defvar *init-header-auto-complete* nil
  "Enables auto-complete for header files (non-RTags)")

(defvar *init-rtags-auto-complete* nil
  "Enables RTags as the source for auto-complete")


;;; Others

(defvar *init-yasnippet* nil
  "Enables YASnippet")

(defvar *init-helm-projectile* t
  "Enables Helm and Projectile")

(defvar *init-git-gutter* t
  "Enables git gutter fringe")

(defvar *init-clojure* nil
  "Enables clojure")


(provide 'init-prefs)
