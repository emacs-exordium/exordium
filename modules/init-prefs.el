;;;; Definition of the variables that init-local-prefs.el can override.
;;;; These variables control other init-XXX files, such as init-ui.el.

(defvar *init-preferred-fonts*
  '(("Consolas"  . 120)
    ("Monaco"    . 120)
    ("Monospace" . 120)
    ("Mono"      . 120))
  "List preferred fonts/sizes to use, in decreasing order of
  preference. We will use the first one that is available on the
  local machine. It is a list of pairs (font-name
  . font-size). Note that you can get the list of available font
  names by evaluating (font-family-list)")

(defvar *init-preferred-frame-width* 100
  "Default frame width (number of columns)")

(defvar *init-preferred-frame-height* 65
  "Default frame height (a number of lines)")

(provide 'init-prefs)
