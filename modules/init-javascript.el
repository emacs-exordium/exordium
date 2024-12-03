;;; init-javascript.el --- JavaScript mode           -*- lexical-binding: t -*-

;;; Commentary:
;;
;; `js2-mode' comes with some useful utility functions for working with .js
;; files more efficiently.  For example, `ac-js2-jump-to-definition' quickly
;; jumps to the definition of one variable/function that is defined in the same
;; file, `js2-mark-defun' selects the current function.  You can use the
;; command `apropos-command' to list all js2 commands.

;;; Code:
(eval-when-compile
  (unless (featurep 'init-require)
    (load (file-name-concat (locate-user-emacs-file "modules") "init-require"))))
(exordium-require 'init-prefs)

(use-package js2-mode
  ;; Open JSON files with j2 modes by default, instead of the built-in
  ;; javascript mode.
  :hook (js . js-minor-mode)
  :mode "\\.json'"
  :custom
  ;; Define some RDEL symbols
  (js2-global-externs
   (mapcar 'symbol-name '(BB
                          debug
                          assert
                          activebase)))
  :bind
  (:map js-mode-map
   ("C-M-g" . #'helm-imenu))
  :config
  ;; js2-mode provides 4 level of syntax highlighting. They are:
  ;; - 0 or a negative value means none.
  ;; - 1 adds basic syntax highlighting.
  ;; - 2 adds highlighting of some Ecma built-in properties.
  ;; - 3 adds highlighting of many Ecma built-in functions.
  (unless exordium-treesit-modes-enable
    (if exordium-font-lock
        (setq js2-highlight-level 3)
      (setq js2-highlight-level -1)))
  (js2-imenu-extras-mode))

(when (eq exordium-complete-mode :auto-complete)
  (use-package ac-js2
    :hook
    (js2-mode . ac-js2-mode)))


(provide 'init-javascript)

;;; init-javascript.el ends here
