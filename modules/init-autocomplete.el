;;; init-autocomplete.el --- Autocomplete -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(eval-when-compile
  (unless (featurep 'init-require)
    (load (file-name-concat (locate-user-emacs-file "modules") "init-require"))))
(exordium-require 'init-prefs)

(use-package auto-complete
  :commands (ac-stop
             ac-complete)
  :defer t
  :custom
  ;; Case sensitivity is important when finding matches
  ;; Values are: t, nil, or 'smart
  (ac-ignore-case nil)
  ;; Start auto-completion after 2 characters of a word
  ;; Values are: an integer, or nil to disable
  (ac-auto-start 2)
  :config
  ;; Default config for auto-complete
  (ac-config-default)
  :bind
  (;; Key to force trigger auto-complete (useful if ac-auto-start is set to nil)
   ("C-." . #'auto-complete)
   :map ac-completing-map
   ("ESC" . #'ac-stop)
   ("RET" . #'ac-complete)))

(provide 'init-autocomplete)

;;; init-autocomplete.el ends here
