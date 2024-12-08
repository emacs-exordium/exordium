;;; init-clojure.el --- Let's give Light Table a run for its money -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Installed packages (and dependencies):
;; - cider
;; - rainbow-delimiters
;; - paredit
;; And you need 'lein' from leiningen.org in your shell PATH.
;; Note that clojure.el is in subdir 'extensions'.
;;
;; Usage:
;;   M-x cider-jack-in
;;   (Ignore the error when it starts).  Use M-C-x to evaluate Clojure forms.
;;   C-c C-q to quit in the REPL (or use the CIDER menu)

;;; Code:
(eval-when-compile
  (unless (featurep 'init-require)
    (load (file-name-concat (locate-user-emacs-file "modules") "init-require"))))
(exordium-require 'init-environment)

(use-package clojure-mode
  :defer t)
(use-package cider
  :defer t)
(use-package rainbow-delimiters
  :defer t)
(use-package paredit
  :defer t)

(unless exordium-osx ; otherwise it is in init-osx.el
  (use-package exec-path-from-shell
    :config
    (exec-path-from-shell-initialize)))

(exordium-require 'clojure :location "extensions")

;;; Hippie expand - don't try to complete with file names
(setq hippie-expand-try-functions-list
      (delete 'try-complete-file-name hippie-expand-try-functions-list))
(setq hippie-expand-try-functions-list
      (delete 'try-complete-file-name-partially hippie-expand-try-functions-list))

(provide 'init-clojure)

;;; init-clojure.el ends here
