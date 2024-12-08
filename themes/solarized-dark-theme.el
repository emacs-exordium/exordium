;;; solarized-dark-theme.el --- Solarized Dark Theme -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (unless (featurep 'init-require)
    (load (file-name-concat (locate-user-emacs-file "modules") "init-require"))))

(exordium-require 'color-theme-solarized :location "themes")

(define-solarized-theme dark)

(provide 'solarized-dark-theme)

;;; solarized-dark-theme.el ends here
