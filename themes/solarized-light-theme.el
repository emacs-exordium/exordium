;;; solarized-light-theme.el --- Solarized Light Theme -*- lexical-binding: t -*-

;;; Commentary:
;;
;;; Code:
(eval-when-compile
  (unless (featurep 'init-require)
    (load (file-name-concat (locate-user-emacs-file "modules") "init-require"))))

(exordium-require 'color-theme-solarized :location "themes")

(define-solarized-theme light)

(provide 'solarized-light-theme)

;;; solarized-light-theme.el ends here
