;;; catppuccin-mocha-theme.el --- Catppuccin Theme -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (unless (featurep 'init-require)
    (load (file-name-concat (locate-user-emacs-file "modules") "init-require"))))

(exordium-require 'color-theme-catppuccin :location "themes")

(define-catppuccin-theme mocha)

(provide 'catppuccin-mocha-theme)

;;; catppuccin-mocha-theme.el ends here
