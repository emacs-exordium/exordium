;;; material-theme.el --- Material Theme             -*- lexical-binding: t -*-

;;; Commentary:
;;
;;; Code:
(eval-when-compile
  (unless (featurep 'init-require)
    (load (file-name-concat (locate-user-emacs-file "modules") "init-require"))))

(exordium-require 'color-theme-material :location "themes")

(define-material-theme)

(provide 'material-theme)

;;; material-theme.el ends here
