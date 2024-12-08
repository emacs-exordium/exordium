;;; tomorrow-night-eighties-theme.el --- Tomorrow Night Eighties Theme -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (unless (featurep 'init-require)
    (load (file-name-concat (locate-user-emacs-file "modules") "init-require"))))

(exordium-require 'color-theme-tomorrow :location "themes")

(define-tomorrow-theme night-eighties)

(provide 'tomorrow-night-eighties-theme)

;;; tomorrow-night-eighties-theme.el ends here
