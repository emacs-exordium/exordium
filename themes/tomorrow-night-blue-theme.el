;;; tomorrow-night-blue-theme.el --- Tomorrow Night Blue Theme -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (unless (featurep 'init-require)
    (load (file-name-concat (locate-user-emacs-file "modules") "init-require"))))

(exordium-require 'color-theme-tomorrow :location "themes")

(define-tomorrow-theme night-blue)

(provide 'tomorrow-night-blue-theme)

;;; tomorrow-night-blue-theme.el ends here
