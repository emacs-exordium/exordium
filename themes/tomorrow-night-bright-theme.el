;;; tomorrow-night-bright-theme.el --- Tomorrow Night Bright Theme -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (unless (featurep 'init-require)
    (load (file-name-concat (locate-user-emacs-file "modules") "init-require"))))

(exordium-require 'color-theme-tomorrow :location "themes")

(define-tomorrow-theme night-bright)

(provide 'tomorrow-night-bright-theme)

;;; tomorrow-night-bright-theme.el ends here
