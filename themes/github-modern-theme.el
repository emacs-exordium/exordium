;;; github-modern-theme.el --- GitHub Modern Theme   -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (unless (featurep 'init-require)
    (load (file-name-concat (locate-user-emacs-file "modules") "init-require"))))

(exordium-require 'color-theme-github-modern "themes" define-github-modern-theme)

(define-github-modern-theme)

(provide 'github-modern-theme)

;;; github-modern-theme.el ends here
