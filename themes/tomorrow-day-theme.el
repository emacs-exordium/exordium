;;; tomorrow-day-theme.el --- Tomorrow Day Theme     -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (unless (featurep 'init-require)
    (load (file-name-concat (locate-user-emacs-file "modules") "init-require"))))

(exordium-require 'color-theme-tomorrow :location "themes")

(define-tomorrow-theme day)

(provide 'tomorrow-day-theme)

;;; tomorrow-day-theme.el ends here
