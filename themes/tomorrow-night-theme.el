;;; tomorrow-night-theme.el --- Tomorrow Nithg Theme -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (unless (featurep 'init-require)
    (load (file-name-concat (locate-user-emacs-file "modules") "init-require"))))

(exordium-require 'color-theme-tomorrow "themes")

(define-tomorrow-theme night)

(provide 'tomorrow-night-theme)

;;; tomorrow-night-theme.el ends here
