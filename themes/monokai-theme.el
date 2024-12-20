;;; monokai-theme.el --- Monokai Theme               -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (unless (featurep 'init-require)
    (load (file-name-concat (locate-user-emacs-file "modules") "init-require"))))

(exordium-require 'color-theme-monokai :location "themes")

(define-monokai-theme)

(provide 'monokai-theme)

;;; monokai-theme.el ends here
