;;; atom-one-theme.el --- Atom One Theme             -*- lexical-binding: t -*-

;;; Commentary:
;;
;;; Code:
(eval-when-compile
  (unless (featurep 'init-require)
    (load (file-name-concat (locate-user-emacs-file "modules") "init-require"))))

(exordium-require 'color-theme-atom-one :location "themes")

(define-atom-one-theme)

(provide 'atom-one-theme)

;;; atom-one-theme.el ends here
