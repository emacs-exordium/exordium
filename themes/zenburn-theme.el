;;; zenburn-theme.el --- Zenburn Theme               -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (unless (featurep 'init-require)
    (load (file-name-concat (locate-user-emacs-file "modules") "init-require"))))

(exordium-require 'color-theme-zenburn :location "themes")

(define-zenburn-theme)

(provide 'zenburn-theme)

;;; zenburn-theme.el ends here
