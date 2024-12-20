;;; init-xml.el --- Configuration of the nxml mode -*- lexical-binding: t -*-


;;; Commentary:
;;

;;; Code:
(use-package nxml-mode
  :ensure nil
  :defer t
  ;; Automatically close tags when typing </ (electric slash)
  :custom
  (custom nxml-slash-auto-complete-flag t))

(provide 'init-xml)

;;; init-xml.el ends here
