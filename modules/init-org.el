;;;; Org mode

(setq org-todo-keywords
      '((sequence "TODO" "WORK" "WAIT" "DONE")))
(setq org-startup-truncated nil)

;;; Native formatting in block codes
(setq org-src-fontify-natively t)

;;; Show images inline
(setq org-startup-with-inline-images t)

;;; Show org-mode bullets as UTF-8 characters.
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(provide 'init-org)
