;;;; Org mode

(setq org-todo-keywords
      '((sequence "TODO" "WORK" "WAIT" "DONE")))
(setq org-startup-truncated nil)

;;; Native formatting in block codes
(setq org-src-fontify-natively t)

(provide 'init-org)
