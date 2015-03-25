;;;; Configuration for Python

;;; Turn on FCI automatically
(add-hook 'python-mode-hook 'fci-mode)

;;; Font lock changes
;;; Display TODO: and FIXME: and TBD: in red
(when exordium-font-lock
  (font-lock-add-keywords
   'python-mode
   '(("\\<\\(FIXME\\):" 1 font-lock-warning-face prepend)
     ("\\<\\(TBD\\):" 1 font-lock-warning-face prepend)
     ("\\<\\(TODO\\):" 1 font-lock-warning-face prepend))))


(provide 'init-python)
