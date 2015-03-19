;;;; Configuration for Emacs Lisp

;;; Bind M-. to find-function instead of find-tag
(define-key emacs-lisp-mode-map [(meta .)] 'find-function)

;;; Bind M-C-g to helm-imenu (lists functions and variables in buffer)
(define-key emacs-lisp-mode-map [(meta control g)] 'helm-imenu)

;;; Loud face for TODOs in elisp comments
(when *init-font-lock*
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (font-lock-add-keywords
               nil
               '(("\\<\\(TODO\\|FIXME\\|TBD\\):" 1 font-lock-warning-face t))))))

(provide 'init-elisp)
