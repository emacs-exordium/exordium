;;;; Configuration for Emacs Lisp

;;; Bind M-. to find-function instead of find-tag
(define-key emacs-lisp-mode-map [(meta .)] 'find-function)

;;; Loud face for TODOs in elisp comments

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '(("\\<\\(TODO\\|FIXME\\|TBD\\):" 1 font-lock-warning-face t)))))

(provide 'init-elisp)
