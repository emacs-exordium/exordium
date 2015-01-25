;;;; Configuration for Emacs Lisp

;;; Loud face for TODOs in elisp comments

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '(("\\<\\(TODO\\|FIXME\\|TBD\\):" 1 font-lock-warning-face t)))))

(provide 'init-elisp)
