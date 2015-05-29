;;;; Configuration for Emacs Lisp

;;; Bind M-. to find-function instead of find-tag
(define-key emacs-lisp-mode-map [(meta .)] 'find-function)

;;; Bind M-C-g to helm-imenu (lists functions and variables in buffer)
(define-key emacs-lisp-mode-map [(meta control g)] 'helm-imenu)

(provide 'init-elisp)
