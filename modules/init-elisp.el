;;;; Configuration for Emacs Lisp

;;; Bind M-. to find-function instead of find-tag
(define-key emacs-lisp-mode-map [(meta .)] 'find-function)

;;; Bind M-C-g to helm-imenu (lists functions and variables in buffer)
(define-key emacs-lisp-mode-map [(meta control g)] 'helm-imenu)

;;; Display page breaks with an horizontal line instead of ^L.
;;; Note: To insert a page break: C-q C-l
;;;       To jump to the previous/next page break: C-x [ and C-x ]
(require 'page-break-lines)

;;; Animation when evaluating a defun or a region:
(require 'highlight)
(require 'eval-sexp-fu)

(defun exordium-elisp-mode-hook ()
  "Hook for elisp mode. Enables `page-break-lines' mode and
enables line truncation as well to prevent a rendering bug (page
break lines wrap around)."
  (set (make-local-variable 'truncate-lines) t)
  (turn-on-page-break-lines-mode)
  (diminish 'page-break-lines-mode))

(add-hook 'emacs-lisp-mode-hook #'exordium-elisp-mode-hook)

(provide 'init-elisp)
