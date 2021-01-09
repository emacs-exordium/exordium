;;;; Configuration for Emacs Lisp

;;; Bind M-. to find-function instead of find-tag
(define-key emacs-lisp-mode-map [(meta .)] 'find-function)

;;; Bind M-C-g to helm-imenu (lists functions and variables in buffer)
(define-key emacs-lisp-mode-map [(meta control g)] 'helm-imenu)

(defun exordium-page-break-lines-hook ()
  "Enable `page-break-lines' mode.
When in TUI enable line truncation as well to prevent a rendering
bug (page break lines wrap around)."
  (unless (display-graphic-p)
    (set (make-local-variable 'truncate-lines) t))
  (page-break-lines-mode))

;;; Display page breaks with an horizontal line instead of ^L.
;;; Note: To insert a page break: C-q C-l
;;;       To jump to the previous/next page break: C-x [ and C-x ]
(use-package page-break-lines
  :diminish
  :hook
  (emacs-lisp-mode . exordium-page-break-lines-hook))

;;; Animation when evaluating a defun or a region:
(use-package highlight)
(use-package eval-sexp-fu)

(provide 'init-elisp)
