;;;; All git-related stuff
;;;
;;; ----------------- ---------------------------------------------------------
;;; Key               Definition
;;; ----------------- ---------------------------------------------------------
;;; C-c g             Magit status

;;; Ctrl-c g = magit-status
(define-key global-map [(control c)(g)] 'magit-status)

;;; Git gutter fringe: display added/removed/changed lines in the left fringe.
(when *init-git-gutter*
  (require 'git-gutter-fringe)
  (global-git-gutter-mode t)
  (diminish 'git-gutter-mode))

(provide 'init-git)
