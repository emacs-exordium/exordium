;;;; All git-related stuff
;;;
;;; ----------------- ---------------------------------------------------------
;;; Key               Definition
;;; ----------------- ---------------------------------------------------------
;;; C-x g             Magit status
;;; C-c g             Toggle git gutter

;;; Ctrl-x g = magit-status
(define-key global-map [(control x)(g)] 'magit-status)

;;; Git gutter fringe: display added/removed/changed lines in the left fringe.
(when *init-git-gutter*
  (require 'git-gutter-fringe)
  (global-set-key [(control c)(g)] 'git-gutter:toggle)
  (global-git-gutter-mode t)
  (diminish 'git-gutter-mode))

(provide 'init-git)
