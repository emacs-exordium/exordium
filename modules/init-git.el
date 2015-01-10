;;;; All git-related stuff
;;;
;;; ----------------- ---------------------------------------------------------
;;; Key               Definition
;;; ----------------- ---------------------------------------------------------
;;; C-c g             Magit status

(require 'magit)

;;; Ctrl-c g = magit-status
(define-key global-map [(control c)(g)] 'magit-status)

;;; Git gutter fringe: display added/removed/changed lines in the left fringe.
(when *init-git-gutter*
  (require 'git-gutter-fringe)
  (global-git-gutter-mode t)
  (diminish 'git-gutter-mode))

;;; Make `magit-status' run alone in the frame, and then restore the old window
;;; configuration when you quit out of magit.
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

(provide 'init-git)
