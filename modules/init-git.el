;;;; All git-related stuff
;;;
;;; All keys are C-c g <one-more-key>:
;;;
;;; ----------------- ---------------------------------------------------------
;;; Key               Definition
;;; ----------------- ---------------------------------------------------------
;;; C-c g s           Magit status
;;; C-c g l           Magit log
;;; C-c g f           Magit file log
;;; C-c g b           Toggle Magit blame mode
;;;
;;; C-c g down        Goto next hunk in buffer
;;; C-c g up          Goto previous hunk in buffer
;;; C-c g d           Diff current hunk
;;; C-c g r           Revert current hunk (asks for confirmation)

;;; Magit
(require 'magit)

;;; Keys
(define-key global-map [(control c)(g)(s)] (function magit-status))
(define-key global-map [(control c)(g)(l)] (function magit-log))
(define-key global-map [(control c)(g)(f)] (function magit-file-log))
(define-key global-map [(control c)(g)(b)] (function magit-blame-mode))

;;; Make `magit-status' run alone in the frame, and then restore the old window
;;; configuration when you quit out of magit.
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

;;; Make `magit-log' run alone in the frame, and then restore the old window
;;; configuration when you quit out of magit.
(defadvice magit-log (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

;;; Don't show "MRev" in the modeline
(diminish 'magit-auto-revert-mode)


;;; Git gutter fringe: display added/removed/changed lines in the left fringe.

(when exordium-git-gutter-non-fringe
  (setq exordium-git-gutter nil)
  (require 'git-gutter)
  (global-git-gutter-mode t)
  (git-gutter:linum-setup)
  (diminish 'git-gutter-mode))

(when (and exordium-git-gutter (not exordium-git-gutter-non-fringe))
  (require 'git-gutter-fringe)
  (global-git-gutter-mode t)
  (diminish 'git-gutter-mode))

;; keys
(when (or exordium-git-gutter exordium-git-gutter-non-fringe)
  (define-key global-map [(control c)(g)(down)] 'git-gutter:next-hunk)
  (define-key global-map [(control c)(g)(up)] 'git-gutter:previous-hunk)
  (define-key global-map [(control c)(g)(d)] 'git-gutter:popup-hunk)
  (define-key global-map [(control c)(g)(r)] 'git-gutter:revert-hunk))

(provide 'init-git)
