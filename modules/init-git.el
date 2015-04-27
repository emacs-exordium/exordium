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
(define-prefix-command 'exordium-git-map nil)
(define-key exordium-git-map (kbd "s") (function magit-status))
(define-key exordium-git-map (kbd "l") (function magit-log))
(define-key exordium-git-map (kbd "f") (function magit-file-log))
(define-key exordium-git-map (kbd "b") (function magit-blame-mode))
(global-set-key (kbd "C-c g") 'exordium-git-map)

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


;;; Turn off the horrible warning about magit auto-revert of saved buffers
(setq magit-last-seen-setup-instructions "1.4.0")


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
  (define-key exordium-git-map (kbd "<down>") 'git-gutter:next-hunk)
  (define-key exordium-git-map (kbd "<up>") 'git-gutter:previous-hunk)
  (define-key exordium-git-map (kbd "d") 'git-gutter:popup-hunk)
  (define-key exordium-git-map (kbd "r") 'git-gutter:revert-hunk))


;;; Git Timemachine

(define-key exordium-git-map (kbd "t") 'git-timemachine-toggle)



(provide 'init-git)
