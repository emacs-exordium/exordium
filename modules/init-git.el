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
;;; C-c g c           Magit clone
;;;
;;; C-c g down        Goto next hunk in buffer
;;; C-c g n           Goto next hunk in buffer
;;; C-c g up          Goto previous hunk in buffer
;;; C-c g p           Goto previous hunk in buffer
;;; C-c g d           Diff current hunk
;;; C-c g r           Revert current hunk (asks for confirmation)

;;; Magit
(define-prefix-command 'exordium-git-map nil)
(global-set-key (kbd "C-c g") 'exordium-git-map)

(use-package magit
  :init
  (defun exordium-magit-log-buffer ()
    (interactive)
    (if (fboundp 'magit-log-buffer-file)
        (call-interactively 'magit-log-buffer-file)
      (call-interactively 'magit-file-log)))

  (defun exordium-magit-blame ()
    (interactive)
    (if (fboundp 'magit-blame)
        (call-interactively 'magit-blame)
      (call-interactively 'magit-blame-mode)))

  (defun exordium-magit-log ()
    "If in `dired-mode', call `magit-dired-log'. Otherwise call
`magit-log-current (or `magit-log' if former not present)."
    (interactive)
    (if (eq 'dired-mode major-mode)
        (call-interactively 'magit-dired-log)
      (if (fboundp 'magit-log-current)
          (call-interactively 'magit-log-current)
        (call-interactively 'magit-log))))

  (defun magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer"
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen))

  ;;; Turn off the horrible warning about magit auto-revert of saved buffers
  (setq magit-last-seen-setup-instructions "1.4.0")

  :bind
  (:map exordium-git-map
        ("s" . (function magit-status))
        ("l" . 'exordium-magit-log)
        ("f" . 'exordium-magit-log-buffer)
        ("b" . 'exordium-magit-blame)
        ("c" . (function magit-clone))
   :map magit-status-mode-map
        ("q" . 'magit-quit-session))

  :config
;;; Make `magit-status',`exordium-magit-log' (a wrapper around `magit-log' and
;;; `magit-dired-log'), `magit-status-setup-buffer' (called from `magit-clone'),
;;; and `magit-status-internal' (called from `projectile-vc') to run alone in
;;; the frame, and then restore the old window configuration when you quit out
;;; of magit.
  (when exordium-use-magit-fullscreen
    (defun exordium-define-advice-magit-fullscreen (symbol)
      (cl-flet ((advice (orig-fun &rest args)
                        (window-configuration-to-register :magit-fullscreen)
                        (apply orig-fun args)
                        (delete-other-windows)))
        (advice-add symbol :around #'advice)))
    (exordium-define-advice-magit-fullscreen 'magit-status)
    (exordium-define-advice-magit-fullscreen 'exordium-magit-log)
    (exordium-define-advice-magit-fullscreen 'magit-status-setup-buffer)
    (when (fboundp 'magit-status-internal) ;; check just like in `projectile-vc'
      (exordium-define-advice-magit-fullscreen 'magit-status-internal)))

  (define-advice magit-clone-regular (:after
                                      (_repo directory _args)
                                      exordium-projectile-add-known-project)
    (projectile-add-known-project directory)))


;;; Don't show "MRev" in the modeline
(when (bound-and-true-p magit-auto-revert-mode)
  (diminish 'magit-auto-revert-mode))




;;; Git gutter fringe: display added/removed/changed lines in the left fringe.

;;;###autoload
(define-globalized-minor-mode exordium-global-git-gutter-mode
  git-gutter-mode
  (lambda () (when (let ((file-name (buffer-file-name)))
                     (if exordium-git-gutter-for-remote-files
                         file-name ;; enable for all files
                       (and file-name ;; enable only for local files
                            (not (file-remote-p file-name)))))
               (git-gutter--turn-on))))

(use-package git-gutter
  :if exordium-git-gutter-non-fringe
  :init
  (setq exordium-git-gutter nil)
  :config
  (exordium-global-git-gutter-mode t)
  (git-gutter:linum-setup)
  :diminish)

(use-package git-gutter-fringe
  :if (and exordium-git-gutter (not exordium-git-gutter-non-fringe))
  :config (exordium-global-git-gutter-mode t)
  :diminish git-gutter-mode
  :bind (:map exordium-git-map
              ("<down>" . 'git-gutter:next-hunk)
              ("n" . 'git-gutter:next-hunk)
              ("<up>" . 'git-gutter:previous-hunk)
              ("p" . 'git-gutter:previous-hunk)
              ("d" . 'git-gutter:popup-hunk)
              ("r" . 'git-gutter:revert-hunk))
  :init
  (add-hook 'git-gutter:update-hooks 'magit-revert-buffer-hook))



;;; Git Timemachine
(use-package git-timemachine
  :defer t
  :bind
  (:map exordium-git-map ("t" . 'git-timemachine-toggle)))

;;; Magit Forge
(use-package forge
  :defer t)


;;; Git Grep

(define-key exordium-git-map (kbd "g")
  (if exordium-helm-everywhere
      (lambda()
        (interactive)
        (setq current-prefix-arg '(4))
        (call-interactively 'helm-grep-do-git-grep))
    (function vc-git-grep)))


;;; Make backtick an electric pair
(require 'init-lib)

(add-hook 'git-commit-mode-hook 'exordium-electric-mode-add-back-tick)


(provide 'init-git)
