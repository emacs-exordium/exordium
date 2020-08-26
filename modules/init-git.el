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


(use-package ediff
  :ensure nil
  :defer t
  :config
  (defun exordium-ediff-copy-both-to-C (first second)
    "Copy FIRST then SECOND into the C buffer in `ediff-mode'.
This command should be called form `ediff''s control buffer.

Adapted from: http://stackoverflow.com/questions/9656311/conflict-resolution-with-emacs-ediff-how-can-i-take-the-changes-of-both-version"
    (interactive
     (let ((first-string (completing-read "First: " '("A" "B") nil t "A"))
           (second-string (completing-read "Second: " '("A" "B") nil t "B")))
       (list first-string second-string)))
    (let ((first (or first "A"))
          (second (or second "B")))
      (ediff-copy-diff ediff-current-difference nil 'C nil
                       (concat
                        (ediff-get-region-contents
                         ediff-current-difference (intern first) ediff-control-buffer)
                        (ediff-get-region-contents
                         ediff-current-difference (intern second) ediff-control-buffer)))))

  (defun exordium--add-copy-both-to-ediff-mode-map ()
    (when ediff-merge-job
      (define-key ediff-mode-map "A"
        #'(lambda ()
            (interactive)
            (exordium-ediff-copy-both-to-C "A" "B")))
      (define-key ediff-mode-map "B"
        #'(lambda ()
            (interactive)
            (exordium-ediff-copy-both-to-C "B" "A")))))

  (defconst exordium--ediff-long-help-message-merge
    "
p,DEL -previous diff |     | -vert/horiz split   |  x -copy buf X's region to C
n,SPC -next diff     |     h -highlighting       |  X -copy both buf's regions
    j -jump to diff  |     @ -auto-refinement    |     to C; X's region first
   gx -goto X's point|    ## -ignore whitespace  |  r -restore buf C's old diff
  C-l -recenter      | #f/#h -focus/hide regions |  * -refine current region
  v/V -scroll up/dn  |     X -read-only in buf X |  ! -update diff regions
  </> -scroll lt/rt  |     m -wide display       |  + -combine diff regions
    ~ -swap variants |     s -shrink window C    | wx -save buf X
                     |  $$ -show clashes only    | wd -save diff output
                     |  $* -skip changed regions |  / -show/hide ancestor buff
                     |                           |  & -merge w/new default
"
    "Help message for merge sessions.
This is a copy of `ediff-long-help-message-merge' with addition of X key.")

  (defun exordium--ediff-set-help-message()
    "Redefine the `ediff-long-help-message' and `ediff-help-message'.
This follows what `ediff-set-help-message' function is doing."
    (when ediff-merge-job
      (setq ediff-long-help-message
            (concat ediff-long-help-message-head
		            exordium--ediff-long-help-message-merge
		            ediff-long-help-message-tail))
      (when ediff-use-long-help-message
        (setq ediff-help-message ediff-long-help-message))))

  (add-hook 'ediff-display-help-hook 'exordium--ediff-set-help-message)
  (add-hook 'ediff-keymap-setup-hook 'exordium--add-copy-both-to-ediff-mode-map))


(provide 'init-git)
