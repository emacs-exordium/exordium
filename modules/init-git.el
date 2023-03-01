;;;; All git-related stuff, except for forge
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
;;;
;;; C-c ^ d           Show SMerge Dispatch

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

  (defvar exordium--magit-fullscreen-configuration-tmp nil
    "A temporary (cached) screen configuration and a point-marker that are set by a
first executing `exordium--magit-fullscreen'.")

  (defvar-local exordium--magit-fullscreen-configuration nil
    "A screen configuration and a point-marker that are to be restored by
`exordium-magit-quit-session'.")

  (defun exordium-magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer."
    (interactive)
    (let ((configuration exordium--magit-fullscreen-configuration))
      (kill-buffer)
      (when (and configuration
                 (window-configuration-p (car configuration)))
        (set-window-configuration (car configuration))
        (goto-char (cadr configuration)))))

  (defun exordium--magit-fullscreen (orig-fun &rest args)
    "Store the current window configuration, call ORIG-FUN with ARGS, and delete other windows.

The window configuration (including `point-marker') is stored in a very beginning
and cached, so that when the function is called again, the cached version is used.

The function is meant to be used as an advice with conjunction with `exordium-magit-quit-session'."
    (let ((exordium--magit-fullscreen-configuration-tmp
           (or exordium--magit-fullscreen-configuration-tmp
               (list (current-window-configuration) (point-marker)))))
      (apply orig-fun args)
      (delete-other-windows)
      (setq-local exordium--magit-fullscreen-configuration
                  exordium--magit-fullscreen-configuration-tmp)))

  (defun exordium-magit--dont-insert-symbol-for-search ()
    "Don't insert a symbol at point when starting ag or rg."
    (setq-local helm-ag-insert-at-point nil)
    (setq-local helm-rg-thing-at-point nil))

  ;;; Turn off the horrible warning about magit auto-revert of saved buffers
  (setq magit-last-seen-setup-instructions "1.4.0")

  :bind
  (:map exordium-git-map
        ("s" . #'magit-status)
        ("l" . #'exordium-magit-log)
        ("f" . #'exordium-magit-log-buffer)
        ("b" . #'exordium-magit-blame)
        ("c" . #'magit-clone)
   :map magit-status-mode-map
        ("q" . #'exordium-magit-quit-session))

  :hook
  (magit-status-mode . exordium-magit--dont-insert-symbol-for-search)

  :custom
  (magit-diff-refine-hunk t)

  :config
;;; Make `magit-status',`exordium-magit-log' (a wrapper around `magit-log' and
;;; `magit-dired-log'), `magit-status-setup-buffer' (called from `magit-clone'),
;;; and `magit-status-internal' (called from `projectile-vc') to run alone in
;;; the frame, and then restore the old window configuration when you quit out
;;; of magit.
  (when exordium-use-magit-fullscreen
    (advice-add 'magit-status :around #'exordium--magit-fullscreen)
    (advice-add 'exordium-magit-log :around #'exordium--magit-fullscreen)
    (advice-add 'magit-status-setup-buffer :around #'exordium--magit-fullscreen)
    (when (fboundp 'magit-status-internal) ;; check just like in `projectile-vc'
      (advice-add 'magit-status-internal :around #'exordium--magit-fullscreen)))

  (define-advice magit-clone-regular (:after
                                      (_repo directory _args)
                                      exordium-projectile-add-known-project)
    (projectile-add-known-project directory)))

;;; Don't show "MRev" in the modeline
(when (bound-and-true-p magit-auto-revert-mode)
  (diminish 'magit-auto-revert-mode))


;; SMerge Dispatch
(defun exordium-smerge-save-and-status ()
  "Save current buffer and show Magit status buffer."
  (interactive)
  (save-buffer)
  (magit-status-setup-buffer))

(defun exordium-smerge-revert-and-status ()
  "Revert current buffer and run Magit status buffer."
  (interactive)
  (revert-buffer nil t)
  (magit-status-setup-buffer))

(use-package transient
  :config
  (transient-define-suffix exordium-smerge:undo ()
    :description "undo"
    :key (if exordium-keyboard-ctrl-z-undo "C-z" "C-x u")
    (interactive)
    (undo))

  (transient-define-prefix exordium-smerge-dispatch ()
    "Dispatch for `smerge-mode' commands."
    :transient-suffix     'transient--do-stay
    :transient-non-suffix 'transient--do-warn
    [["Movement"
      ("n" "next hunk" smerge-next)
      ("p" "prev hunk" smerge-prev)
      ("C-n" "next line" next-line)
      ("C-p" "prev line" previous-line)
      ("C-v" "scroll up" scroll-up-command)
      ("M-v" "scroll down" scroll-down-command)
      ("C-l" "recenter" recenter-top-bottom)]
     ["Merge action"
      ("b" "keep base" smerge-keep-base)
      ("u" "keep upper" smerge-keep-upper)
      ("l" "keep lower" smerge-keep-lower)
      ("a" "keep all"   smerge-keep-all)
      ("c" "keep current" smerge-keep-current)
      ("r" "resolve" smerge-resolve)]
     ["Diff action"
      ("= <" "upper/base" smerge-diff-base-upper)
      ("= =" "upper/lower" smerge-diff-upper-lower)
      ("= >" "base/lower" smerge-diff-base-lower)
      ("R" "refine" smerge-refine)
      ("C" "combine with next" smerge-combine-with-next)
      ("k" "kill current" smerge-kill-current)]
     ["Other"
      ("C-c C-s" "save" save-buffer)
      ("C-c C-c" "save & status" exordium-smerge-save-and-status
       :transient nil)
      ("C-c C-k" "revert & status" exordium-smerge-revert-and-status
       :transient nil)
      (exordium-smerge:undo)
      ("E" "ediff" smerge-ediff
       :transient nil)]]))

(use-package smerge-mode
  :ensure nil
  :bind
  (:map smerge-mode-map
        ("C-c ^ d" . #'exordium-smerge-dispatch)))

(defun exordium-smerge-dispatch-maybe ()
  "Display `exordium-smerge-dispatch' when buffer is in `smerge-mode'."
  (when (and smerge-mode exordium-smerge-show-dispatch)
    (exordium-smerge-dispatch)))

(use-package magit
  :hook
  (magit-diff-visit-file . exordium-smerge-dispatch-maybe))


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
  :diminish)

(use-package git-gutter-fringe
  :if (and exordium-git-gutter (not exordium-git-gutter-non-fringe))
  :diminish git-gutter-mode
  :bind
  (:map exordium-git-map
        ("<down>" . #'git-gutter:next-hunk)
        ("n" . #'git-gutter:next-hunk)
        ("<up>" . #'git-gutter:previous-hunk)
        ("p" . #'git-gutter:previous-hunk)
        ("d" . #'git-gutter:popup-hunk)
        ("r" . #'git-gutter:revert-hunk))
  :init
  (add-hook 'git-gutter:update-hooks 'magit-revert-buffer-hook)
  :config
  (exordium-global-git-gutter-mode t)

  ;; Style
  (when (eq exordium-git-gutter-fringe-style :flat)
    (setq-default fringes-outside-margins t)
    (define-fringe-bitmap 'git-gutter-fr:added [224]
      nil nil '(center repeated))
    (define-fringe-bitmap 'git-gutter-fr:modified [224]
      nil nil '(center repeated))
    (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240]
      nil nil 'bottom)))



;;; Git Timemachine
(use-package git-timemachine
  :defer t
  :bind
  (:map exordium-git-map
        ("t" . #'git-timemachine-toggle)))


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
