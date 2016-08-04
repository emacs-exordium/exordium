;;;; Configuration of dired, dired+, and wdired

(require 'dired)
(require 'wdired)
(require 'dired-x)
(require 'init-lib)

(autoload 'dired-jump "dired-x"
  "Jump to Dired buffer corresponding to current buffer." t)

(autoload 'dired-jump-other-window "dired-x"
  "Like \\[dired-jump] (dired-jump) but in other window." t)

;; FIXME: exordium-define-key does not work with those:
(define-key global-map "\C-x\C-j" 'dired-jump)
(define-key global-map "\C-x4\C-j" 'dired-jump-other-window)

(add-hook 'dired-load-hook
          (lambda ()
            ;; Set dired-x global variables here.  For example:
            (setq wdired-allow-to-change-permissions t)
            (setq dired-x-hands-off-my-keys nil)
            (load "dired-x")
            ))

(add-hook 'dired-mode-hook
          (lambda ()
            ;; Set dired-x buffer-local variables here.  For example:
            (define-key (current-local-map) [(mouse-3)]
              'dired-mouse-find-file-other-window)
            ))

(require 'find-dired)
;; xargs to get options rather than exec ls on each find
(setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))

(provide 'init-dired)
