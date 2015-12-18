;;;; Things specific to OS X

;; TODO: still broken
;; (setq command-line-default-directory "~/")
;; (setq-default default-directory "~/")

(require 'init-environment)

(unless exordium-nw
  (defun toggle-fullscreen ()
    "Toggle full screen"
    (interactive)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

  ;; Give focus to emacs window at startup
  (x-focus-frame nil))

;; Make $PATH available in shell mode
(exec-path-from-shell-initialize)

(provide 'init-osx)
