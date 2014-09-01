;;;; Things specific to OS X

;; TODO: still broken
;; (setq command-line-default-directory "~/")
;; (setq-default default-directory "~/")

(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
   nil 'fullscreen
   (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

;; Give focus to emacs window at startup
(x-focus-frame nil)

(provide 'init-osx)
