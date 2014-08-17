;;;; Things specific to OS X

;; (setq command-line-default-directory "~/")
;; (setq-default default-directory "~/")

(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
   nil 'fullscreen
   (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

(provide 'init-osx)
