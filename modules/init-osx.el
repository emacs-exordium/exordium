;;; init-osx.el --- Things specific to OS X -*- lexical-binding: t -*-

;;; Commentary:
;;
;; TODO: still broken
;; (setq command-line-default-directory "~/")
;; (setq-default default-directory "~/")


(eval-when-compile
  (unless (featurep 'init-require)
    (load (file-name-concat (locate-user-emacs-file "modules") "init-require"))))
(exordium-require 'init-environment)

;;; Code:

(unless exordium-nw
  (defun toggle-fullscreen ()
    "Toggle full screen"
    (interactive)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

  ;; Give focus to emacs window at startup
  (add-hook 'after-init-hook
            (lambda ()
              (x-focus-frame nil))))

;; Make $PATH available in shell mode

(use-package exec-path-from-shell
  :custom
  (exec-path-from-shell-arguments
   (if (string-suffix-p "zsh" (exec-path-from-shell--shell))
       '("-i")
     '("-l" "-i")))
  :hook (after-init . exec-path-from-shell-initialize))

(provide 'init-osx)

;;; init-osx.el ends here
