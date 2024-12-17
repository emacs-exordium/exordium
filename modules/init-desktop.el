;;; init-desktop.el --- Configuration of desktop state and history -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'saveplace)
(require 'savehist)
(require 'desktop)

(defun exordium--restore-desktop ()
  "Restore desktop."
  (setq desktop-path (list user-emacs-directory))
  (setq desktop-save t)
  (message (format "Loading desktop from %s" desktop-path))
  (desktop-read)
  (desktop-save-mode 1))

(if (not (daemonp))
    (desktop-save-mode 1)
  (add-hook 'server-after-make-frame-hook #'exordium--restore-desktop))

(savehist-mode t) ;; minibuffer history is saved

(setq save-place-file
      (locate-user-emacs-file "saveplace"))   ;; location to save point
(save-place-mode)                             ;; activate it for all buffers

(provide 'init-desktop)

;;; init-desktop.el ends here
