;;;; Configuration of desktop state and history

(defun exordium--restore-desktop ()
  (setq desktop-path '("~/.emacs.d/"))
  (setq desktop-save t)
  (message (format "loading desktop from %s" desktop-path))
  (desktop-read)
  (desktop-save-mode 1))

(if (not (daemonp))
    (desktop-save-mode 1)
  (add-hook 'server-after-make-frame-hook #'exordium--restore-desktop))

(savehist-mode t) ;; minibuffer history is saved

(setq save-place-file
      (locate-user-emacs-file "saveplace"))   ;; location to save point
(setq-default save-place t)                   ;; activate it for all buffers
(use-package saveplace)                          ;; Automatically save place in files


(provide 'init-desktop)
