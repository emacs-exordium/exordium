;;;; Configuration of desktop state and history

(desktop-save-mode 1) ;;the state of Emacs is saved from one session to another.

(savehist-mode t) ;; minibuffer history is saved

(setq save-place-file "~/.emacs.d/saveplace") ;; location to save point
(setq-default save-place t)                   ;; activate it for all buffers
(require 'saveplace)                          ;; Automatically save place in files


(provide 'init-desktop)
