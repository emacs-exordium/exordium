;;; init-yasnippet.el --- Yasnippet - insert code snippets based on keywords, using a trigger key -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Put your snippets under ~/.emacs.d/snippets/<mode-name>.  They are loaded
;; just-in-time in order to avoid long Emacs start-up times.  They are only
;; enabled for C++ mode.
;;
;; The trigger key is C-c C-y.  By default Yasnippet uses TAB and tries to be
;; about what the user wants (indent, insert tabs, auto-complete, or snippet)
;; but I think the TAB key is already too busy.
;;
;; For example: type "info" followed by C-c y.
;;
;; For details about how to write a snippet, see
;; https://github.com/capitaomorte/yasnippet/blob/master/README.mdown

;;; Code:

(use-package yasnippet
  :commands (yas-expand-from-trigger-key yas-load-directory)
  ;;; Enable YAS only for C and C++
  :hook ((c-mode-common . yas-minor-mode))
  :diminish yas-minor-mode
  :bind
  (:map yas-minor-mode-map
   ("C-c y" . #'yas-expand-from-trigger-key))
  :config
  ;; Directory tree where to find snippets (subdirectories must be mode names).
  ;; The t means JIT loading, which saves time during Emacs startup.
  (yas-load-directory (locate-user-emacs-file "snippets") t))

;;; Variables for file templates
(defvar *bde-component-author* exordium-yasnippet-author
  "Default component author string for BDE headers.")

(provide 'init-yasnippet)

;;; init-yasnippet.el ends here
