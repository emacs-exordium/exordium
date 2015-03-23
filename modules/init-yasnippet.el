;;;; Yasnippet - insert code snippets based on keywords, using a trigger key.
;;;
;;; Put your snippets under ~/.emacs.d/snippets/<mode-name>. They are loaded
;;; just-in-time in order to avoid long Emacs start-up times. They are only
;;; enabled for C++ mode.
;;;
;;; The trigger key is C-c C-y. By default Yasnippet uses TAB and tries to be
;;; about what the user wants (indent, insert tabs, auto-complete, or snippet)
;;; but I think the TAB key is already too busy.
;;;
;;; For example: type "info" followed by C-c y.
;;;
;;; For details about how to write a snippet, see
;;; https://github.com/capitaomorte/yasnippet/blob/master/README.mdown

(require 'yasnippet)
;;(yas-global-mode 1) ; always on

;;; Directory tree where to find snippets (subdirectories must be mode names).
;;; The t means JIT loading, which saves time during Emacs startup.
(yas-load-directory (locate-user-emacs-file "snippets") t)

;;; Enable YAS only for C++
(add-hook 'c-mode-common-hook
          '(lambda ()
             (yas-minor-mode)))

;;; Trigger key
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "C-c y") 'yas-expand)

;;; Don't show this minor mode in the modeline
(diminish 'yas-minor-mode)

;;; Variables for file templates
(defvar *bde-component-author* "Philippe Grenet (pgrenet)"
  "Default component author string for BDE headers")

(provide 'init-yasnippet)
