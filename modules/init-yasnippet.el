;;;; Yasnippet
;;;
;;; TODO: loading snippets takes a long time when Emacs starts.
;;; Investigate loading them only on demand, when the first C++ file is open.
;;; For example, in the hook:
;;; (let (dir ((list "~/.emacs.d/snippets")
;;;            yas-installed-snippets-dir)))
;;;   (yas--load-directory-1 yas-installed-snippets-dir 'c++-mode))

(require 'yasnippet)
;;(yas-global-mode 1) ; always on

;; Enable YAS only for C++
(yas-reload-all)
(add-hook 'c-mode-common-hook
          '(lambda ()
             (yas-minor-mode)))

;; Don't show this minor mode in the modeline
(diminish 'yas-minor-mode)

(provide 'init-yasnippet)
