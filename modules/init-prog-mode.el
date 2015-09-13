;; Shared prog-mode configuration

(require 'init-prefs)

(define-minor-mode exordium-show-trailing-whitespace-mode
  "Enables `show-trailing-whitespace'."
  :init-value nil
  :lighter nil
  (progn (setq show-trailing-whitespace exordium-show-trailing-whitespace-mode)))

(define-minor-mode exordium-require-final-newline-mode
  "Enables `require-final-newline'."
  :init-value nil
  :lighter nil
  (progn (setq require-final-newline exordium-require-final-newline-mode)))

(add-hook 'prog-mode-hook 'show-paren-mode)
(add-hook 'prog-mode-hook 'exordium-show-trailing-whitespace-mode)
(add-hook 'prog-mode-hook 'exordium-require-final-newline-mode)

(when (eq exordium-spell-check :prog)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))


;;; Electric pair: automatically close parenthesis, curly brace etc.
;;; `electric-pair-open-newline-between-pairs'.
(setq electric-pair-open-newline-between-pairs t)
(when exordium-enable-electric-pair-mode
  (add-hook 'prog-mode-hook 'electric-pair-mode))


;;; The return key
(cond (exordium-enable-newline-and-indent
       (define-key prog-mode-map (kbd "<return>") (function newline-and-indent))
       (define-key prog-mode-map (kbd "<S-return>") (function newline)))
      (t
       (define-key prog-mode-map (kbd "<S-return>") (function newline-and-indent))))


;;; Fill comments, comment regions
(require 'newcomment)
(setq comment-auto-fill-only-comments 1)
(define-key prog-mode-map (kbd "\C-c\C-c") (function comment-region))


;;; Step through compile errors
(global-set-key (quote [f10]) (quote next-error))
(global-set-key (quote [(control f10)]) (quote previous-error))



;;; Font lock changes

;;; Display TODO: and FIXME: and TBD: in red
(when exordium-font-lock
  (defun add-keywords-for-todos ()
    (font-lock-add-keywords
     nil
     '(("\\<\\(FIXME\\):" 1 font-lock-warning-face prepend)
       ("\\<\\(TBD\\):" 1 font-lock-warning-face prepend)
       ("\\<\\(TODO\\):" 1 font-lock-warning-face prepend))))
  (add-hook 'prog-mode-hook 'add-keywords-for-todos))




;;; Fill column indicator
(when (eq exordium-fci-mode :prog)
  (add-hook 'prog-mode-hook 'fci-mode))


(provide 'init-prog-mode)
;; End of file
