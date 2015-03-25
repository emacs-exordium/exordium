;;;; Font Lock
;;;
;;; exordium-font-lock turns on or off the syntax highighting globally.

(cond (exordium-font-lock
       (global-font-lock-mode 1)
       (setq font-lock-maximum-decoration
             '((emacs-lisp-mode . t)
               (c-mode . t)
               (C++-mode . 1) ;; t or 1 or 2
               (t . t)))
       ;;
       ;; Lazy font-lock to avoid the bug in Emacs 24
       (cond ((fboundp 'jit-lock-mode)
              (setq jit-lock-chunk-size 5000
                    jit-lock-context-time 0.2
                    jit-lock-defer-time .1
                    jit-lock-stealth-nice 0.2
                    jit-lock-stealth-time 5
                    jit-lock-stealth-verbose nil)
              (jit-lock-mode t))
             ((fboundp 'turn-on-lazy-shot)
              (add-hook 'font-lock-mode-hook 'turn-on-lazy-shot))
             ((fboundp 'turn-on-lazy-lock)
              (add-hook 'font-lock-mode-hook 'turn-on-lazy-lock)
              (setq lazy-lock-stealth-time 10)
              (setq lazy-lock-minimum-size 10000)))
       ;;(fci-always-use-textual-rule t)
       )
      (t
       ;; Disable font lock completely.
       (global-font-lock-mode -1)))

(provide 'init-font-lock)
