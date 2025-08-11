;;; init-font-lock.el --- Font Lock                  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; exordium-font-lock turns on or off the syntax highighting globally.

;;; Code:
(eval-when-compile
  (unless (featurep 'init-require)
    (load (file-name-concat (locate-user-emacs-file "modules") "init-require"))))
(exordium-require 'init-prefs)

(cond (exordium-font-lock
       (global-font-lock-mode 1)
       (unless exordium-treesit-modes-enable
         (setq font-lock-maximum-decoration
               '((c++-mode . 1) ;; t or 1 or 2
                 (t . t))))
       (setq jit-lock-chunk-size 5000
             jit-lock-context-time 0.2
             jit-lock-defer-time .1
             jit-lock-stealth-nice 0.2
             jit-lock-stealth-time 5
             jit-lock-stealth-verbose nil)
       (jit-lock-mode t))
      (t
       ;; Disable font lock completely.
       (global-font-lock-mode -1)))

(provide 'init-font-lock)

;;; init-font-lock.el ends here
