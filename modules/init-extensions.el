;;;; Extensions
;;;
;;; This file loads minor extensions:
;;;
;;; cua ............: use normal copy-paste keys when non-ambiguous
;;; expand region...: smart selection using just 1 key
;;; autopairs ......: autocomplete parentheses, braces and quotes
;;; fci mode .......: 80 column ruler (fill column indicator)
;;; fic mode .......: color TODOs
;;; highlight symbol: highlight occurences of symbol under point
;;;
;;; -------------- -------------------------------------------------------
;;; Key            Definition
;;; -------------- -------------------------------------------------------
;;; Ctrl-=         Expand region
;;; Ctrl-|         Toggle fci mode on and off


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CUA

;; C-x, C-c, C-v to cut, copy and paste when mark is active.
;; Add shift or double the Ctrl-* to switch back to Emacs keys.
;; C-Ret for rectangular regions.
(when *init-enable-cua-mode*
  (cua-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Expand region

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Autopairs

(when *init-enable-electric-pair-mode*
  (electric-pair-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 80-column ruler bound to Ctrl-|

(require 'fill-column-indicator)
(setq fci-rule-use-dashes t)
(setq fci-dash-pattern 0.5)
(setq fci-rule-width 1)
(setq fci-rule-color "dim gray")
(define-key global-map [(control |)] 'fci-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FIXME mode

;; (require 'fic-mode)
;; (add-hook 'c++-mode-hook 'turn-on-fic-mode)
;; (add-hook 'markdown-mode-hook 'turn-on-fic-mode)

;; This is better:
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '(("\\<\\(TODO\\|FIXME\\|TBD\\):" 1 font-lock-warning-face t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Highlight symbol

(require 'highlight-symbol)
;; (dolist (hook '(prog-mode-hook html-mode-hook))
;;   (add-hook hook 'highlight-symbol-mode)
;;   (add-hook hook 'highlight-symbol-nav-mode))

;; Don't show this mode in the modeline
(eval-after-load 'highlight-symbol
  '(diminish 'highlight-symbol-mode))

(provide 'init-extensions)
