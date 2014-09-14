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

;; CUA makes C-x, C-c and C-v cut/copy/paste when a region is selected.
;; Adding shift or doubling the Ctrl-* makes it switch back to Emacs keys.
;; It also has a nice feature: C-ret for selecting rectangular regions.
;; If *init-enable-cua-mode* is nil, only the rectangular regions are enabled.
(if *init-enable-cua-mode*
    (cua-mode t)
  (cua-selection-mode t))

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
;;; Loud face for TODOs in elisp comments

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '(("\\<\\(TODO\\|FIXME\\|TBD\\):" 1 font-lock-warning-face t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Highlight symbol

(require 'highlight-symbol)

(highlight-symbol-nav-mode)

(add-hook 'prog-mode-hook (lambda () (highlight-symbol-mode)))
(setq highlight-symbol-on-navigation-p t)

;; Don't show this mode in the modeline
(eval-after-load 'highlight-symbol
  '(diminish 'highlight-symbol-mode))

(provide 'init-extensions)
