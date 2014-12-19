;;;; Configuration for Ruby
;;;; https://github.com/zenspider/enhanced-ruby-mode

(autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Font lock changes

;;; Display TODO: and FIXME: and TBD: in red
(font-lock-add-keywords
 'enh-ruby-mode
 '(("\\<\\(FIXME\\):" 1 font-lock-warning-face prepend)
   ("\\<\\(TBD\\):" 1 font-lock-warning-face prepend)
   ("\\<\\(TODO\\):" 1 font-lock-warning-face prepend)))

(provide 'init-ruby)
