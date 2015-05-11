;;;; Markdown
;;; See http://jblevins.org/projects/markdown-mode/

(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))

;;; Loud face for TODOs in markdown documents
(require 'init-prefs)
(require 'markdown-mode)
(when exordium-font-lock
  (setq markdown-mode-font-lock-keywords-core
        (list
         (cons markdown-regex-italic '(2 markdown-italic-face))
         (cons "\\<\\(TODO\\|FIXME\\|TBD\\):" '(1 font-lock-warning-face)))))

(provide 'init-markdown)
