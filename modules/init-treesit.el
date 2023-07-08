;; -*- lexical-binding: t; -*-

;;;; Configuration for Treesitter

(if (and (version< "29" emacs-version ) (treesit-available-p))
    (unless (getenv "ci_tests")
      (use-package tree-sitter-langs)
      (use-package tree-sitter
        :diminish
        :after (tree-sitter-langs)
        :hook
        (tree-sitter-after-on . tree-sitter-hl-mode)
        :custom
        (font-lock-maximum-decoration t)
        :config
        (when-let ((language-name (alist-get 'ruby-mode
                                             tree-sitter-major-mode-language-alist)))
          (add-to-list 'tree-sitter-major-mode-language-alist
                       (cons 'enh-ruby-mode language-name)))
        (add-to-list 'tree-sitter-major-mode-language-alist
                     (cons 'forge-post-mode 'markdown))
        (global-tree-sitter-mode)))

  (defun exordium--add-forward-ts-hook (mode)
    (when-let ((ts-hook (intern (concat (symbol-name mode) "-ts-mode-hook")))
               (hook (intern (concat (symbol-name mode) "-mode-hook")))
               (_ (and (symbolp ts-hook) (symbolp hook))))
      (add-to-list ts-hook
                   #'(lambda ()
                       (run-hooks hook)))))

  (use-package treesit-auto
    :requires treesit
    :after lsp-mode
    :config
    (setq treesit-auto-install 'prompt)
    (global-treesit-auto-mode))

  (use-package treesit
    :requires treesit
    :ensure nil
    :after lsp-mode
    :config
    (mapc exordium--add-forward-ts-hook
      '(
        LaTeX
        c
        c++
        java
        go
        markdown
        python
        ruby
        rust
        scala
        ))
    (setq treesit-font-lock-level 4)))

(provide 'init-treesit)
