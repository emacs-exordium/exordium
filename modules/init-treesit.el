;; -*- lexical-binding: t; -*-

;;;; Configuration for Treesitter

(defun exordium--add-forward-ts-hook (mode)
  (when-let* ((ts-hook (intern (concat (symbol-name mode) "-ts-mode-hook")))
              (hook (intern (concat (symbol-name mode) "-mode-hook")))
              ((and (symbolp ts-hook) (symbolp hook))))
    (add-hook ts-hook
              #'(lambda ()
                  (run-hooks hook)))))

(when exordium-treesit-modes-enable
  (unless (getenv "ci_tests")
    (if (and (version< "29" emacs-version) (treesit-available-p))
        (progn
          (message "Enabling treesit-auto and builtin treesit")
          (use-package treesit-auto
            :requires treesit
            :config
            (setq treesit-auto-install 'prompt)
            (global-treesit-auto-mode))

          (use-package treesit
            :requires treesit
            :ensure nil
            :config
            (mapc #'exordium--add-forward-ts-hook
                  '(
                    LaTeX
                    bash
                    c
                    c++
                    cmake
                    csharp
                    css
                    go
                    go-mod
                    java
                    js
                    json
                    markdown
                    python
                    ruby
                    rust
                    scala
                    toml
                    typescript
                    yaml
                    ))
            (setq treesit-font-lock-level 4)))

      (progn
        (use-package tree-sitter-langs)
        (use-package tree-sitter
          :diminish
          :after (tree-sitter-langs)
          :hook
          (tree-sitter-after-on . tree-sitter-hl-mode)
          :custom
          (font-lock-maximum-decoration t)
          :config
          (when-let* ((language-name (alist-get 'ruby-mode
                                                tree-sitter-major-mode-language-alist)))
            (add-to-list 'tree-sitter-major-mode-language-alist
                         (cons 'enh-ruby-mode language-name)))
          (add-to-list 'tree-sitter-major-mode-language-alist
                       (cons 'forge-post-mode 'markdown))
          (global-tree-sitter-mode))))
    ) ;; (unless (getenv "ci_tests")
  ) ;; exordium-treesit-modes-enable
(provide 'init-treesit)
