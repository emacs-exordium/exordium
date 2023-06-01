;;;; Configuration for Treesitter

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

  (setq rust-ts-mode-hook rust-mode-hook)
  (setq c-ts-mode-hook c-mode-hook)
  (setq c++-ts-mode-hook c++-mode-hook)
  (setq python-ts-mode-hook python-mode-hook)
  (setq LaTeX-ts-mode-hook LaTeX-mode-hook)
  (setq treesit-font-lock-level 4))

(provide 'init-treesit)
