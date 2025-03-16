;;; init-treesit.el --- Configuration for Treesitter -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (unless (featurep 'init-require)
    (load (file-name-concat (locate-user-emacs-file "modules") "init-require"))))
(exordium-require 'init-prefs)
(exordium-require 'init-git)

(defun exordium--add-forward-ts-hook (mode)
  "Add hook to a ts-MODE that will run hooks from MODE."
  (when-let* ((ts-hook (intern (concat (symbol-name mode) "-ts-mode-hook")))
              (hook (intern (concat (symbol-name mode) "-mode-hook")))
              ((and (symbolp ts-hook) (symbolp hook))))
    (add-hook ts-hook
              (lambda ()
                (run-hooks hook)))))

(defmacro exordium-eval-unless-compile-or-ci (&rest body)
  "Don't evaluate BODY when either file is byte compiled when in CI."
  (declare (debug (&rest def-form)) (indent 0))
  (if (or (bound-and-true-p byte-compile-current-file)
          (eval-when-compile (getenv "ci_tests")))
      `(message "Skipping during %s %S"
                ,(if (bound-and-true-p byte-compile-current-file)
                     "compilation"
                   "CI tests")
                ',body)
    `(progn ,@body)))

(if (and (version< "29" emacs-version) (treesit-available-p))
    (progn
      (message "Enabling built-in treesit and external treesit-auto")
      (use-package git-commit-ts-mode
        :functions (exordium--git-commit-ts-adaptive-fill)
        :init
        (use-package treesit
          :ensure nil
          :defer t
          :autoload (treesit-node-at))

        (use-package git-commit
          :ensure magit
          :defer t
          :custom
          (git-commit-major-mode #'git-commit-ts-mode)
          :config
          ;; When the changelog support is on, the `fill-paragraph' doesn't
          ;; respect "hanging" multiline trailers (second and following line
          ;; starting with one or more spaces). This is due to
          ;; `fill-indent-according-to-mode'. Remove support for the changelog
          ;; in `git-commit'.
          (setq git-commit-setup-hook
                (remq #'git-commit-setup-changelog-support
                      git-commit-setup-hook)))

        (defun exordium--git-commit-ts-adaptive-fill ()
          "Return two spaces when in a trailer node or a breaking change node."
          (when (member (treesit-node-type
                         (treesit-node-parent
                          (treesit-node-at (point))))
                        '("trailer" "breaking_change"))
            (make-string 2 ? )))

        (defun exordium--git-commit-ts-setup-hanging-trailers ()
          "Setup `adaptive-fill-function' to auto fill with hanging trailers."
          (setq-local adaptive-fill-function
                      #'exordium--git-commit-ts-adaptive-fill))

        :hook (git-commit-ts-mode
               . exordium--git-commit-ts-setup-hanging-trailers))

      (use-package treesit-auto
        :after treesit
        :demand t
        :autoload (make-treesit-auto-recipe)
        :commands (global-treesit-auto-mode)
        :defines (treesit-auto-recipe-alist
                  treesit-auto-mode-langs)
        :custom
        (treesit-auto-install (if (boundp 'treesit-auto-install)
                                  treesit-auto-install
                                (unless (getenv "ci_tests")
                                  'prompt))
                              "Disable automatic grammar downloading in CI")
        :config
        (unless (memq 'gitcommit treesit-auto-langs)
          (push
           (make-treesit-auto-recipe
            :lang 'gitcommit
            :ts-mode 'git-commit-ts-mode
            :url "https://github.com/gbprod/tree-sitter-gitcommit"
            :ext "\\COMMIT_EDITMSG\\'")
           treesit-auto-recipe-list)
          (push 'gitcommit treesit-auto-langs))
        (global-treesit-auto-mode))

      (use-package treesit
        :ensure nil
        :custom
        (treesit-font-lock-level 4)
        :config
        (mapc #'exordium--add-forward-ts-hook
              '(
                LaTeX
                ada
                awk
                bash
                c
                c++
                closure
                cmake
                csharp
                css
                dockerfile
                elixir
                git-commit
                go
                go-mod
                graphql
                haskell
                heex
                html
                java
                jq
                js
                json
                julia
                kotlin
                lua
                markdown
                nix
                php
                python
                protobuf
                ruby
                rust
                scala
                swift
                toml
                typescript
                tsx
                yaml
                ))))

  (exordium-eval-unless-compile-or-ci
    (message "Enabling external tree-sitter and tree-sitter-langs")
    (use-package tree-sitter-langs)
    (use-package tree-sitter
      :diminish
      :commands (global-tree-sitter-mode)
      :defines (tree-sitter-major-mode-language-alist)
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

(provide 'init-treesit)

;;; init-treesit.el ends here
