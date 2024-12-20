;;; init-treesit.el --- Configuration for Treesitter -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (unless (featurep 'init-require)
    (load (file-name-concat (locate-user-emacs-file "modules") "init-require"))))
(exordium-require 'init-prefs)

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
      (use-package treesit-auto
        :after treesit
        :demand t
        :commands (global-treesit-auto-mode)
        :custom
        (treesit-auto-install (if (boundp 'treesit-auto-install)
                                  treesit-auto-install
                                (unless (getenv "ci_tests")
                                  'prompt))
                              "Disable automatic grammar downloading in CI")
        :config
        (global-treesit-auto-mode))

      (use-package treesit
        :ensure nil
        :custom
        (treesit-font-lock-level 4)
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
