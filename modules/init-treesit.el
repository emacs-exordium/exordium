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
        :functions (exordium--git-commit-ts-verify
                    exordium--git-commit-ts-adaptive-fill)
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

        (use-package thingatpt
          :ensure nil
          :defer t
          :defines (thing-at-point-email-regexp))

        (defun exordium--git-commit-ts-verify ()
          "Used for `flyspell-generic-check-word-predicate' in `git-commit-ts-mode'."
          (unless (eql (point) (point-min))
            (let* ((node (treesit-node-at (point)))
                   (node-type (treesit-node-type node)))
              (when (equal node-type "value")
                (setq node-type (treesit-node-type
                                 (treesit-node-parent node))))
              (when (member node-type '("subject"
                                        "message_line"
                                        "trailer"
                                        "breaking_change"))
                (cond
                 ;; Tokens for trailer and breaking change are only parsed
                 ;; after colon and space are entered. This leads to spell
                 ;; checking them, which is probably best to avoid.
                 ((and (equal node-type "message_line")
                       (string-match-p
                        (rx string-start
                            (or (seq "BREAKING" (or " " "-") "CHANGE")
                                (one-or-more (any (?a . ?z) (?A . ?Z) "-")))
                            (zero-or-more " ") (or ":" ?\xff1a))
                        (buffer-substring (pos-bol) (min (1+ (point))
                                                         (point-max)))))
                  nil)
                 ;; `thing-at-point' returns nil for `email' when it ends
                 ;; with an `@'. This is however, quite normal, for example
                 ;; while in the middle of typing an address.
                 ((pcase-let* ((`(,user ,domain)
                                (string-split thing-at-point-email-regexp
                                              "@"))
                               (thing-at-point-email-regexp
                                (rx-to-string
                                 `(seq (regexp ,user)
                                       "@"
                                       (zero-or-one (regexp ,domain))))))
                    (thing-at-point 'email))
                  nil)
                 ;; User and team @mentions contain characters `@' and
                 ;; `/'. Neither is a part of a word, so let's temporarily
                 ;; modify syntax table such they are grabbed.
                 ((let ((table (copy-syntax-table)))
                    (modify-syntax-entry ?@ "w" table)
                    (modify-syntax-entry ?/ "w" table)
                    (with-syntax-table table
                      (string-match-p
                       (rx-let ((identifier
                                 (seq alphanumeric
                                      (repeat 0 38 (or alphanumeric "-")))))
                         (rx string-start
                             "@"
                             (zero-or-one identifier
                                          (zero-or-one "/"))
                             (zero-or-one identifier)
                             string-end))
                       (thing-at-point 'word))))
                  nil)
                 ;; Spell check everything else
                 (t t))))))

        :hook (git-commit-ts-mode
               . exordium--git-commit-ts-setup-hanging-trailers)
        :config
        ;; Only set our `flyspell-mode-predicate' when there's none set,
        ;; see: https://github.com/danilshvalov/git-commit-ts-mode/pull/8
        (unless (get 'git-commit-ts-mode 'flyspell-mode-predicate)
          (put 'git-commit-ts-mode
               'flyspell-mode-predicate
               #'exordium--git-commit-ts-verify)))

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
