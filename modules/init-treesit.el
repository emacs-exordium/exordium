;;; init-treesit.el --- Configuration for Treesitter -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (unless (featurep 'init-require)
    (load (file-name-concat (locate-user-emacs-file "modules") "init-require"))))
(exordium-require 'init-prefs)
(exordium-require 'init-git)

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
        :bind (:map git-commit-ts-mode-map
               ([remap prog-fill-reindent-defun] . #'fill-paragraph))
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
        :autoload (treesit-auto--build-treesit-source-alist)
        :init
        (defun exordium--add-forward-ts-hook (recipe)
          "Add hook to a `ts-mode' slot from RECIPE.
A hook is added for each mode that is found in `remap' slot in recipe."
          (when-let* ((modes (ensure-list (treesit-auto-recipe-remap recipe)))
                      (ts-mode (treesit-auto-recipe-ts-mode recipe)))
            (dolist (mode modes)
              (let* ((ts-hook (intern (concat (symbol-name ts-mode) "-hook")))
                     (hook (intern (concat (symbol-name mode) "-hook"))))
                (add-hook ts-hook
                          (lambda ()
                            (run-hooks hook)))))))

        (defun exordium-treesit-auto-install-language-grammar (lang)
          "Install grammar for LANG using recipe from `treesit-auto-recipe-list'."
          (interactive
           (list (intern
                  (completing-read "Language: " treesit-auto-langs nil t))))
          (let ((treesit-language-source-alist
                 (treesit-auto--build-treesit-source-alist)))
            (treesit-install-language-grammar lang)))

        :functions (exordium--add-forward-ts-hook)
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

        ;; Recipe in `treesit-auto' for `janet' (from
        ;; https://github.com/sogaiu/tree-sitter-janet-simple) uses C function
        ;; `tree_sitter_janet_simple' as entrypoint
        (when-let* ((recipe (cl-find-if (lambda (recipe)
                                  (eq (oref recipe lang) 'janet))
                                treesit-auto-recipe-list)))
          (oset recipe lang 'janet-simple)
          (setq treesit-auto-langs
                (delq 'janet treesit-auto-langs))
          (push 'janet-simple treesit-auto-langs))

        ;; Recipe in `treesit-auto' for `markdown' (from
        ;; https://github.com/tree-sitter-grammars/tree-sitter-markdown) has a
        ;; different location for source code
        (when-let* ((recipe (cl-find-if (lambda (recipe)
                                  (eq (oref recipe lang) 'markdown))
                                treesit-auto-recipe-list)))
          (oset recipe source-dir "tree-sitter-markdown/src"))

        (global-treesit-auto-mode)
        (dolist (recipe treesit-auto-recipe-list)
          (exordium--add-forward-ts-hook recipe)))

      (use-package treesit
        :ensure nil
        :demand t
        :functions (exordium--treesit-generate-parser)
        :autoload (treesit--call-process-signal)
        :init
        (defun exordium--treesit-generate-parser (&rest args)
          "Try to run \\='tree-sitter generate\\=' if there's no parser.c."
          (when-let* (((equal "parser.c" (car (last args))))
                      ((not (file-exists-p (expand-file-name "parser.c"))))
                      (tree-sitter (executable-find "tree-sitter"))
                      (default-directory (file-name-parent-directory
                                          default-directory)))
            (message "Generating parser.c")
            (treesit--call-process-signal
             tree-sitter nil t nil "generate")))
        :custom
        (treesit-font-lock-level 4)
        :config
        ;; Some treesit grammars are delivered without a `parser.c' file, for
        ;; example https://github.com/latex-lsp/tree-sitter-latex/pull/114. It
        ;; seems that onus is on user to generate the file or use a versioned
        ;; release which should have the file. Unfortunately, Emacs is quite
        ;; simplistic when it comes to source of language grammars: it needs to
        ;; be a git repository (or a local directory, since Emacs-30). Let's
        ;; try to generate the `parser.c' file if it doesn't exists, and the
        ;; `tree-sitter' command line tool is available.
        (advice-add 'treesit--call-process-signal
                    :before #'exordium--treesit-generate-parser)))

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
