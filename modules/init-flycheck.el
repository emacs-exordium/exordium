;; -*- lexical-binding: t; -*-
;;; init-flycheck --- Flycheck configuration for Exordium.
;;; Commentary:
;;; Flycheck see https://www.flycheck.org

;;; Code:
(require 'init-prefs)


;;; Basic configuration (as need for lsp and for shellcheck)
(use-package flycheck
  :commands flycheck-mode
  :custom
  (flycheck-global-modes '(not c++-mode c-mode org-mode))
  :hook
  (after-init . global-flycheck-mode)
  :config
  (add-to-list 'flycheck-shellcheck-supported-shells 'ksh93))


;;; A custom mypy checker, with error explanations
(use-package poly-rst)

(defvar exordium--flycheck-mypy-error-codes-alist nil
  "Error codes alist in a form of (MYPY-VERSION . CODES).

The MYPY-VERSION is a symbol of a mypy versoin and CODES are an
alist in a form of (CODE . BODY).  The CODE is the symbol of a
mypy error code and BODY is a rst body of the code.")

(use-package flycheck
  :init
  (defun exodrium--flycheck-mypy-retrieve-error-codes (mypy-version)
    (let* (error-codes-alist
           (error-headline "^[A-Z][a-z' ]+ \\[\\([a-z-]+\\)\\]
-+"))
      (mapc
       (lambda (error-codes-file-name)
         (with-current-buffer
             (url-retrieve-synchronously
              (concat
               "https://raw.githubusercontent.com/python/mypy/v"
               mypy-version
               "/docs/source/"
               error-codes-file-name))
           (when (re-search-forward "^\\.\\. _error-code\\(?:s\\)?-.*:" nil t)
             (while (re-search-forward error-headline nil t)
               (let* ((error-code (match-string 1))
                      (error-body-start (match-beginning 0))
                      (error-body-end
                       (save-match-data
                         (if (re-search-forward error-headline nil t)
                             (match-beginning 0)
                           (point-max))))
                      (error-body (buffer-substring error-body-start
                                                    error-body-end)))
                 (push (cons (intern error-code) error-body) error-codes-alist)
                 (goto-char error-body-end))))))
       '("error_code_list.rst" "error_code_list2.rst"))
      error-codes-alist))

  :config
  ;; Extended version of https://github.com/flycheck/flycheck/blob/5f2ef17/flycheck.el#L10838-L10860
  (flycheck-define-checker exordium-python-mypy
    "Mypy syntax and type checker.  Requires mypy>=0.730.

Note that this checker substitutes the original `python-mypy' checker and uses
its configuration variables (i.e., `flycheck-mypy-*' variables).

See URL `http://mypy-lang.org/'."
    :command ("mypy"
              "--show-column-numbers"
              "--show-error-codes"
              "--no-pretty"
              (config-file "--config-file" flycheck-python-mypy-config)
              (option "--cache-dir" flycheck-python-mypy-cache-dir)
              (option "--python-executable"
                      flycheck-python-mypy-python-executable)
              source-original)
    :error-patterns
    ((error line-start (file-name) ":" line (optional ":" column)
            ": error:" (message)
            (one-or-more (any space)) "[" (id (one-or-more not-newline)) "]"
            line-end)
     (warning line-start (file-name) ":" line (optional ":" column)
              ": warning:" (message)
              (one-or-more (any space)) "[" (id (one-or-more not-newline)) "]"
              line-end)
     (info line-start (file-name) ":" line (optional ":" column)
           ": note:" (message)
           (one-or-more (any space)) "[" (id (one-or-more not-newline)) "]"
           line-end))
    :working-directory flycheck-python-find-project-root
    :modes (python-mode python-ts-mode)
    ;; Ensure the file is saved, to work around
    ;; https://github.com/python/mypy/issues/4746.
    :predicate flycheck-buffer-saved-p
    :error-explainer
    (lambda (error)
      (when-let* ((error-code (flycheck-error-id error))
                  (mypy-version
                   (replace-regexp-in-string
                    "mypy \\(\\(?:[0-9]\\.\\)+[0-9]\\).*\n"
                    "\\1"
                    (shell-command-to-string "mypy --version")))
                  (error-codes-alist
                   (exordium-setf-when-nil
                    (alist-get (intern mypy-version)
                               exordium--flycheck-mypy-error-codes-alist)
                    (exodrium--flycheck-mypy-retrieve-error-codes
                     mypy-version)))
                  (explanation (alist-get (intern error-code)
                                          error-codes-alist)))
        (lambda ()
          (with-current-buffer standard-output
            (insert explanation)
            (poly-rst-mode)
            (view-mode)
            (font-lock-flush)
            (font-lock-ensure))))))

  (add-to-list 'flycheck-checkers 'exordium-python-mypy)
  (mapc (lambda (checker)
          (flycheck-add-next-checker checker '(warning . exordium-python-mypy)))
        '(python-flake8 python-pylint python-pycompile))
  (add-hook 'python-mode-hook
            #'(lambda ()
                (add-to-list 'flycheck-disabled-checkers 'python-mypy))))


;; A custom ruff checker with error explanations
(use-package flycheck
  :config
  ;; Extended version of https://gist.github.com/abo-abo/277d1fe1e86f0e46d3161345f26e8f3a
  (flycheck-define-checker exordium-python-ruff
    "A Python syntax and style checker using the ruff utility.
To override the path to the ruff executable, set
`flycheck-exordium-python-ruff-executable'.
See URL `http://pypi.python.org/pypi/ruff'."
    :command ("ruff" "check"
              (eval (let ((ruff-version (replace-regexp-in-string
                                    "ruff \\([0-9]\.\+\\).*\n?" "\\1"
                                    (shell-command-to-string "ruff --version"))))
                      (cond
                       ((version< ruff-version "0.1")
                        '("--format" "text"))
                       ((version< ruff-version "0.5")
                        '("--output-format" "text"))
                       (t
                        '("--output-format" "concise")))))
              (eval (when buffer-file-name
                      (concat "--stdin-filename=" buffer-file-name)))
              "-")
    :standard-input t
    :error-filter
    (lambda (errors)
      (let ((errors (flycheck-sanitize-errors errors)))
        (seq-map #'flycheck-flake8-fix-error-level errors)))
    :error-patterns
    ((error line-start
            (file-name) ":" line ":" (optional column ":") " "
            (id (one-or-more (any alpha digit))) ": "
            (message (one-or-more not-newline)))
            (warning line-start
              (file-name) ":" line ":" (optional column ":") " "
              (id (one-or-more (any alpha)) (one-or-more digit)) " "
              (message (one-or-more not-newline))
              line-end))
    :error-explainer
    (lambda (error)
      (when-let* ((error-code (flycheck-error-id error))
                  (error-level (flycheck-error-level error)))
        (if (eq error-level 'error)
            (message "No explanation for error: %s" error-code)
          (lambda ()
            (flycheck-call-checker-process
             'exordium-python-ruff nil standard-output t "rule" error-code)
            (with-current-buffer standard-output
              (let ((markdown-fontify-code-block-default-mode 'python-mode)
                    (markdown-fontify-code-blocks-natively t)
                    (markdown-hide-markup t))
                (markdown-view-mode)
                (font-lock-flush)
                (font-lock-ensure)))))))
    :modes (python-mode python-ts-mode))

  (add-to-list 'flycheck-checkers 'exordium-python-ruff)
  (flycheck-add-next-checker 'exordium-python-ruff '(warning . exordium-python-mypy)))



(provide 'init-flycheck)
;;; init-flycheck.el ends here
