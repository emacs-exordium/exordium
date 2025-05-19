;;; init-company.el --- company-mode                 -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (unless (featurep 'init-require)
    (load (file-name-concat (locate-user-emacs-file "modules") "init-require"))))
(exordium-require 'init-prefs)
(exordium-require 'init-git)

(require 'cl-lib)

(when (version< "29" emacs-version)
  (exordium-require 'init-forge))

(use-package company
  :diminish "CA"
  :demand t
  :commands (company-other-backend
             company-abort)
  :custom
  (company-idle-delay nil)
  (company-files-exclusions '(".git/" ".gitignore" ".gitmodules" ".DS_Store"
                              ".vscode/" ".envrc" ".direnv/" ".clangd"
                              "venv/" ".venv/"))
  (company-transformers '(delete-consecutive-dups))

  :config
  (add-to-list 'company-backends
               '(company-capf company-yasnippet company-files
                 :with company-dabbrev-code))

  ;; Turn on company mode everywhere
  (global-company-mode)

  :bind
  (("C-." . #'company-complete)
   ("C-c C-\\" . #'company-other-backend)))

(use-package company
  :diminish "CA"
  :if (version< "29" emacs-version)
  :defer t
  :commands (company-begin-backend)
  :functions (exordium--company-conventional-commits-type-p)
  :init
  (use-package forge-core
    :ensure forge
    :defer t
    :autoload (forge-get-repository))

  (use-package git-commit
    :ensure magit)

  (defun exordium-company-assignees (command &optional arg &rest _ignored)
    "A `company-mode' backend for assigneees in `forge-mode' repository."
    (interactive (list 'interactive))
    (cl-case command
      (interactive (company-begin-backend 'exordium-company-assignees))
      (prefix
       (save-match-data
         (when (and (or (bound-and-true-p git-commit-mode)
                        (derived-mode-p 'forge-post-mode
                                        'git-commit-elisp-text-mode))
                    (forge-get-repository 'full)
                    (looking-back
                     (rx "@"
                         (zero-or-one
                          (group alphanumeric
                                 (repeat 0 38
                                         (or alphanumeric
                                             (seq (any "-/") alphanumeric))))))
                     (max (- (point) 40)
                          (line-beginning-position))))
           ;; IDK how to match end of a 'symbol' that is equal to an "@" or is
           ;; equal to "@foo" in neither `git-commit-mode' nor
           ;; `forge-post-mode'. Hence it's handled manually.  The
           ;; `looking-back' above matches an "@" or an "@foo". When it was the
           ;; latter there was a match in group 1.  Now, check if this is at
           ;; the very end of the "@" or "@foo".  Note that "@<point>@" also
           ;; matches. Probably a few other characters, substituting the second
           ;; "@" in latter pattern, would also give a positive result. Yet, in
           ;; such a case the `match' is "", so that's all fine - all
           ;; candidates will be shown.
           (when (or (save-match-data (looking-at "\\W"))
                     (= (point) (point-max)))
             (cons (or (match-string 1) "") t)))))
      (candidates (when-let* ((repo (forge-get-repository :tracked?))
                              ((slot-exists-p repo 'assignees))
                              ((slot-boundp repo 'assignees))
                              (assignees (oref repo assignees)))
                    (cl-remove-if-not
                     (lambda (assignee)
                       (string-prefix-p arg assignee))
                     (mapcar (lambda (assignee)
                               (propertize (cadr assignee)
                                           'full-name (caddr assignee)))
                             assignees))))
      (annotation (when-let* ((assignee (get-text-property 0 'full-name arg)))
                    (format " [%s]" assignee)))))

  (defconst exordium--company-conventional-commits-types
    (list
     ;; From Conventional Commits: https://www.conventionalcommits.org
     ;; and Angular: https://github.com/angular/angular/blob/22b96b9/CONTRIBUTING.md#type
     (propertize
      "build"
      'exordium-cc-annotation
      "Changes that affect the build system or external dependencies")
     (propertize
      "ci"
      'exordium-cc-annotation
      "Changes to CI configuration files and scripts")
     "chore"
     (propertize
      "docs"
      'exordium-cc-annotation
      "Documentation only changes")
     (propertize
      "feat"
      'exordium-cc-annotation
      "Documentation only changes")
     (propertize
      "fix"
      'exordium-cc-annotation
      "A bug fix")
     (propertize
      "perf"
      'exordium-cc-annotation
      "A change that improves performance")
     (propertize
      "refactor"
      'exordium-cc-annotation
      "A change that neither fixes a bug nor adds a feature")
     (propertize
      "revert"
      'exordium-cc-annotation
      "A change that reverts other commit(s)")
     (propertize
      "style"
      'exordium-cc-annotation
      "Changes that do not affect the meaning of the code ")
     (propertize
      "test"
      'exordium-cc-annotation
      "Adding missing tests or correcting existing tests")
     )
    "A list of Conventional Commit types that appear in completion candidates.")

  (defconst exordium--company-conventional-commits-longest-type
    (apply #'max (mapcar #'length exordium--company-conventional-commits-types)))

  (defconst exordium--company-conventional-commits-footers
    (append
     (list
     ;; From Conventional Commits: https://www.conventionalcommits.org
     ;; and Angular: https://github.com/angular/angular/blob/22b96b9/CONTRIBUTING.md#type
     (propertize
      "BREAKING CHANGE"
      'exordium-cc-annotation
      "Explanation of breaking changes")
     ;; From GitHub:
     ;; https://docs.github.com/en/get-started/writing-on-github/working-with-advanced-formatting/using-keywords-in-issues-and-pull-requests#linking-a-pull-request-to-an-issue
     (propertize
      "close"
      'exordium-cc-annotation
      "Closes issue # on GitHub")
     (propertize
      "closes"
      'exordium-cc-annotation
      "Closes issue # on GitHub")
     (propertize
      "closed"
      'exordium-cc-annotation
      "Closes issue # on GitHub")
     (propertize
      "fix"
      'exordium-cc-annotation
      "Fixes and closes issue # on GitHub")
     (propertize
      "fixes"
      'exordium-cc-annotation
      "Fixes and closes issue # on GitHub")
     (propertize
      "fixed"
      'exordium-cc-annotation
      "Fixes and closes issue # on GitHub")
     (propertize
      "resolve"
      'exordium-cc-annotation
      "Resolves and closes issue # on GitHub")
     (propertize
      "resolves"
      'exordium-cc-annotation
      "Resolves and closes issue # on GitHub")
     (propertize
      "resolved"
      'exordium-cc-annotation
      "Resolves and closes issue # on GitHub")
     ;; Top 1% of trailers: https://lore.kernel.org/git/60ad75ac7ffca_2ae08208b@natae.notmuch/
     "acked-by"
     "reviewed-by"
     )
     git-commit-trailers)
    "A list of Conventional Commit footers that appear in completion candidates.")

  (defconst exordium--company-conventional-commits-longest-footer
    (apply #'max (mapcar #'length exordium--company-conventional-commits-footers)))

  (defun exordium--company-conventional-commits-type-p ()
    "Return non-nil when point is at possible type completion.
Type completion can possibly happen in first line or in any line
that is preceded by lines that are either empty or contain only
whitespaces."
    (or (equal 1 (line-number-at-pos (point)))
        (when-let* ((pos-last-empty
                     (save-excursion
                       (goto-char (point-min))
                       (re-search-forward
                        (rx-to-string
                         `(seq string-start (one-or-more (or "\n" whitespace))))
                        nil t))))
          (<= (compat-call pos-bol) pos-last-empty))))

  (defun exordium-company-conventional-commits (command &optional arg &rest _ignored)
    "A `company-mode' backend for Conventional Commits.
See https://www.conventionalcommits.org for details."
    (interactive (list 'interactive))
    (cl-case command
      (interactive (company-begin-backend 'company-simple-backend))
      (prefix
       (when (or (bound-and-true-p git-commit-mode)
                 (derived-mode-p 'forge-post-mode
                                 'git-commit-ts-mode
                                 'git-commit-elisp-text-mode))
         (let ((col (current-column))
               (type (exordium--company-conventional-commits-type-p)))
           (when (and
                  (< col (if type
                             exordium--company-conventional-commits-longest-type
                           exordium--company-conventional-commits-longest-footer))
                  (save-excursion
                    (goto-char (compat-call pos-bol)) ; since Emacs-29
                    (re-search-forward
                     (rx-to-string
                      `(seq line-start
                            (= ,col ,@(if type '(alnum) '((or alnum space))))))
                     nil t)))
             (match-string 0)))))
      (candidates
       (let ((candidates
              (if (exordium--company-conventional-commits-type-p)
                  exordium--company-conventional-commits-types
                exordium--company-conventional-commits-footers)))
         (cl-remove-if-not (lambda (candidate)
                             (string-prefix-p arg candidate))
                           candidates)))
      (annotation
       (when-let* ((annotation (get-text-property
                                0 'exordium-cc-annotation arg)))
         (format " [%s]" annotation)))))

  :config
  ;; This is block is deferred , so this backed will end up first
  (add-to-list 'company-backends 'exordium-company-assignees)
  (add-to-list 'company-backends 'exordium-company-conventional-commits))

(use-package company-statistics
  :after (company)
  :config
  (company-statistics-mode)
  (add-to-list 'company-transformers
               'company-sort-by-backend-importance 'append))

(provide 'init-company)

;;; init-company.el ends here
