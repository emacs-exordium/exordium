;;; init-forge.el --- Forge - work with Git forges Magit -*- lexical-binding: t -*-

;;; Commentary:
;;
;; ----------------- ---------------------------------------------------------
;; Key               Definition
;; ----------------- ---------------------------------------------------------
;; C-c C-p           Markdown preview (in `forge-post-mode')
;; C-c C-d           Forge post submit as draft (in `forge-post-mode')
;; C-c C-d           Forge mark pull request at point mark as ready for review
;;                   (in `magit-status-mode' and in `forge-topic-mode')

;;; Code:

(eval-when-compile
  (unless (featurep 'init-require)
    (load (file-name-concat (locate-user-emacs-file "modules") "init-require"))))
(exordium-require 'init-git)
(exordium-require 'init-markdown)

(require 'cl-lib)

;;; Magit Forge
(use-package forge
  :functions (exordium-forge--add-draft
              exordium-ghub-graphql--pull-request-id
              exordium-ghub-grqphql--mark-pull-request-ready-for-review
              exordium-forge-markdown-preview
              exordium-forge-post-submit-draft
              exordium-forge-mark-ready-for-rewiew)
  :defer t
  :init
  (use-package ghub-graphql
    :ensure ghub
    :defer t
    :autoload (ghub-graphql))
  (use-package forge-post
    :ensure forge
    :defer t
    :commands (forge-post-submit)
    :autoload (forge-post-at-point))
  (use-package forge-topic
    :ensure forge
    :defer t
    :autoload (forge-current-topic))
  (use-package magit-git
    :ensure magit
    :defer t
    :autoload (magit-git-string))
  (use-package markdown-mode
    :defer t
    :autoload (markdown-preview))

  (defun exordium-forge-markdown-preview ()
    "Preview current buffer as a preview in a `markdown-mode' buffer would do."
    (interactive)
    (let ((temp-file (make-temp-file (file-name-base buffer-file-name)
                                     nil ".md"
                                     (buffer-string))))
      (with-temp-buffer
        (insert-file-contents temp-file t)
        (markdown-preview))
      (delete-file temp-file)))

  (defun exordium-forge--add-draft (alist)
    "Add draft to ALIST."
    (append alist '((draft . "t"))))

  (defun exordium-forge-post-submit-draft ()
    "Submit the post that is being edited in the current buffer as a draft.
This relies on implementation of `forge--topic-parse-buffer', that requires
a key `draft' to have a value of t."
    (interactive)
    (advice-add 'forge--topic-parse-buffer
                :filter-return #'exordium-forge--add-draft)
    (condition-case err
        (forge-post-submit)
      (t
       (advice-remove 'forge--topic-parse-buffer #'exordium-forge--add-draft)
       (signal (car err) (cdr err))))
    (advice-remove 'forge--topic-parse-buffer #'exordium-forge--add-draft))

  (cl-defun exordium-ghub-graphql--pull-request-id
      (owner name number &key username auth host)
    "Return the id of the PR specified by OWNER, NAME, and NUMBER.
USERNAME, AUTH, and HOST behave as for `ghub-request'."
    (let-alist (ghub-graphql
                "query($owner:String!, $name:String!, $number:Int!) {
                 repository(owner:$owner, name:$name) {
                   pullRequest(number:$number) { id }}}"
                `((owner . ,owner)
                  (name . ,name)
                  (number . ,number))
                :username username :auth auth :host host)
      .data.repository.pullRequest.id))

  (cl-defun exordium-ghub-grqphql--mark-pull-request-ready-for-review
      (id &key username auth host)
    "Mark the pull request with the specified ID as ready for review.
Return t if pull request is a draft.  Return nil otherwise.
USERNAME, AUTH, and HOST behave as for `ghub-request'."
    (let-alist (ghub-graphql
                "mutation($id:String!) {
                   markPullRequestReadyForReview(input:{pullRequestId:$id}) {
                     pullRequest { isDraft }}}"
                `((id . ,id))
                :username username :auth auth :host host
                ;; because of GHE 2.20
                :headers '(("Accept" .
                            "application/vnd.github.shadow-cat-preview+json")))
      .data.markPullRequestReadyForReview.pullRequest.isDraft))

  (defun exordium-forge-mark-ready-for-rewiew ()
    "Mark the thing (the PR) at point as ready for review."
    (interactive)
    (if-let* ((url (forge-get-url (or (forge-post-at-point)
                                      (forge-current-topic))))
              ((string-match
                "//\\([^/]+\\)/\\([^/]+\\)/\\([^/]+\\)/pull/\\([0-9]+\\)$"
                url))
              (number (match-string 4 url))
              (host (car (alist-get (match-string 1 url)
                                    forge-alist
                                    nil nil #'string=)))
              (username (magit-git-string "config"
                                          (concat "github." host ".user")))
              (id (exordium-ghub-graphql--pull-request-id
                   (match-string 2 url)
                   (match-string 3 url)
                   (string-to-number number)
                   :username username :auth 'forge :host host))
              (not-a-draft-anymore
               (not (exordium-ghub-grqphql--mark-pull-request-ready-for-review
                     id
                     :username username :auth 'forge :host host))))
        (message "PR #%s is ready for review." number)
      (user-error "Nothing at point that is a PR or mark failed")))

  (use-package magit
    :defer t
    :bind
    (:map magit-status-mode-map
     ("C-c C-d" . #'exordium-forge-mark-ready-for-rewiew)))
  :hook
  (forge-post-mode . (lambda ()
                       (set-fill-column 1000)))
  :bind
  (:map forge-post-mode-map
   ("C-c C-p" . #'exordium-forge-markdown-preview)
   ("C-c C-d" . #'exordium-forge-post-submit-draft)
   :map forge-topic-mode-map
   ("C-c C-d" . #'exordium-forge-mark-ready-for-rewiew)))

(use-package forge-db
  :ensure forge
  :functions (exordium-forge-cleanup-known-repositories--concat
              exordium-forge-cleanup-known-repositories--question
              exordium-forge-cleanup-known-repositories)
  :autoload (forge-sql)
  :init
  (use-package async
    :defer t
    :autoload (async-inject-variables))
  (use-package magit-mode
    :ensure magit
    :defer t
    :autoload (magit-refresh))

  (defun exordium-forge-cleanup-known-repositories--question (to-delete &optional number)
    "Return a question about deletion of up to NUMBER of TO-DELETE repositories.

Only up to NUMBER first elements from TO-DELETE are included in
the returned question.  When length of TO-DELETE is greater than
NUMBER the *Messages* buffer is populated with all elements in
TO-DELETE list.

Default value of NUMBER is 5.

Each element of TO-DELETE is in the same format as used in
`exordium-forge-cleanup-known-repositories'."
    (let* ((length (length to-delete))
           (reminder (- length (or number 5)))
           (question
            "Do you really want to remove the following from the db? [%s] "))
      (if (<= reminder 0)
          (format question
                  (exordium-forge-cleanup-known-repositories--concat to-delete))
        (message "Repositories to delete from the db: %s"
                 (exordium-forge-cleanup-known-repositories--concat to-delete))
        (format (concat
                 question
                 "and %s other %s (see *Messages* for a full list)? ")
                (exordium-forge-cleanup-known-repositories--concat
                 (cl-subseq to-delete 0 (- reminder)))
                reminder
                (if (= 1 reminder) "repository" "repositories")))))

  (defun exordium-forge-cleanup-known-repositories--concat (to-delete)
    "Return a concatenation of TO-DELETE repositories.
Each element of TO-DELETE is in the same format as used in
`exordium-forge-cleanup-known-repositories'."
    (mapconcat
     (lambda (repo)
       (pcase-let ((`(,host ,owner ,name) (cdr repo)))
         (format "%s/%s @%s" owner name host)))
     to-delete
     ", "))

  (defun exordium-forge-cleanup-known-repositories ()
    "Cleanup forge repositories whose worktree doesn't exist anymore."
    (interactive)
    (if-let* ((to-delete (cl-remove-if
                          (lambda (repo)
                            (if-let* ((worktree (car repo)))
                                (file-exists-p worktree)
                              t))
                          (forge-sql [:select [worktree githost owner name]
                                              :from repository
                                              :order-by [(asc owner) (asc name)]]
                                     [worktree githost owner name]))))
        (when (yes-or-no-p (exordium-forge-cleanup-known-repositories--question
                            to-delete))
          (async-start
           ;; Deletion can be very slow and could block UI. To be executed in a
           ;; child process.
           (lambda ()
             (package-initialize)
             (require 'forge)
             (async-inject-variables "\\`\\(to-delete\\|forge-alist\\)\\'")
             (let (results)
               (dolist (repo to-delete)
                 (when-let* ((forge-repo (forge-get-repository (cdr repo))))
                   (let ((t0 (current-time))
                         ;; Timeout is huge (10x what was the default at the
                         ;; time of writing this) as db ops are long sometimes.
                         ;; And this is happening in a child process anyway.
                         (emacsql-global-timeout 300))
                     (closql-delete forge-repo)
                     (setq results
                           (cons (append (cdr repo)
                                         (list (float-time (time-since t0))))
                                 results)))))
               results))
           (lambda (results)
             (when results (magit-refresh))
             (dolist (repo results)
               (pcase-let ((`(,host ,owner ,name ,time) repo))
                 (message "- Deleted %s/%s @%s - took %.06fs"
                          owner name host time)))
             (message "Cleanup complete. Deleted %s %s from the db."
                      (length results)
                      (if (= 1 (length results)) "repository" "repositories")))))
      (message "Nothing to cleanup"))))


(provide 'init-forge)

;;; init-forge.el ends here
