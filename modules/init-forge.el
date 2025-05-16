;;; init-forge.el --- Forge - work with Git forges Magit -*- lexical-binding: t -*-

;;; Commentary:
;;
;; ----------------- ---------------------------------------------------------
;; Key               Definition
;; ----------------- ---------------------------------------------------------
;; C-c M-p           Markdown preview (in `forge-post-mode')
;; C-c M-r           Forge post submit as draft (in `forge-post-mode')
;; C-c M-d           Forge show diff for pull request (in `forge-post-mode')
;; C-c M-r           Forge mark pull request at point mark as ready for review
;;                   (in `magit-status-mode' and in `forge-topic-mode')
;; C-c M-c           Insert messages from all commits that are part of
;;                   a pull-request (in `forge-post-mode')

;;; Code:

(eval-when-compile
  (unless (featurep 'init-require)
    (load (file-name-concat (locate-user-emacs-file "modules") "init-require"))))
(exordium-require 'init-git)
(exordium-require 'init-markdown)

(require 'cl-lib)

;;; Magit Forge

(defvar-local exordium--forge-diff-buffer-window-configuration nil
  "A cons in form (DIFF-BUFFER . WINDOW-CONFIGURATION).
The DIFF-BUFFER is diff buffer created for a pull-request (if
any), while WINDOW-CONFIGURATION is a windows configuration prior
to displaying new-pullreq buffer.")

(use-package forge
  :functions (exordium-forge--add-draft
              exordium-ghub-graphql--pull-request-id
              exordium-ghub-grqphql--mark-pull-request-ready-for-review
              exordium-forge-markdown-preview
              exordium-forge-post-submit-draft
              exordium-forge-mark-ready-for-rewiew
              exordium-forge-insert-pullreq-commit-messages
              exordium--forge-diff-for-pullreq
              exordium--forge-store-window-configuration
              exordium--forge-kill-diff-buffer-restore-window-configuration)
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
  (use-package forge-commands
    :ensure forge
    :defer t
    :autoload (forge-create-pullreq--read-args))
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


  (defun exordium--forge-diff-for-pullreq (source target)
                                        ; checkdoc-params: (source target)
    "Show diff for the current pull-request."
    (when-let* ((magit-commit-show-diff)
                (pullreq-buffer (current-buffer))
                (pullreq-window (frame-selected-window)))
      (when exordium-use-magit-fullscreen
        (delete-other-windows))
      (let ((diff-buffer (magit-diff-range (format "%s..%s" target source))))
        (with-current-buffer pullreq-buffer
          (setq exordium--forge-diff-buffer-window-configuration
                (cons diff-buffer
                      (cdr exordium--forge-diff-buffer-window-configuration)))))
      (select-window pullreq-window)))

  (defun exordium--forge-store-window-configuration (orig-fun &rest args)
                                        ; checkdoc-params: (orig-fun args)
    "Store windows configuration."
    (let ((window-configuration (current-window-configuration))
          (pullreq-buffer (apply orig-fun args)))
      (with-current-buffer pullreq-buffer
        (setq exordium--forge-diff-buffer-window-configuration
              (cons nil
                    window-configuration)))
      pullreq-buffer))

  (defun exordium--forge-kill-diff-buffer-restore-window-configuration (orig-fun &rest args)
                                        ; checkdoc-params: (orig-fun args)
    "Kill a diff buffer and restore windows configuration."
    (pcase-let ((`(,diff-buffer . ,window-configuration)
                 exordium--forge-diff-buffer-window-configuration))
      (apply orig-fun args)
      (when diff-buffer
        (with-current-buffer diff-buffer
          (magit-mode-bury-buffer 'kill))
        (when window-configuration
          (set-window-configuration window-configuration)))))


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

  (defun exordium-forge-post-submit-draft ()
    "Submit the post that is being edited in the current buffer as a draft.
This relies on implementation of `forge--topic-parse-buffer', that requires
a key `draft' to have a value of t."
    (interactive)
    (setq-local forge-buffer-draft-p t)
    (forge-post-submit))

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


  (defun exordium-forge-insert-pullreq-commit-messages (source target)
    "Insert messages from all commits that are part of a pull-request.
Commits that are part of accessible from SOURCE but not from
TARGET.  When called interactively SOURCE and TARGET are by
default the same as the ones in forge's pull-request buffer. When
called with a prefix arg or not from a pull-request buffer then
ask for SOURCE and TARGET."
    (interactive (if (and forge--buffer-base-branch forge--buffer-head-branch
                          (not current-prefix-arg))
                     (list forge--buffer-head-branch forge--buffer-base-branch)
                  (forge-create-pullreq--read-args)))
    (dolist (commit (magit-git-lines "rev-list" "--left-only"
                                     (format "%s...%s" source target)))
      (insert "## ")
      (magit-rev-insert-format "%B" commit)))


  (use-package magit
    :defer t
    :bind
    (:map magit-status-mode-map
     ("C-c M-r" . #'exordium-forge-mark-ready-for-rewiew)))

  :bind
  (:map forge-post-mode-map
   ("C-c M-p" . #'exordium-forge-markdown-preview)
   ("C-c M-r" . #'exordium-forge-post-submit-draft)
   ("C-c M-d" . #'exordium--forge-diff-for-pullreq)
   ("C-c M-c" . #'exordium-forge-insert-pullreq-commit-messages)
   :map forge-topic-mode-map
   ("C-c M-r" . #'exordium-forge-mark-ready-for-rewiew))

  :config
  (advice-add 'forge--prepare-post-buffer
              :around #'exordium--forge-store-window-configuration)
  (advice-add 'forge-create-pullreq
              :after #'exordium--forge-diff-for-pullreq)
  (advice-add 'forge-post-cancel
              :around #'exordium--forge-kill-diff-buffer-restore-window-configuration)
  (advice-add 'forge-post-submit
              :around #'exordium--forge-kill-diff-buffer-restore-window-configuration)

  (with-eval-after-load 'forge-post
    (dolist (suffix '(("M-p" "Markdown preview" exordium-forge-markdown-preview)
                      ("M-d" "Diff" exordium--forge-diff-for-pullreq)
                      ("M-r" "Submit draft" exordium-forge-post-submit-draft)))
      (unless (ignore-errors
                  (transient-get-suffix 'forge-post-dispatch (car suffix)))
        (transient-append-suffix 'forge-post-dispatch
          "C-c" suffix)))))

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
  (use-package closql
    :defer t
    :autoload (closql-delete))

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
