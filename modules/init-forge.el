;;;; Forge - work with Git forges Magit
;;;
;;; ----------------- ---------------------------------------------------------
;;; Key               Definition
;;; ----------------- ---------------------------------------------------------
;;; C-c C-p           Markdown preview (in `forge-post-mode')
;;; C-c C-d           Forge post submit as draft (in `forge-post-mode')
;;; C-c C-d           Forge mark pull request at point mark as ready for review
;;;                   (in `magit-status-mode' and in `forge-topic-mode')



;;; Magit Forge
(use-package forge
  :defer t
  :init

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
    (if-let ((url (forge-get-url (or (forge-post-at-point)
                                     (forge-current-topic))))
             (_ (string-match
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
  :hook
  (forge-post-mode . (lambda ()
                       (set-fill-column 1000)))
  :bind
  (:map forge-post-mode-map
        ("C-c C-p" . #'exordium-forge-markdown-preview)
        ("C-c C-d" . #'exordium-forge-post-submit-draft)
   :map magit-status-mode-map
        ("C-c C-d" . #'exordium-forge-mark-ready-for-rewiew)
   :map forge-topic-mode-map
        ("C-c C-d" . #'exordium-forge-mark-ready-for-rewiew)))


(provide 'init-forge)
