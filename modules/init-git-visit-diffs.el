;;; init-git-visit-diffs.el --- View changes in a git repo in narrowed buffers
;;;
;;; This extension finds all the changes ("hunks") between a working copy of a
;;; git repository and a reference (branch name or hashref) and presents them
;;; as a series of narrowed buffers.
;;;
;;; Interactive functions:
;;;
;;; blp-git-visit-diffs (ref) : prompts for a ref, to be passed to `git diff`;
;;; collects the hunks and view the first hunk, if any. Operates from the
;;; current directory.
;;; blp-git-visit-diffs-next () : view the next hunk, if any.
;;; blp-git-visit-diffs-prev () : view the previous hunk, if any.


;;; Global variables and types

(defvar *blp-git-visit-current-hunk-list*)
(defvar *blp-git-visit-previous-hunk-list*)
(define-error 'git-error "Git error")


;;; Utility functions

(defun blp-git-visit-visit-modified-region (filename line count)
  "Visit a hunk in a narrowed buffer"
  (find-file filename)
  (widen)
  (goto-line line)
  (beginning-of-line)
  (let ((beginning-position (line-beginning-position)))
    (next-line count)
    (narrow-to-region beginning-position (line-beginning-position))
    (beginning-of-buffer)))


(defun blp-git-visit-get-hunk-list (dir ref)
  "Return a list of changes between the current working copy and a git ref"
  ;; one triplet per change, like this: ((file1 13 8) (file1 19 63) (file2 24 12))
  (save-excursion
    (let (hunk-list file)
      (with-temp-buffer
      ;; (with-current-buffer (get-buffer-create "*blp-git-visit-output*") (erase-buffer)
        (if (zerop (call-process "git" nil (current-buffer) t "diff" ref))
            (progn
              (beginning-of-buffer)
              (while (re-search-forward "\\+\\+\\+ \\(.*\\)\\|@@ -[0-9]+,[0-9]+ \\+\\([0-9]+\\),\\([0-9]+\\) @@" nil t)
                (if (match-string 1)
                    (setq file
                          (and
                           (not (string-equal (match-string 1) "/dev/null"))
                           (concat dir (substring (match-string 1) 2))))
                  (if file
                      (setq hunk-list
                            (cons (list file
                                        (string-to-number (match-string 2))
                                        (string-to-number (match-string 3)) )
                                  hunk-list))) )))
          (signal 'git-error (buffer-substring-no-properties (buffer-end -1) (buffer-end 1)))))
      hunk-list)))

;; (blp-git-visit-get-hunk-list "./" "master")


;;; Interactive functions

(defun blp-git-visit-diffs-next ()
  "Show next diff in narrowed buffer."
  (interactive)
  (if *blp-git-visit-current-hunk-list*
      (let ((next-hunk (car *blp-git-visit-current-hunk-list*)))
        (setq *blp-git-visit-previous-hunk-list* (cons next-hunk *blp-git-visit-previous-hunk-list*))
        (setq *blp-git-visit-current-hunk-list* (cdr *blp-git-visit-current-hunk-list*))
        (apply 'blp-git-visit-visit-modified-region next-hunk))
    (message "at end of hunk list")))

(defun blp-git-visit-diffs-prev ()
  "Show previous diff in narrowed buffer."
  (interactive)
  (if (and *blp-git-visit-previous-hunk-list* (cdr *blp-git-visit-previous-hunk-list*))
      (let ((next-hunk (cadr *blp-git-visit-previous-hunk-list*)))
        (setq *blp-git-visit-current-hunk-list* (cons (car *blp-git-visit-previous-hunk-list*) *blp-git-visit-current-hunk-list*))
        (setq *blp-git-visit-previous-hunk-list* (cdr *blp-git-visit-previous-hunk-list*))
        (apply 'blp-git-visit-visit-modified-region next-hunk))
    (message "at beginning of hunk list")))

(defun blp-git-visit-diffs (ref)
  "Finds the diffs between REF and working copy. Shows the first diff in a narrowed buffer."
  (interactive "sref: ")
  (let ((dir (ignore-errors
               (file-name-as-directory (car (process-lines "git" "rev-parse" "--show-toplevel"))))))
    (if (not dir)
        (error (message "%s" "cannot locate repository root"))
      (condition-case error
          (progn
            (setq *blp-git-visit-current-hunk-list* (blp-git-visit-get-hunk-list dir ref))
            (setq *blp-git-visit-previous-hunk-list* nil)
            ;; (print *blp-git-visit-current-hunk-list* (get-buffer "*scratch*"))
            (blp-git-visit-diffs-next))
        (git-error (message (cadr error)))))))

(provide 'init-git-visit-diffs)
