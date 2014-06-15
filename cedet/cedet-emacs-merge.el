;;; ceemme --- CEDET <-> Emacs merge helper

;;; Copyright (C) 2012 David Engster

;; Author: David Engster <dengste@eml.cc>

;; This file is not part of GNU Emacs.

;; CEDET is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This is a helper package for merging CEDET with Emacs which at
;; least takes care of the most tedious stuff when doing cross-project
;; merges, but still won't make it a thrilling experience (sorry).
;; Since it will only ever be used by CEDET maintainers, I didn't
;; polish it very much, and documentation is... well, sparse.
;; However, frobbing `ceemme-emacs-trunk' and `ceemme-cedet-repo' and
;; calling `ceemme' should give you an idea (be prepared to wait for
;; bzr, though).
;;
;; Basic workflow:
;; - Fire up ceemme.
;; - Look at commits which aren't yet marked.
;; - Mark commits like "merge from trunk" and the like as ignored
;;   using 'm i'.
;; - Look at details with RET and list affected files with 'f'.
;; - Test commit with 't' and investigate failed hunks.
;; - Apply the patch with 'a' and use ediff to manually deal with
;;   failed hunks.
;; - Generate commit message with 'c' (it will also be in the kill-ring).
;; - Call up vc-dir with 'v'.
;; - Commit, yank generated commit message, edit at will.
;; - Mark commit as applied or partly applied (the latter will require a
;;   comment regarding why).
;; - Check other commands with '?'.
;;
;; State of merge will be saved as soon as you mark a commit. You
;; should commit that state to the repository when you're done so that
;; others know what great deeds you've accomplished.
;;
;; IMPORTANT: This tool will only work with files which are in the
;; Emacs repository, so adding new files has to be done manually.

(require 'esh-util)			; For flatten lists
(require 'faces)

;; Location of Emacs trunk
(defvar ceemme-emacs-trunk (expand-file-name "~/emacs/trunk"))
;; Location of CEDET repository
;; The branches 'trunk', 'to-emacs' and 'from-emacs' must be present.
(defvar ceemme-cedet-repo (expand-file-name "~/cedet"))

(defvar ceemme-cedet-files nil)

(defvar ceemme-files-regexp "\\.\\(el\\|srt\\)$")
(defvar ceemme-files-ignore-regexp "\\(loaddefs\\|-wy\\|-by\\)\\.el")

(defvar ceemme-emacs-log-file (expand-file-name "~/.ceemme-emacstrunk-log.txt"))

(defvar ceemme-cedet-file-regexp
  "\\(?:lisp/cedet\\|lisp/emacs-lisp/eieio\\)")

(defvar ceemme-state nil)

(defvar ceemme-log-regexp1
  "^\\s-*\\([0-9.]+\\): \\(.+?\\) \\([0-9-]+\\) \\(.*\\)$")
(defvar ceemme-log-regexp2
  "^\\s-*\\([0-9.]+\\): \\(.+?\\) \\([0-9-]+\\) \\(\\[merge\\]\\) \\(.*\\)$")
(defvar ceemme-patch-out-regexp1
  "^Patching file \\(.+\\) using.*$")
(defvar ceemme-patch-out-regexp2
  "^\\(Hunk #[0-9]+\\) \\(FAILED\\) at.*$")
(defvar ceemme-patch-out-regexp3
  "^\\(Hunk #[0-9]+\\) \\(succeeded\\) at.*$")
(defvar ceemme-patch-out-regexp4
  "^\\(Reversed .or previously applied. patch detected.\\)")
(defvar ceemme-patch-out-regexp5
  "^\\(Hunk #[0-9]+\\) \\(ignored\\) at.*$")

(defvar ceemme-merge-from-trunk-regexp
  "\\[merge\\]\\s-*[mM]erge \\(from \\)?[tT]runk")

(defvar ceemme-patch-program "/usr/bin/patch")

;; Direction of merges; can be 'e2c or 'c2e.
(defvar ceemme-merge-direction 'e2c)

(defvar ceemme-log-entries nil)

(defvar ceemme-first-emacs-revision 106349)
(defvar ceemme-first-cedet-revision 7000)

(defvar ceemme-buffer-name "*ceemme main*")
(defvar ceemme-out-buffer-name "*ceemme out*")

(defvar ceemme-from nil)
(defvar ceemme-to nil)

(defsubst ceemme-buffer ()
  (with-current-buffer (get-buffer-create ceemme-buffer-name)
    (setq buffer-read-only nil)
    (current-buffer)))

(defsubst ceemme-out-buffer ()
  (with-current-buffer (get-buffer-create ceemme-out-buffer-name)
    (setq buffer-read-only nil)
    (current-buffer)))

(defvar ceemme-mark-faces
  '((applied ceemme-applied-face)
    (ignore ceemme-ignore-face)
    (partly ceemme-partly-face)))

(defface ceemme-applied-face
  '((t :background "dark green"))
  "Applied face.")

(defface ceemme-ignore-face
  '((t :background "dim gray" :foreground "black"))
  "Applied face.")

(defface ceemme-ignore-face
  '((t :background "dark blue"))
  "Applied face.")

(defvar ceemme-key->symbol
  '((?a applied)
    (?i ignore)
    (?p partly)))

(defvar ceemme-mode-map
  (let ((map (make-keymap)))
    (define-key map [(return)] 'ceemme-show-details)
    (define-key map [(d)] 'ceemme-show-diff)
    (define-key map [(f)] 'ceemme-show-affected-files)
    (define-key map [(\?)] 'ceemme-show-commands)
    (define-key map [(m)] 'ceemme-mark-revno)
    (define-key map [(i)] 'ceemme-ignore-merge-from-trunk)
    (define-key map [(h)] 'ceemme-toggle-visibility)
    (define-key map [(t)] 'ceemme-test-patch)
    (define-key map [(a)] 'ceemme-apply)
    (define-key map [(c)] 'ceemme-commit-message)
    (define-key map [(v)] 'ceemme-vc-dir)
    map)
  "keymap")

(defvar ceemme-patch-output-mode-map
  (let ((map (make-keymap)))
    (define-key map [(return)] 'ceemme-patch-goto)
    map)
  "keymap")

(defvar ceemme-patch-out-font-lock-keywords)
(defun ceemme-patch-output-mode ()
  "Major mode for patch output."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'ceemme-pach-output-mode)
  (setq mode-name "ceemme-patch")
  (set-syntax-table text-mode-syntax-table)
  (use-local-map ceemme-patch-output-mode-map)
  (make-local-variable 'font-lock-defaults)
  (setq ceemme-patch-out-font-lock-keywords
	`((,ceemme-patch-out-regexp1
	   (1 'link))
	  (,ceemme-patch-out-regexp2
	   (1 'link)
	   (2 'warning))
	  (,ceemme-patch-out-regexp3
	   (1 'link)
	   (2 'success))
	  (,ceemme-patch-out-regexp4
	   (1 'warning))
	  (,ceemme-patch-out-regexp5
	   (1 'link)
	   (2 'error))))
  (setq buffer-read-only t)
  (setq font-lock-defaults '(ceemme-patch-out-font-lock-keywords)))

(defun ceemme-toggle-visibility ()
  (interactive)
  (dolist (ov (overlays-in (point-min) (point-max)))
    (if (overlay-get ov 'invisible)
	(overlay-put ov 'invisible nil)
      (overlay-put ov 'invisible t))))

(defun ceemme-vc-dir ()
  (interactive)
  (message "Calling vc-dir on %s" ceemme-to)
  (vc-dir ceemme-to))

(defun ceemme-commit-message ()
  (interactive)
  (let ((revno (ceemme-get-revno))
	author message)
    (pop-to-buffer (ceemme-out-buffer))
    (erase-buffer)
    (ceemme-call-bzr "log" "-r" revno ceemme-from)
    (goto-char (point-min))
    (setq author
	  (save-excursion
	    (or (re-search-forward "^author: \\(.+\\)$" nil t)
		(re-search-forward "^committer: \\(.+\\)$" nil t))
	    (match-string 1)))
    (setq message
	  (save-excursion
	    (re-search-forward "^message:\n\\([^\0]*\\)" nil t)
	    (match-string 1)))
    (replace-regexp-in-string "^\\(\\s-*\\)" "" message nil nil 1)
    (erase-buffer)
    (insert "Author: " author "\n\n")
    (insert "Merged Emacs rev. " revno ".\n\n")
    (insert message)
    (kill-new (buffer-substring-no-properties (point-min) (point-max)))
    (message "Copied commit message to kill-ring.")))

(defun ceemme-patch-goto ()
  (interactive)
  (save-excursion
    (let ((props (text-properties-at (point)))
	  file hunk)
      (when (member 'link props)
	(beginning-of-line)
	(when (looking-at "^Patching file \\(.+\\) using.*$")
	  (setq file (match-string-no-properties 1))
	  (find-file (concat (file-name-as-directory ceemme-to) file)))
	(when (looking-at "^Hunk #\\([0-9]+\\)")
	  (setq hunk (match-string-no-properties 1))
	  (re-search-backward "^Patching file \\(.+\\) using.*$")
	  (setq file (match-string-no-properties 1))
	  (pop-to-buffer (ceemme-out-buffer))
	  (goto-char (point-min))
	  (re-search-forward (concat "=== modified file.*" (regexp-quote file)))
	  (diff-hunk-next (string-to-number hunk)))))))

(defun ceemme-get-revno ()
  (save-excursion
    (beginning-of-line)
    (looking-at "\\s-*\\([0-9.]+\\):")
    (match-string-no-properties 1)))

(defun ceemme-is-merge ()
  (save-excursion
    (beginning-of-line)
    (search-forward "[merge]" (point-at-eol) t)))

(defun ceemme-save-state ()
  (with-temp-buffer
    (prin1 ceemme-state (current-buffer))
    ;; Just cosmetics to make it more human readable
    (goto-char (point-min))
    (while (search-forward ")" nil t)
      (insert "\n"))
    (write-region (point-min) (point-max)
		  (expand-file-name "ceemme-state" ceemme-to ))
    (message "Saved ceemme state to %s." (expand-file-name "ceemme-state" ceemme-to))))

(defun ceemme-show-commands ()
  (interactive)
  (save-selected-window
    (pop-to-buffer (ceemme-out-buffer))
    (erase-buffer)
    (insert "Available commands on a commit:\n"
	    "RET - Show details\n"
	    "d - Show diff\n"
	    "f - Show affected files in CEDET repository\n"
	    "t - Test: Dry run of patch\n"
	    "a - Apply this patch\n"
	    "m - Mark commit\n"
	    "i - Ignore all \"merge from trunk\" commits\n"
	    "c - Generate commit message\n"
	    "h - Hide/Unhide all marked commits\n"
	    "v - Call vc-dir on target repository\n"
	    "? - This")))

(defun ceemme-mark-revno (mark &optional dontsave)
  (interactive "cMark (a=applied, p=partly applied, i=ignore): ")
  (let* ((revno (ceemme-get-revno))
	 (entry (assoc revno ceemme-state))
	 comment)
    (setq mark (cadr (assoc mark ceemme-key->symbol)))
    (unless mark
      (error "Unknown mark."))
    (when (or (equal mark 'partly)
	      (and (equal mark 'ignore)
		   (not (ceemme-is-merge))))
      (setq comment (read-from-minibuffer "Comment: ")))
    (if entry
	(setcdr entry (list mark comment))
      (push (list revno mark comment) ceemme-state)))
  (ceemme-set-mark-overlays)
  (unless dontsave
    (ceemme-save-state)))

(defun ceemme-ignore-merge-from-trunk ()
  (interactive)
  (goto-char (point-min))
  (while (not (eobp))
    (let* ((revno (ceemme-get-revno))
	   (entry (assoc revno ceemme-state)))
      (when (and (not entry)
		 (re-search-forward ceemme-merge-from-trunk-regexp
				    (point-at-eol) t))
	(ceemme-mark-revno ?i t))
      (forward-line 1)))
  (ceemme-save-state))

(defun ceemme-set-mark-overlays ()
  (interactive)
  (remove-overlays)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let* ((revno (ceemme-get-revno))
	     (entry (assoc revno ceemme-state))
	     ov)
	(when entry
	  (setq ov (make-overlay (point-at-bol) (1+ (point-at-eol))))
	  (overlay-put ov 'face
		       (cadr (assoc (cadr entry) ceemme-mark-faces)))))
      (forward-line 1))))

(defun ceemme-show-details ()
  (interactive)
  (save-selected-window
    (let ((revno (ceemme-get-revno)))
      (pop-to-buffer (ceemme-out-buffer))
      (erase-buffer)
      (ceemme-call-bzr-cedet-files "log" "-v" "-r" revno)
      (goto-char (point-min))
      (setq buffer-read-only t)
      (fundamental-mode))))

(defun ceemme-show-diff ()
  (interactive)
  (let ((revno (ceemme-get-revno)))
      (pop-to-buffer (ceemme-out-buffer))
      (erase-buffer)
      (ceemme-call-bzr-cedet-files "diff" "-c" revno)
      (goto-char (point-min))
      (setq buffer-read-only t)
      (diff-mode)))

(defun ceemme-apply (&optional dry-run)
  (interactive)
  (let ((revno (ceemme-get-revno)))
      (pop-to-buffer (ceemme-out-buffer))
      (setq default-directory ceemme-to)
      (erase-buffer)
      (ceemme-call-bzr-cedet-files "diff" "-c" revno)
      (goto-char (point-min))
      (ceemme-adapt-paths)
      (diff-mode)
      (setq buffer-read-only t)
      (if dry-run
	  (ceemme-call-patch (current-buffer) ceemme-to
			     "-p0" "--verbose" "-u" "--dry-run"
			     "--no-backup-if-mismatch" "-r" "-")
	(ceemme-call-patch (current-buffer) ceemme-to
			   "-p0" "--verbose" "-u" "--no-backup-if-mismatch"
			   "-r" "-"))
      (pop-to-buffer "*ceemme patch output*")
      (goto-char (point-min))
      (when dry-run
	(insert "DRY RUN!\n\n"))
      (save-excursion
	(while (re-search-forward
		"^Hmm.*like a unified[^\0]*?\nPatching file" nil t)
	  (replace-match "Patching file")))
      (ceemme-patch-output-mode)))

(defun ceemme-test-patch ()
  (interactive)
  (ceemme-apply t))

(defun ceemme-call-patch (buf dir &rest options)
  (with-current-buffer buf
    (let ((default-directory (file-name-as-directory dir))
	  (outbuf (get-buffer-create "*ceemme patch output*")))
      (with-current-buffer outbuf
	(setq buffer-read-only nil)
	(erase-buffer))
      (apply 'call-process-region (point-min) (point-max)
	     ceemme-patch-program
	     nil outbuf t options))))

(defun ceemme-build-filelist ()
  (eshell-flatten-list
   (append
    (ceemme-get-cedet-files-under
     (concat ceemme-emacs-trunk "/lisp/cedet"))
    (mapcar
     (lambda (x)
       (concat ceemme-emacs-trunk "/lisp/emacs-lisp/" x))
     (directory-files
      (concat ceemme-emacs-trunk "/lisp/emacs-lisp")
      nil "eieio.*\\.el$")))))

(defun ceemme-get-cedet-files-under (dir)
  (let ((files (directory-files-and-attributes dir)))
    (delq nil
	  (mapcar
	   (lambda (file)
	     (let ((x (car file)))
	       (if (and (eq t (cadr file))
			(not (or (string= x ".")
				 (string= x ".."))))
		   (ceemme-get-cedet-files-under
		    (concat (file-name-as-directory dir) x))
		 (when (and (string-match ceemme-files-regexp x)
			    (not (string-match ceemme-files-ignore-regexp x)))
		   (concat (file-name-as-directory dir) x)))))
	   files))))

(defun ceemme-adapt-paths ()
  (save-excursion
    (goto-char (point-min))
    (if (eq ceemme-merge-direction 'e2c)
	(while (re-search-forward
		(concat "^[+=-]\\{3\\} "
			"\\(?:modified file '\\)?"
			"\\(lisp/emacs-lisp/\\)") nil t)
	  (replace-match "lisp/eieio/" nil nil nil 1))
      (while (re-search-forward "^[+=-]\\{3\\} \\(lisp/eieio/\\)" nil t)
	(replace-match "lisp/emacs-lisp/" nil nil nil 1)))))

(defun ceemme-show-affected-files ()
  (interactive)
  (let ((revno (ceemme-get-revno)))
    (pop-to-buffer (ceemme-out-buffer))
    (ceemme-call-bzr "log" "-v" "-r" revno ceemme-from)
    (goto-char (point-min))
    (if (re-search-forward "^\\s-*\\(removed\\|added\\|modified\\|renamed\\):$" nil t)
	(progn
	  (forward-line 1)
	  (delete-region (point-min) (point))
	  (while (not (eobp))
	    (if (or (looking-at (concat "^\\s-*" ceemme-cedet-file-regexp))
		    (looking-at "^\\s-*\\(removed\\|added\\|modified\\|renamed\\):$"))
		(forward-line 1)
	      (delete-region (point-at-bol) (1+ (point-at-eol))))))))
  (setq buffer-read-only t))

(defsubst ceemme-call-bzr (command &rest args)
  (apply 'call-process "bzr" nil (current-buffer) nil command args))

(defsubst ceemme-call-bzr-cedet-files (command &rest args)
    (apply 'call-process "bzr" nil (current-buffer) nil command (append args ceemme-cedet-files)))

(defvar ceemme-mode-font-lock-keywords)

(defun ceemme-mode ()
  "Major mode for CEDET Emacs merging."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'ceemme-mode)
  (setq mode-name "ceemme")
  (set-syntax-table text-mode-syntax-table)
  (use-local-map ceemme-mode-map)
  (make-local-variable 'font-lock-defaults)
  (setq ceemme-mode-font-lock-keywords
	(list (list ceemme-log-regexp1
		    '(1 font-lock-constant-face)
		    '(2 font-lock-string-face)
		    '(3 font-lock-keyword-face))
	      (list ceemme-log-regexp2
		    '(1 font-lock-constant-face)
		    '(2 font-lock-string-face)
		    '(3 font-lock-keyword-face)
		    '(4 font-lock-warning-face))))
  (ceemme-set-mark-overlays)
  (setq buffer-read-only t)
  (message "CEDET <-> Emacs merge mode. Type '?' to get list of available commands.")
  (setq font-lock-defaults '(ceemme-mode-font-lock-keywords)))

(defun ceemme-load-state ()
  (when (file-exists-p (expand-file-name "ceemme-state" ceemme-to))
    (with-temp-buffer
      (insert "(setq ceemme-state '")
      (insert-file-contents (expand-file-name "ceemme-state" ceemme-to))
      (goto-char (point-max))
      (insert ")")
      (eval-buffer))))

(defun ceemme-filelist-c2e (filelist)
  (let ((l (length ceemme-emacs-trunk)))
  (mapcar
   (lambda (x)
     (when (string-match "/lisp/emacs-lisp/" x)
       (setq x (replace-match "/lisp/eieio/" nil t x)))
     (concat ceemme-cedet-repo (substring x l)))
   filelist)))

(defun ceemme ()
  (interactive)
  (switch-to-buffer (ceemme-buffer))
  (erase-buffer)
  (if (eq ceemme-merge-direction 'e2c)
      (setq ceemme-cedet-files (ceemme-build-filelist)
	    ceemme-from ceemme-emacs-trunk
	    ceemme-to (expand-file-name "from-emacs" ceemme-cedet-repo))
    (setq ceemme-cedet-files
	  (ceemme-filelist-c2e
	   (ceemme-build-filelist))
	  ceemme-from (expand-file-name "trunk" ceemme-cedet-repo)
	  ceemme-to (expand-file-name "to-emacs" ceemme-cedet-repo)))
  (if (and (eq ceemme-merge-direction 'e2c)
	   (file-exists-p ceemme-emacs-log-file)
	   (y-or-n-p "Use cached Emacs revisions? "))
      (insert-file-contents ceemme-emacs-log-file)
    (message "Generating list of revisions.")
    (if (eq ceemme-merge-direction 'e2c)
	(ceemme-call-bzr-cedet-files "log" "-n0" "--line" "-r"
				     (format "%d.." ceemme-first-emacs-revision))
      (ceemme-call-bzr-cedet-files "log" "-n0" "--line" "-r"
				   (format "%d.." ceemme-first-cedet-revision)))
    (when (eq ceemme-merge-direction 'e2c)
      (write-region (point-min) (point-max) ceemme-emacs-log-file)))
  (ceemme-load-state)
  (ceemme-mode)
  (setq default-directory (file-name-as-directory ceemme-from))
  (font-lock-mode))
