;;; cedet-update-changelog --- Utility for updating changelogs in CEDET.

;;; Copyright (C) 2005, 2008, 2009, 2010, 2012 Eric M. Ludlam
;;;               2012 David Engster

;; Author: Eric M. Ludlam <zappo@gnu.org>

;; This file is not part of GNU Emacs.

;; Semantic is free software; you can redistribute it and/or modify
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
;; The CEDET project does not maintain ChangeLog files under version
;; control.  However, since people often find them useful, they are
;; generated from the version control logs and shipped with CEDET
;; releases.  In the CVS days, this was done through rcs2log, but this
;; obviously does not work with bzr.  While bzr does support ChangeLog
;; generation through the '--gnu-changelog' option, this has a few
;; quirks, which is why we do the conversion ourselves.

;;; History:
;;
;; Mostly rewritten in 3/2012 to support conversion from bzr logs.

(require 'cedet)

;;; Code:

(defvar cuc-bzr-log-regexp
  (concat
   "^\\s-*revno: \\(.+\\)"
   "\\(?:\n\\s-*tags: \\(.+\\)\\)?"
   "\\(?:\n\\s-*author: \\(.+\\)\\)?"
   "\n\\s-*committer: \\(.+\\)"
   "\n\\s-*branch nick: \\(.+\\)"
   "\n\\s-*timestamp: \\(.+\\)"
   "\n\\s-*message:\\([^\0]*?\\)"
   "\n\\s-*\\(removed\\|added\\|modified\\|renamed\\):")
  "Regexp for parsing the bzr log output.")

(defvar cuc-committer-names
  '(
    ;; Eric
    ("zappo" "Eric M. Ludlam <zappo@gnu.org>")
    ;; David (I)
    ("ponced" "David Ponce <david@dponce.com>")
    ("david_ponce" "David Ponce <david@dponce.com>")
    ;; Richard
    ("emacsman" "Richard Y. Kim <emacs18@gmail.com>")
    ;; Klaus
    ("berndl" "Klaus Berndl <klaus.berndl@sdm.de>")
    ;; Suraj
    ("surajacharya" "Suraj Acharya <sacharya@gmail.com>")
    ;; Marco
    ("safanaj" "Marco (Bj) Bardelli <safanaj@users.sourceforge.net>")
    ;; Anton
    ("kpoxman" "Anton V. Belyaev <kpoxman@users.sourceforge.net>")
    ;; Dan
    ("airboss" "Dan Debertin <airboss@users.sourceforge.net>")
    ;; Jan
    ("scymtym" "Jan Moringen <scymtym@users.sourceforge.net>")
    ;; David (II)
    ("davenar" "David Engster <dengste@eml.cc>")
    ;; Alex
    ("ottalex" "Alex Ott <alexott@gmail.com>")
    ;; Joakim
    ("joakimv" "Joakim Verona <joakim@verona.se>")
    ;; Lluís
    ("xscript" "Lluís <xscript@users.sourceforge.net>"))
  "Sourceforge names of committers from the older CVS imports.
Newer bzr commits should have proper names in them.")

(defvar cuc-entries nil)

(defvar cuc-dirs
  (let ((pack cedet-packages)
	(dirs nil)
	loc)
    (while pack
      (setq loc (locate-library
		 (symbol-name (car (car pack)))))
      (when loc
	(setq dirs
	      (cons (file-name-directory loc) dirs)))
      (setq pack (cdr pack)))
    (let* ((base (file-name-directory (car dirs)))
	   (root (file-name-directory (directory-file-name base)))
	   )
      (setq dirs (cons (expand-file-name "tests/" root) dirs)))
    (nreverse dirs))
  "List of directories we need to change the ChangeLog in.")

(defun cuc-update-changelog (dir &optional start-revision)
  "Update the changelog in DIR."
  (interactive "DDir: \nP")
  (find-file (concat dir "ChangeLog"))
  (erase-buffer)
  (goto-char (point-min))
  (sit-for 0)
  (message "Calling bzr log on %s..."
	   (file-name-nondirectory (directory-file-name dir)))
  (if (numberp start-revision)
      (call-process "bzr" nil (current-buffer) t
		    "log" "-n0" "-v" (expand-file-name (directory-file-name dir))
		    "-r" (format "%d.." start-revision))
    (call-process "bzr" nil (current-buffer) t
		  "log" "-n0" "-v" (expand-file-name (directory-file-name dir))))
  ;; Symmetry makes things easier.
  (goto-char (point-max))
  (insert "------------------------------------------------------------")
  ;; Generate ChangeLog.
  (cuc-convert-to-changelog)
  (save-buffer))

(defun cuc-convert-to-changelog ()
  "Convert bzr log output to ChangeLog format."
  (interactive)
  (goto-char (point-min))
  (setq cuc-entries nil)
  (while (progn
	   (forward-line 1)
	   (looking-at "^\\s-*revno: "))
    (if (null
	 (re-search-forward cuc-bzr-log-regexp
			    (save-excursion
			      (re-search-forward "^\\s-*------------------------------" nil t)
			      (point)) t))
	(error "Could not correctly parse this log entry")
      (let* ((revision (match-string-no-properties 1))
	     (tags (match-string-no-properties 2))
	     (author (or (match-string-no-properties 3) (match-string-no-properties 4)))
	     (branch (match-string-no-properties 5))
	     (timestamp (match-string-no-properties 6))
	     (message (match-string-no-properties 7))
	     (ismerge (string-match "\\[merge\\]" revision))
	     files type added removed renamed modified tmp)
      ;; Parse modified/added/removed files
      (beginning-of-line)
      (while (looking-at "^\\s-*\\(added\\|removed\\|modified\\|renamed\\):$")
	(forward-line 1)
	(let ((type (intern (match-string-no-properties 1))))
	  (while (looking-at "^\\s-+\\([^- ].+\\)$")
	    (let ((tmp (match-string-no-properties 1)))
	      (unless (string-match "/$" tmp)
		(set type (cons (match-string-no-properties 1) (symbol-value type)))))
	    (forward-line 1))))
      (push (list timestamp ismerge author modified added removed renamed message)
	    cuc-entries))))
  (erase-buffer)
  (change-log-mode)
  (cuc-generate-changelog))

(defun cuc-generate-changelog ()
  "Generate ChangeLog from `cuc-entries'."
  (let (cur (last '("" . "")))
    ;; Sort according to timestamp.
    (setq cuc-entries
	  (sort cuc-entries
		(lambda (x y)
		  (> (float-time (date-to-time (car x)))
		     (float-time (date-to-time (car y)))))))
    ;; Insert entries.
    (while (setq cur (pop cuc-entries))
      (setq last (cuc-insert-changelog-entry cur (car last) (cdr last)))
      (sit-for 0)))
  ;; Make things prettier.
  (delete-trailing-whitespace)
  (fill-region (point-min) (point-max) t))

(defun cuc-insert-changelog-entry (entry lasttime lastauthor)
  "Insert one ChangeLog entry from ENTRY.
Don't insert the TIME/AUTHOR combo if it matches the LASTTIME and LASTAUTHOR.
Return Time String & Author."
  (let ((time (car entry))
	(author (nth 2 entry))
	(message (nth 7 entry))
	(ismerge (nth 1 entry))
	(modded (nth 3 entry))
	(added (nth 4 entry))
	(removed (nth 5 entry))
	(renamed (nth 6 entry))
	timestr
	name)
    ;; Ommit 'merge from trunk' messages from feature branches.
    (unless (and ismerge
		 (string-match "[Mm]erge from trunk" message))
      ;; Fix old CVS author names.
      (string-match "\\(.+\\) <" author)
      (setq author (or (cadr (assoc (match-string 1 author)
				    cuc-committer-names))
		       author))
      (setq timestr (nth 1 (split-string time)))
      (when (not (and (string= timestr lasttime)
		      (string= author lastauthor)))
	(when (not (bobp)) (insert "\n"))
	(insert timestr "  " author "\n"))
      (if ismerge
	  ;; This is a regular merge commit.
	  (insert "\n\t[Branch merge]\n" message "\n\n")
	;; This is a regular commit
	;; Let's try to see if the committer already provided file information.
	(if (string-match "^\\s-*\\* [a-zA-Z]+" message)
	    (insert message "\n")
	  ;; If not, add it from the bzr log.
	  (setq message (progn (string-match "^[ \n\t]*\\([^\0]*\\)" message)
			       (match-string-no-properties 1 message)))
	  ;; Check if committer provided an initial summary line.
	  (when (string-match "^\\([^(][A-Z].+\\)\n\\s-*\n\\s-*(" message)
	    (insert "\n\t" (match-string 1 message) "\n")
	    (setq message (substring message (1- (match-end 0)))))
	  (insert
	   (concat
	    (when removed
	      (mapconcat (lambda (x) (concat "\n\t* " x ": Removed.")) removed ""))
	    (when added
	      (mapconcat (lambda (x) (concat "\n\t* " x ": New file.")) added ""))
	    (when renamed
	      (mapconcat (lambda (x) (concat "\n\t* " x ": Renamed.")) renamed ""))
	    (when modded
	      (mapconcat (lambda (x) (concat "\n\t* " x ":")) modded ""))))
	  (when (string-match "^(" message)
	    (backward-delete-char 1))
	  (insert " " message "\n")))

      ;; Return the timestr and author
      (cons timestr author)
      )))

(defun cuc-update-all-changelogs ()
  "Update all ChangeLogs for CEDET."
  (interactive)
  (let ((d cuc-dirs))
    (while d
      (cuc-update-changelog (car d))
      (setq d (cdr d)))))

(provide 'cedet-update-changelog)

;;; cedet-update-changelog.el ends here
