;;; semantic/ectags/util.el --- Utilities for Exuberant CTags and Semantic

;; Copyright (C) 2008, 2009, 2010 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Utilities for working the Exuberant CTags integration with Semantic.
;;
;; * Configuration for ectags.
;; * Running ectags.
;; * Decoding ectags output.

(eval-when-compile (require 'inversion))

;;; Code:

(defcustom semantic-ectags-program-list '("ctags-exuberant"
					 "ectags"
					 "ctags")
  "*List of possible exuberant ctags programs that coule be installed."
  :group 'semantic
  :type '(repeat file))

(defcustom semantic-ectags-program nil
  "The Exuberant CTags program to use."
  :group 'semantic
  :type 'program)

(defun semantic-ectags-program ()
  "Return our best guess at an exuberant ctags program."
  (or semantic-ectags-program
      (let ((pl semantic-ectags-program-list))
	(while (and pl (not semantic-ectags-program))
	  (condition-case nil
	      (progn
		(call-process (car pl) nil nil nil "--version")
		(setq semantic-ectags-program (car pl)))
	    (error nil))
	  (setq pl (cdr pl)))
	(or semantic-ectags-program "ctags"))))

;;; RUN CTAGS
;;
(defun semantic-ectags-run (&rest args)
  "Run Exuberant CTags, and return a buffer with the output.
ARGS are the arguments to pass to Exuberant CTags.
The returned buffer will be recycled in future calls to this function."
  (let ((b (get-buffer-create " *Semantic ECTags*"))
	(dd default-directory))
    (with-current-buffer b
      (erase-buffer)
      (setq default-directory dd)
      (condition-case nil
	  (progn
	    (apply 'call-process (semantic-ectags-program) nil b nil
		   args)
	    b)
	(error nil)))
    ))

;;; Semi-automatic linguistic configuration
;;
;; Ask ectags what languages it supports, and what kinds there are.
(defun semantic-ectags-lang-and-kinds ()
  "Get all the language and kinds supported by ectags."
  (interactive)
  (let* ((b (semantic-ectags-run "--list-kinds=all"))
	 (lang nil)
	 (kinds nil))
    (with-current-buffer b
      (goto-char (point-min))
      (while (not (eobp))
	(setq lang (buffer-substring (point) (point-at-eol)))
	(end-of-line)
	(forward-char 1)
	(setq kinds "")
	(while (looking-at "\\s-+")
	  (let* ((split (split-string (buffer-substring (point) (point-at-eol))
				     "  " t))
		 (letter (car split))
		 (word (car (cdr split))))
	    (when (member word
			  '("function definitions"
			    "functions"
			    "variables"
			    "variable definitions"
			    "type"
			    "types"
			    "classes"
			    "namespaces"))
	      (setq kinds (concat kinds letter))))
	  (end-of-line)
	  (forward-char 1))

	;; This is where we should auto-configure, but i'm not
	;; too happy with the mechanism yet.  Just dump messages
	;; for now.
	(message "Lang %s kinds= %s"
		 (downcase lang)
		 kinds)
	)
      )
    (switch-to-buffer-other-window b)
    (goto-char (point-min))
    ))

;;; Revision Test
;;
;; Make sure we have an up to date version of ectags.

(defun semantic-ectags-version ()
  "Get the revision number of ectags."
  (interactive)
  (let* ((b (semantic-ectags-run "--version"))
	 str ropt)
    (if (not b)
	(progn
	  (message "Could not find program %s"
		   semantic-ectags-program)
	  nil)
      (setq str (with-current-buffer b
		  (goto-char (point-min))
		  (if (re-search-forward "Exuberant Ctags \\(?:\\([0-9.]+\\)\\(~svn[0-9]+\\)?\\|Development\\)," nil t)
		      (match-string 1)
		    nil)
		  )
	    ropt (with-current-buffer b
		   (goto-char (point-min))
		   (if (re-search-forward "\\+regex\\>" nil t)
		       t
		     nil)))
      (if (not str)
	  (let ((whatver
		 (with-current-buffer b
		   (goto-char (point-min))
		   (cond ((looking-at "ctags (?GNU Emacs")
			  "ectags that comes with Emacs")
			 (t
			  "unknown ectags version"))
		   )))
	    (message "Exuberant CTags not found.  Found %s" whatver)
	    nil)
	(when (called-interactively-p 'interactive)
	  (message "Detected Exuberant CTags version : %s %s"
		   str
		   (if ropt
		       "with regex support"
		     "WITHOUT regex support")
		   ))
	(list str ropt) ))))

(defvar semantic-ectags-min-version "5.7"
  "Minimum version of Exuberant CTags we need.")

(defun semantic-ectags-test-version ()
  "Make sure the version of ectags we have is up to date."
  (let* ((vi (semantic-ectags-version))
	 (v (car vi))
	 (r (car (cdr vi))))
    (require 'inversion)
    (when (not v)
      (error "Exuberant CTags not found.  Use M-x semantic-ectags-version RET"))
    (when (and (inversion-check-version v nil semantic-ectags-min-version) (not (string= v "Development")))
      (error "Version of CTags is %s.  Need at least %s"
	     v semantic-ectags-min-version))
    (when (not r)
      (error "CTags was not compiled with +regex support"))
    t))

;;;###autoload
(defun cedet-ectag-version-check (&optional noerror)
  "Check the version of the installed ctags command.
If optional programatic argument NOERROR is non-nil, then
instead of throwing an error if ctags isn't available, then
return nil."
  (interactive)
  (let ((res (if noerror
		 (condition-case nil
		     (semantic-ectags-test-version)
		   (error nil))
	       (semantic-ectags-test-version))))
    (when (and res (called-interactively-p 'interactive))
      (message "Exuberent CTags %s  - Good enough for CEDET." (car (semantic-ectags-version))))
    res))

(provide 'semantic/ectags/util)

;; Local variables:
;; generated-autoload-file: "../loaddefs.el"
;; generated-autoload-load-name: "semantic/ectags/util"
;; End:

;;; semantic/ectags/util.el ends here
