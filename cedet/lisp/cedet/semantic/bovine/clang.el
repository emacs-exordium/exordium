;;; semantic/bovine/clang.el --- Use 'clang' to provide completions for C/C++

;; Copyright (C) 2011, 2012, 2013 David Engster

;; Author: David Engster <deng@randomsample.de>

;; This file is NOT part of GNU Emacs.

;; !! IMPORTANT: DO NOT MERGE THIS FILE TO EMACS !!

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

;; This code overrides semantic-analyze-possible-completions for
;; c-mode to provide smart completions by calling 'clang'.  For
;; information on clang see
;;
;;  http://clang.llvm.org/
;;
;; This needs at least clang v2.9 or newer.

;;; Usage:

;; With CEDET properly installed it should be enough to do
;;
;;   M-x semantic-clang-activate
;;
;; Use M-x semantic-clang-deactivate to restore the old default
;; behavior.
;;
;; This generates a temporary file on which clang can do its work. It
;; should get automatically deleted, but in case of an error it might
;; still be there after exiting Emacs.

;;; Code:

(require 'inversion)
(require 'ede)
(require 'ede/proj)
(require 'ede/cpp-root)
(require 'ede/linux)
(require 'semantic/analyze)
(require 'semantic/analyze/complete)

(eval-when-compile
  (require 'mode-local))

(defvar semantic-clang-binary "/usr/bin/clang"
  "Binary for clang.")

(defvar semantic-clang-arguments '("")
  "Additional arguments for clang.")

(defvar semantic-clang-temp-filename "__SEMANTIC_CLANG_TEMPFILE"
  "Filename used for temporary file.")

(defvar semantic-clang-show-errors t
  "Display errors issued by clang.")

(defvar semantic-clang-system-includes nil)

;;;###autoload
(defun semantic-clang-activate ()
  "Activate clang completions for C/C++."
  (interactive)

  (if (eq
       (inversion-check-version (semantic-clang-version-string) nil '(full 2 9))
       'outdated)
      (message "Need clang version 2.9 or newer"))

  ;; Get system includes
  (setq semantic-clang-system-includes (mapcar (lambda (x) (concat "-I" x))
					       (semantic-clang-get-system-includes)))

  ;; Install the override
  (define-mode-local-override semantic-analyze-possible-completions
    c-mode (context &optional flags)
"Produce smart completions using clang.
Argument CONTEXT is an object specifying the locally derived context.
The optional argument FLAGS changes which return options are returned.
FLAGS can be any number of:
  'no-tc     - do not apply data-type constraint.
  'no-unique - do not apply unique by name filtering."
    (semantic-clang-possible-completions context flags))

  (message "Activated clang completions for C/C++."))

(defun semantic-clang-deactivate ()
  "Deactivate clang completions."
  (interactive)
  ;;This is a hack. How can we cleanly remove mode-local overrides?
  (define-mode-local-override semantic-analyze-possible-completions
    c-mode (context &optional flags)
    "Call default method for producing smart completions.
Argument CONTEXT is an object specifying the locally derived context.
The optional argument FLAGS changes which return options are returned.
FLAGS can be any number of:
  'no-tc     - do not apply data-type constraint.
  'no-unique - do not apply unique by name filtering."
    (semantic-analyze-possible-completions-default context flags))
  (message "Deactivated clang completions for C/C++ and installed old default behavior."))

(defsubst semantic-clang-completion-regexp (completetext)
  "Regular expression to find completions for COMPLETEXT in clang output.
First group is the completion, second group the definition."
  (concat "^COMPLETION: \\(" completetext ".*?\\) : \\(.+\\)"))

(defun semantic-clang-possible-completions (context &optional flags)
  "Implementation for `semantic-analyze-possible-completions'."
  (let* ((a context)
	 (desired-type (semantic-analyze-type-constraint a))
	 (desired-class (oref a prefixclass))
	 (bounds (oref a bounds))
	 (prefix (oref a prefix))
	 (ctext (car (last prefix)))
	 (txt (concat
	       "/* This is a temporary file created by semantic-clang and can be removed. */\n"
	       (buffer-substring-no-properties (point-min) (point-max))))
	 (fext (file-name-extension (buffer-file-name)))
	 (dir (file-name-directory (buffer-file-name)))
	 (tempfile (concat dir semantic-clang-temp-filename "." fext))
	 (proj-args (semantic-clang-args-from-project))
	 complete-pos results)
    (save-excursion
      (when (consp bounds)
	(goto-char (car bounds)))
      (setq complete-pos
	    (concat  ":" (number-to-string
			  (1+ (count-lines (point-min) (point))))
		     ":" (number-to-string (1+ (current-column))))))
    (if (zerop (length fext))
	(message "Buffer's file name doesn't have a proper extension; cannot call clang.")
      (write-region txt nil tempfile nil 'nodisplay)
      (with-temp-buffer
	(let ((exitcode
	       (apply 'call-process
		      semantic-clang-binary nil t nil
		      "-cc1" "-fsyntax-only" "-code-completion-at"
		      (concat tempfile complete-pos)
		      tempfile
		      (append semantic-clang-system-includes
			      proj-args
			      semantic-clang-arguments))))
	  (when (and (> exitcode 0)
		     semantic-clang-show-errors)
	    (goto-char (point-min))
	    (while (re-search-forward "fatal error: \\(.+\\)$" nil t)
	      (message "Clang error: %s" (match-string 1)))))
	(goto-char (point-min))
	(while (re-search-forward
		(semantic-clang-completion-regexp ctext) nil t)
	  (push
	   (semantic-clang-generate-tag (match-string 1) (match-string 2))
	   results)))
      (delete-file tempfile))
    (semantic-clang-filter-results results desired-type desired-class prefix flags)))

;; Typical outputs from clang:
;;
;; Variable:  priv_a : [#int#]priv_a
;; Function:  method : [#void#]method(<#int a#>)
;; Type:      foo : foo

(defun semantic-clang-identify (str)
  "Identify output STR from clang.
Will return a list (class [type [args]]) with 
class: either 'function, 'variable or 'type
type: (Return)-type of function/variable
args: Function arguments"
  (cond
   ((string-match "^\\[#\\(.+?\\)#\\].+(\\(.*\\))$" str)
    (list 'function (match-string 1 str)
	  (let ((args (match-string 2 str))
		(st 0)
		res)
	    (while (string-match "<#\\(.+?\\)#>" args st)
	      (push (match-string 1 args) res)
	      (setq st (match-end 0)))
	    (nreverse res))))
   ((string-match "^\\[#\\(.+?\\)#\\].+$" str)
    (list 'variable (match-string 1 str)))
   (t
    (list 'type))))

(defun semantic-clang-generate-tag (str1 str2)
  "Generate tag for clang output STR1 : STR2."
  (let ((id (semantic-clang-identify str2)))
    (cond
     ((eq (car id) 'function)
      (semantic-tag-new-function str1 (cadr id) (car (last id))))
     ((eq (car id) 'variable)
      (semantic-tag-new-variable str1 (cadr id)))
     ((eq (car id) 'type)
      (semantic-tag-new-type str1 str1 nil nil))
     (t
      (error "Unknown output from clang."))
    )
  ))

(defun semantic-clang-filter-results (results desired-type desired-class prefix
					      &optional flags)
  "Filter tags in RESULTS from clang output.
Tags are filtered against DESIRED-TYPE and DESIRED-CLASS, given
this is not disabled in FLAGS (see doc-string for
`semantic-analyze-possible-completions'.  Also removed will be
con/destructors (according to PREFIX) and operators."
  (delq nil
	(mapcar
	 (lambda (tag)
	   (let ((type-ok (member 'no-tc flags))
		 (tagtype (semantic-tag-type tag))
		 (tagname (semantic-tag-name tag)))
	     (unless type-ok
	       (when (and
		      (or (null desired-type)
			  (string= (semantic-tag-name desired-type)
				   (semantic-tag-type tag)))
		      (or (null desired-class)
			  (member (semantic-tag-class tag) desired-class)))
		 (setq type-ok t)))
	     (when (and type-ok
			;; filter out destructor
			(not (string-match "^~" (semantic-tag-name tag)))
			;; filter out operators
			(not (string-match "^operator[=[(+-%/!<>&|^]+"
					   (semantic-tag-name tag)))
			(or (null tagtype)
			    (<= (length prefix) 1)
			    (not (semantic-tag-p (car prefix)))
			    (not (string= (semantic-clang-get-typename-string (car prefix))
					  tagname))))
	       tag)))
	 results)))

(defun semantic-clang-get-typename-string (tag)
  "Get typename from TAG as a string."
  (let ((tagtype (semantic-tag-type tag)))
    (cond
     ((stringp tagtype)
      tagtype)
     ((semantic-tag-p tagtype)
      (semantic-tag-name tagtype))
     (t
      ""))))

(defun semantic-clang-version-string ()
  "Return version string from clang binary."
  (with-temp-buffer
    (when (not (zerop
		(call-process semantic-clang-binary nil t nil "--version")))
      (error "Could not call clang --version"))
    (goto-char (point-min))
    (when (null (or (re-search-forward "clang version \\([0-9.]+\\)" nil t)
		    (re-search-forward "based on LLVM \\([0-9.]+\\)" nil t)))
      (error "Could not parse clang version string"))
    (match-string 1)))

(defun semantic-clang-get-system-includes ()
  "Return list of system includes from clang compiler.
Those have to be manually added when just calling the frontent
via '-cc1', otherwise it won't find system headers."
  ;; We have to compile a test file for this
  (let (result)
    (with-temp-buffer
      (insert "int foo=0;\n")
      (call-process-region (point-min) (point-max)
			   semantic-clang-binary t t nil
			   "-###" "-x" "c++" "-c" "-o" null-device "-")
      (goto-char (point-min))
      (while (search-forward "\"" nil t)
	(when (looking-at "-internal-\\(externc-\\)?isystem\" \"")
	  (goto-char (match-end 0))
	  (push (buffer-substring (point)
				  (1- (progn (search-forward "\"") (point))))
		result))))
    result))

(defun semantic-clang-args-from-project ()
  "Return list of additional arguments for the compiler from the project.
If the current buffer is part of an EDE project, return a list of
additional arguments for the compiler; currently, this deals with
include directories (-I) and preprocessor symbols (-D)."
  (let ((proj ede-object-root-project)
	(tarproj ede-object))
    (when proj
      (cond
       ;; For ede-cpp-root-project it's easy
       ((ede-cpp-root-project-child-p proj)
	(append
	 (mapcar (lambda (inc) (concat "-I" inc))
		 (append (mapcar (lambda (x) (concat (ede-project-root-directory proj) x))
				 (oref proj include-path))
			 (oref proj system-include-path)))
	 (mapcar (lambda (spp) (concat "-D" (car spp)
				       (when (cdr spp)
					 (concat "=" (cadr spp)))))
		 (oref proj spp-table))
	 (list (concat "-I" (ede-project-root-directory proj)))))
       ;; Similarly for ede-linux-project
       ((ede-linux-project-child-p proj)
	(let* ((root (ede-project-root-directory proj))
	       (dir (file-name-directory (buffer-file-name)))
	       (rel-dir (substring dir (length root))))
	  (append
	   (list (format "-include%s/include/linux/kconfig.h" root))
	   (mapcar (lambda (inc) (concat "-I" inc))
		   (oref proj include-path))
	   (list (concat "-I" root rel-dir)
		 (concat "-I" (oref proj build-directory) rel-dir)
		 "-D__KERNEL__"))))
       ;; For more general project types it's a bit more difficult.
       ((ede-proj-project-p proj)
	;; Get the local and configuration variables.
	(let ((vars (mapcar 'cdr (oref proj variables))))
	  (when (slot-boundp tarproj 'configuration-variables)
	    (setq vars (append vars
			       (mapcar 'cdr
				       (cdr (oref tarproj configuration-variables))))))
	  ;; Get includes and preprocessor symbols.
	  (setq vars (apply 'append (mapcar 'split-string vars)))
	  (append (list (concat "-I" (ede-project-root-directory proj)))
		  (delq nil
			(mapcar (lambda (var)
				  (when (string-match "^\\(-I\\|-D\\)" var)
				    var))
				vars)))))))))

(provide 'semantic/bovine/clang)

;; Local variables:
;; generated-autoload-file: "../loaddefs.el"
;; generated-autoload-load-name: "semantic/bovine/clang"
;; End:

;;; semantic/bovine/clang.el ends here
