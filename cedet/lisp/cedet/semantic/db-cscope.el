;;; semantic/db-cscope.el --- Use CSCOPE databases w/ Semantic

;; Copyright (C) 2007, 2008, 2009, 2010, 2011 Eric M. Ludlam

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
;; cscope is a powerful way to analyze C code and create databases of
;; symbols.  The semantic-symref API provides a simple interface that
;; allows this database to use cscope in semantic to lookup tags.
;;
(require 'semantic/symref/cscope)
(require 'semantic/db)
(require 'data-debug)

(eval-when-compile
  ;; For generic function searching.
  (require 'eieio)
  (require 'eieio-opt)
  )

;;; Code:
;;;###autoload
(defun semanticdb-enable-cscope-databases (&optional noerror)
  "Enable the use of the CScope back end for all files in C/C++.
This will add an instance of a CScope database to each buffer in a
CScope supported hierarchy.

Two sanity checks are performed to assure (a) that cscope program exists
and (b) that the cscope program version is compatibility with the database
version.  If optional NOERROR is nil, then an error may be signalled on version
mismatch.  If NOERROR is not nil, then no error will be signlled.  Instead
return value will indicate success or failure with non-nil or nil respective
values."
  ;; First, make sure the version is ok.
  (if (not (cedet-cscope-version-check noerror))
      nil
    (dolist (mode '(c-mode c++-mode))
      (let ((ih (mode-local-value mode 'semantic-init-mode-hook)))
        (eval `(setq-mode-local
                ,mode semantic-init-mode-hook
                (cons 'semanticdb-enable-cscope-hook ih)))))
    t))

(defun semanticdb-enable-cscope-hook ()
  "Add support for CScope in the current buffer via `semantic-init-hook'."
  (semanticdb-enable-cscope-in-buffer t))

(defclass semanticdb-project-database-cscope
  ;; @todo - convert to one DB per directory.
  (semanticdb-project-database eieio-instance-tracker)
  ()
  "Database representing a CScope tags file.")

(defun semanticdb-enable-cscope-in-buffer (&optional dont-err-if-not-available)
  "Enable a CScope database in the current buffer.
When CScope is not available for this directory, display a message
if optional DONT-ERR-IF-NOT-AVAILABLE is non-nil; else throw an error."
  (interactive "P")
  (if (cedet-cscope-support-for-directory (semantic-symref-calculate-rootdir))
      (setq
       ;; Add to the system database list.
       semanticdb-project-system-databases
       (cons (semanticdb-project-database-cscope "CScope")
	     semanticdb-project-system-databases)
       ;; Apply the throttle.
       semanticdb-find-default-throttle
       (append semanticdb-find-default-throttle
	       '(omniscience))
       )
    (if dont-err-if-not-available
	nil; (message "No CScope support in %s" default-directory)
      (error "No CScope support in %s" default-directory))
    ))

;;; Classes:
(defclass semanticdb-table-cscope (semanticdb-search-results-table)
  ((major-mode :initform nil)
   )
  "A table for returning search results from CScope.")

(defmethod semanticdb-equivalent-mode ((table semanticdb-table-cscope) &optional buffer)
  "Return t, pretend that this table's mode is equivalent to BUFFER.
Equivalent modes are specified by the `semantic-equivalent-major-modes'
local variable."
  ;; @todo - hack alert!
  t)

(defmethod object-print ((obj semanticdb-table-cscope) &rest strings)
  "Pretty printer extension for `semanticdb-table-cscope'.
Adds the number of tags in this file to the object print name."
  (apply 'call-next-method obj (cons " (proxy)" strings)))

;;; Filename based methods
;;
(defmethod semanticdb-get-database-tables ((obj semanticdb-project-database-cscope))
  "For a cscope database, there are no explicit tables.
For each file hit, get the traditional semantic table from that file."
  ;; We need to return something since there is always the "master table"
  ;; The table can then answer file name type questions.
  (when (not (slot-boundp obj 'tables))
    (let ((newtable (semanticdb-table-cscope "Cscope Search Table")))
      (oset obj tables (list newtable))
      (oset newtable parent-db obj)
      (oset newtable tags nil)
      ))

  (call-next-method))

(defmethod semanticdb-file-table ((obj semanticdb-project-database-cscope) filename)
  "From OBJ, return FILENAME's associated table object."
  ;; We pass in "don't load".  I wonder if we need to avoid that or not?
  (car (semanticdb-get-database-tables obj))
  )

;;; Search Overrides
;;
;; Only NAME based searches work with CSCOPE as that is all it tracks.
;;
(defmethod semanticdb-find-tags-by-name-method
  ((table semanticdb-table-cscope) name &optional tags)
  "Find all tags named NAME in TABLE.
Return a list of tags."
  (if tags
      ;; If TAGS are passed in, then we don't need to do work here.
      (call-next-method)
    ;; Call out to Cscope for some results.
    (let* ((semantic-symref-tool 'cscope)
	   (result (semantic-symref-find-tags-by-name name 'project))
	   )
      (when result
	;; We could ask to keep the buffer open, but that annoys
	;; people.
	(semantic-symref-result-get-tags result))
      )))

(defmethod semanticdb-find-tags-by-name-regexp-method
  ((table semanticdb-table-cscope) regex &optional tags)
  "Find all tags with name matching REGEX in TABLE.
Optional argument TAGS is a list of tags to search.
Return a list of tags."
  (if tags (call-next-method)
    (let* ((semantic-symref-tool 'cscope)
	   (result (semantic-symref-find-tags-by-regexp regex 'project))
	   )
      (when result
	(semantic-symref-result-get-tags result))
      )))

(defmethod semanticdb-find-tags-for-completion-method
  ((table semanticdb-table-cscope) prefix &optional tags)
  "In TABLE, find all occurrences of tags matching PREFIX.
Optional argument TAGS is a list of tags to search.
Returns a table of all matching tags."
  (if tags (call-next-method)
    (let* ((semantic-symref-tool 'cscope)
	   (result (semantic-symref-find-tags-by-completion prefix 'project))
	   (faketags nil)
	   )
      (when result
	(dolist (T (oref result :hit-text))
	  ;; We should look up each tag one at a time, but I'm lazy!
	  ;; Doing this may be good enough.
	  (setq faketags (cons
			  (semantic-tag T 'function :faux t)
			  faketags))
	  )
	faketags))))

;;; Deep Searches
;;
;; If your language does not have a `deep' concept, these can be left
;; alone, otherwise replace with implementations similar to those
;; above.
;;
(defmethod semanticdb-deep-find-tags-by-name-method
  ((table semanticdb-table-cscope) name &optional tags)
  "Find all tags name NAME in TABLE.
Optional argument TAGS is a list of tags to search.
Like `semanticdb-find-tags-by-name-method' for cscope."
  (semanticdb-find-tags-by-name-method table name tags))

(defmethod semanticdb-deep-find-tags-by-name-regexp-method
  ((table semanticdb-table-cscope) regex &optional tags)
  "Find all tags with name matching REGEX in TABLE.
Optional argument TAGS is a list of tags to search.
Like `semanticdb-find-tags-by-name-method' for cscope."
  (semanticdb-find-tags-by-name-regexp-method table regex tags))

(defmethod semanticdb-deep-find-tags-for-completion-method
  ((table semanticdb-table-cscope) prefix &optional tags)
  "In TABLE, find all occurrences of tags matching PREFIX.
Optional argument TAGS is a list of tags to search.
Like `semanticdb-find-tags-for-completion-method' for cscope."
  (semanticdb-find-tags-for-completion-method table prefix tags))

;;; TEST
;;
;; Here is a testing fcn to try out searches via the Cscope database.
(defvar semanticdb-test-cscope-startfile "~/src/cscope-15.7/src/main.c"
  "File to use for testing.")

(defun semanticdb-test-cscope (searchfor &optional standardfile)
  "Test the CScope semanticdb.
Argument SEARCHFOR is the text to search for.
If optional arg STANDARDFILE is non nil, use a standard file w/ cscope enabled."
  (interactive "sSearch For Tag: \nP")

  (save-excursion
    (when standardfile
      (save-match-data
	(set-buffer (find-file-noselect semanticdb-test-cscope-startfile))))

    (condition-case err
	(semanticdb-enable-cscope-in-buffer)
      (error (if standardfile
		 (error err)
	       (save-match-data
		 (set-buffer (find-file-noselect semanticdb-test-cscope-startfile)))
	       (semanticdb-enable-cscope-in-buffer))))

    (let* ((db (semanticdb-project-database-cscope "cscope"))
	   (tab (semanticdb-file-table db (buffer-file-name)))
	   (result (semanticdb-deep-find-tags-for-completion-method tab searchfor))
	   )
      (data-debug-new-buffer "*SemanticDB Gnu Cscope Result*")
      (data-debug-insert-thing result "?" "")
      )))

(provide 'semantic/db-cscope)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "semantic/db-cscope"
;; End:

;;; semantic/db-cscope.el ends here
