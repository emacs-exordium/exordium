;;; semantic/db-java.el --- Semantic database extensions for Java

;;; Copyright (C) 2003, 2007, 2008, 2009 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;;Joakim Verona joakim@verona.se
;; Keywords: tags

;; This file is not part of GNU Emacs.

;; Semanticdb is free software; you can redistribute it and/or modify
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
;; 
;;; Commentary:
;;
;; A majority of Java's useful functionality is inside class files.
;; Often these class files do not have parsable source available.  In
;; order to get full advantage of speedbar extensions, access to
;; these class files is needed.
;;
;; The `semantic-project-database-java' class inherits from the
;; database base class.  It uses Clojure to query the class files, and
;; create token compatible with Semantic.
;;

;;see semanticdb-clojure-link-create how to start the backend.

;;; Code:

;;; Classes:
(defclass semanticdb-table-java (semanticdb-search-results-table)
  ((major-mode :initform java-mode)
   )
  "A table for returning search results from Java.")

(defclass semanticdb-project-database-java
  (semanticdb-project-database eieio-singleton)
  ((new-table-class :initform semanticdb-table-java
		    :type class
		    :documentation
		    "New tables created for this database are of this class.")
   )
  "Database representing Java pre-compiled class libraries.")

;; Create the database, and add it to searchable databases for Java mode.
(defvar-mode-local java-mode semanticdb-project-system-databases
  (list
   ;; Create a global instance for all Java source files.
   (semanticdb-project-database-java "Java"))
  "Search Java Class files for for symbols.")


;; NOTE: Be sure to modify this to the best advantage of your
;;       language.

(defvar-mode-local java-mode semanticdb-find-default-throttle 
  '(project system  omniscience)
  "Search project files, then search this omniscience database.
It is not necessary to do system or recursive searching because of
the omniscience database.")


;; ;;; just a hack...
;; (define-mode-local-override semanticdb-find-translate-path java-mode (path brutish)
;;     (message "semanticdb-find-translate-path java-mode %s " path)
;;     (semanticdb-find-translate-path-default path t); always do brutish search. its a hack.
;;  )



; try this stuff from -el implementation instead

(defmethod semanticdb-get-database-tables ((obj semanticdb-project-database-java))
  "For an Emacs Lisp database, there are no explicit tables.
Create one of our special tables that can act as an intermediary."
  ;; We need to return something since there is always the "master table"
  ;; The table can then answer file name type questions.
  (message "semanticdb-get-database-tables java: %s" obj)
  (when (not (slot-boundp obj 'tables))
    (let ((newtable (semanticdb-table-java "java")))
      (oset obj tables (list newtable))
      (oset newtable parent-db obj)
      (oset newtable tags nil)
      ))
  (call-next-method))

(defmethod semanticdb-file-table ((obj semanticdb-project-database-java) filename)
  "From OBJ, return FILENAME's associated table object.
For Emacs Lisp, creates a specialized table."
  (message "semanticdb-file-table java: %s %s" obj filename)  
  (car (semanticdb-get-database-tables obj))
  )






(defmethod semanticdb-get-tags ((table semanticdb-table-java ))
  "Return the list of tags belonging to TABLE."
  ;; specialty table ?  Probably derive tags at request time?
  (message "semanticdb-get-tags %s" table)
  )

(defmethod semanticdb-equivalent-mode ((table semanticdb-table-java) &optional buffer)
  "Return non-nil if TABLE's mode is equivalent to BUFFER.
Equivalent modes are specified by by `semantic-equivalent-major-modes'
local variable."
  (save-excursion
    (set-buffer buffer)
    (or (eq (or mode-local-active-mode major-mode) 'java-mode)
	(eq (or mode-local-active-mode major-mode) 'jde-mode))))
;    (or (eq major-mode 'java-mode)
;	(eq major-mode 'jde-mode))))

    
(defmethod semanticdb-find-tags-by-name-method
  ((table semanticdb-table-java) name  &optional tags)
  "Find all tags name NAME in TABLE.
Return a list of tags."
  (message "semanticdb-find-tags-by-name-method %s %s" name tags)
  
  (list (semantic-tag name 'type))
  )

  ; this should only return a type, and then find-external should return a list of members
  ;(semantic-find-tags-by-name name (or tags (semanticdb-get-tags table)))  )

(defmethod semanticdb-find-tags-by-name-regexp-method
  ((table semanticdb-table-java) regex  &optional tags)
  "Find all tags with name matching REGEX in TABLE.
Return a list of tags."
  (message " semanticdb-find-tags-by-name-regexp-method %s" regex)
  (semantic-find-tags-by-name-regexp regexp (or tags (semanticdb-get-tags table)))
  )

(defmethod semanticdb-find-tags-for-completion-method
  ((table semanticdb-table-java) prefix  &optional tags)
  "In TABLE, find all occurances of tags matching PREFIX.
Returns a table of all matching tags."
  (message "semanticdb-find-tags-for-completion-method %s" prefix)
  (semantic-find-tags-for-completion prefix (or tags (semanticdb-get-tags table)))
  )


;;; Deep Searches
;;
;; If your language does not have a `deep' concept, these can be left
;; alone, otherwise replace with implementations similar to those
;; above. 
;;
(defmethod semanticdb-deep-find-tags-by-name-method
  ((table semanticdb-table-java) name &optional tags)
  "Find all tags name NAME in TABLE.
Optional argument TAGS is a list of tags t
Like `semanticdb-find-tags-by-name-method' for java."
  (message "semanticdb-deep-find-tags-by-name-method %s %s" name tags)
  (semanticdb-find-tags-by-name-method table name tags))

(defmethod semanticdb-deep-find-tags-by-name-regexp-method
  ((table semanticdb-table-java) regex &optional tags)
  "Find all tags with name matching REGEX in TABLE.
Optional argument TAGS is a list of tags to search.
Like `semanticdb-find-tags-by-name-method' for java."
  (message " semanticdb-deep-find-tags-by-name-regexp-method %s %s" regex tags)
  (semanticdb-find-tags-by-name-regexp-method table regex tags))

(defmethod semanticdb-deep-find-tags-for-completion-method
  ((table semanticdb-table-java) prefix &optional tags)
  "In TABLE, find all occurances of tags matching PREFIX.
Optional argument TAGS is a list of tags to search.
Like `semanticdb-find-tags-for-completion-method' for java."
  (message " semanticdb-deep-find-tags-for-completion-method %s %s" prefix tags)
  (semanticdb-find-tags-for-completion-method table prefix tags))

;;; Advanced Searches
;;

(defmethod semanticdb-find-tags-external-children-of-type-method
  ((table semanticdb-table-java) type &optional tags)
  "Find all nonterminals which are child elements of TYPE
Optional argument TAGS is a list of tags to search.
Return a list of tags."
  (message "semanticdb-find-tags-external-children-of-type-method 2 %s %s" type tags)
  (if tags (call-next-method)
    ;; Call Clojure to ge classinfo, and build semantic tags
    (let* ((classinfo (semanticdb-clojure-get-class-info))
           (classtags (mapcar
                        (lambda (el) (semantic-tag-new-function
                                      (car el) ;method name
                                      nil;returntype; seems to confuse completion
                                      (mapcar
                                       (lambda (argtype) (semantic-tag-new-variable argtype argtype  ""))
                                       (cadr el))))
                        classinfo))
      classtags))))



;;clojure interface
(defvar semanticdb-clojure-process nil)
(defvar semanticdb-clojure-end-marker "clojure.core=> "
  "a tag that separates results from clojure")
(defvar semanticdb-clojure-buffer-name "*semantic-clojure*"
  "name of semanticdb:s clojure interaction buffer")

(defun semanticdb-clojure-filter (proc string)
  (with-current-buffer (process-buffer proc)
      (save-excursion
        ;; Insert the text, advancing the process marker.
        (goto-char (process-mark proc))
        (insert string)
        (set-marker (process-mark proc) (point)))
       (goto-char (process-mark proc))))




(defun semanticdb-clojure-link-create ()
  (unless (eq 'open (process-status semanticdb-clojure-process))
    ;;TODO start the clojure listener automatically.
    ;; for now, start it from a shell like below.

    ;;WARNING the socket is currently unauthenticted, so use only for testing ATM!

    ;;clojure # starts a clojure REPL in a shell
    ;;(use 'semanticdb-clojure) 
    ;;(use 'clojure.contrib.server-socket) 
    ;;(create-repl-server 9000) ;;create a new socket REPL on port 9000
    
    ;;the socket REPL will be separate from the stdio REPL, so "nc
    ;;localhost 9000" to talk to it
    (setq semanticdb-clojure-process (open-network-stream "semantic-clojure" semanticdb-clojure-buffer-name "localhost" 9000))
    (set-process-filter semanticdb-clojure-process 'semanticdb-clojure-filter)
  
))

(defun semanticdb-clojure-send (msg)
  "Send MSG to Clojure for evaluation, return as lisp objects."
  (semanticdb-clojure-link-create)
  (process-send-string semanticdb-clojure-process (concat msg "\n" ))
  (accept-process-output semanticdb-clojure-process 10)
  (read (semanticdb-clojure-mark-result-region))
)

(defun semanticdb-clojure-mark-result-region ()
  "Return the last value from Clojure."
  ;:now the filter is supposed to have been run
    (with-current-buffer (process-buffer semanticdb-clojure-process)
      (save-excursion
        (goto-char (point-max))
        (search-backward semanticdb-clojure-end-marker)
        (setq myend (- (point) 1))
        (search-backward semanticdb-clojure-end-marker)
        (setq mystart (+ (length semanticdb-clojure-end-marker) (point)))
        (buffer-substring mystart myend)
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,
;;call clojure backend methods
;; TODO some clever macros to build the interface methods

(defun semanticdb-clojure-add-classpath (url)
  "Add URL to the classpath of the Clojure instance.
Example url: file:///opt/mmx/jmf/lib/jmf.jar
"
  (semanticdb-clojure-send (format "(add-classpath \"%s\")" url)))

(defun semanticdb-clojure-get-class-info (classname)
  "Return a list of members of CLASSNAME.
Each entry looks like:(method-name (argument_type,...) return_type)"
  
 (semanticdb-clojure-send (format "(get-class-info \"%s\")" classname)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;test code
(defun semanticdb-java-test ()
  (semantic-find-tags-by-name
   "java.lang.String"
   (mode-local-value  'java-mode 'semanticdb-project-system-databases)))
;(semanticdb-java-test)


(provide 'semantic/db-java)
;;; semantic/db-java.el ends here

