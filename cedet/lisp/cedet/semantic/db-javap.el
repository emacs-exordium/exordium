;;; semantic/db-javap.el --- Java include path management and symbol database via javap.
;;
;; Copyright (C) 2011, 2012, 2014 Eric M. Ludlam
;;
;; Author: Eric M. Ludlam <eric@siege-engine.com>
;;
;; This file is not part of GNU Emacs.
;;
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
;; Support path translation for java.  Enhances existing features of
;; `semanticdb-find.el' to allow for java syntax of imports, such as
;; '*', and the fact that symbols can be discovered via file name,
;; where the filename matches the class name.
;;
;; Also support data-base like features via the use of the javap
;; commands.  Javap can read symbols out of a .jar or .class file into
;; a Semantic readable source syntax.  By combining the by-symbol
;; nature of files with this mechanism, minimal searching of source
;; files can be achieved, which still maintaining accuracy.
;;
;; This depends on good naming conventions for files.
;;
;; TODO - once this is implemented, extract any common "Files Are
;; Symbols" code out into a new utility.

;;; Code:

;;; COMPOUND TABLE
;;
;; The compound database table represents a series of tags (classes)
;; based on file names that can be searched, and upon finding them,
;; will extract tags from the sources associated with the tag file in
;; a new table.
;;
;; The compound table will get passed around the search routines, but
;; never return results directly from this table.

(require 'cedet-java)
(require 'ede)
(require 'semantic/find)
(require 'semantic/db)
(require 'semantic/analyze)
(require 'semantic/db-find)
(require 'semantic/db-typecache)

(eval-when-compile
  (require 'mode-local)
  (require 'eieio))

;;; CLASSPATH and PATH EXPANSION
;;
(defcustom semanticdb-javap-classpath '()
  "Classpath used by Javap when used by Semanticdb."
  :group 'semanticdb
  :type '(repeat directory))

(define-mode-local-override semanticdb-find-translate-path
  java-mode (path brutish)
  "Override the default path translator for Java.
This will execute the default implementation, but stick a table
including the current package onto the path.  See
`semanticdb-javap-dir-to-compound-table' for how this is done."
  (if (semanticdb-find-results-p path)
      ;; nil means perform the search over these results.
      nil
    (if brutish
	(semanticdb-find-translate-path-brutish-default path)
      ;; Of the includes case, add our special table.
      (let ((defaultpath (semanticdb-find-translate-path-includes-default path))
	    (javapath (semanticdb-javap-dir-to-compound-table default-directory))
	    ;; Any other packages w/ this name known in classpath or to EDE?
	    (similarpath (semanticdb-javap-paths-for-package (current-buffer)))
            ;; Look for always accessible package - java.lang.*
            (langpath (semanticdb-javap-paths-for-java-lang)))
	;; If this is a dup, remove.
	(setq similarpath (remq javapath similarpath))
	
	(append (list langpath) defaultpath (list javapath) similarpath)
	))))

;;; ANALYZER HACKS
;;
;; The analyze defaults to C/C++, but java is a bit simpler.
;; If the standard sequence finder gets lost, we can run our backup
;; which uses our typecache to find the beginning of a sequence via
;; above tricks for only the starting subset.  This will allow us to
;; perform completions in package names.

(define-mode-local-override semantic-analyze-find-tag-sequence
  java-mode (sequence &optional scope typereturn throwsym flags)
  "For Java buffers, use our javap typecache as a backup search method.
If the default returns only strings, search for the first part of sequence
in the typecache.  Create a return list from that, and append the last
string from sequence to the found tag in the typecache.
SCOPE, TYPERETURN, and THROWSYM are all passed to the default method, but
not used locally."
  (let* ((lastpart (car (last sequence)))
	 (ans (semantic-analyze-find-tag-sequence-default
	       sequence scope typereturn throwsym flags)))
    ;; If the car of ans is a STRING, then lets try our hack.
    (when (and (> (length ans) 1) (stringp (car ans)))
      (setq ans (append
		 (semanticdb-typecache-find (nreverse (cdr (reverse sequence))))
		 (list lastpart))))
    ;; If we have only one answer, but the name doesn't match the last string in sequence
    ;; then we need to perform a little trickery to fix up the problem.
    (when (and (= (length ans) 1) (semantic-tag-p (car ans)) (stringp lastpart)
	       (not (string= (semantic-tag-name (car ans)) lastpart)))
      (setq ans (append ans (list lastpart))))

    ;; Make sure typereturn has the right data in it.
    ;; THIS IS A QUICK HACK
    ;; Lets look for cases where this fails.
    (when (and typereturn (not (symbol-value typereturn)))
      (set typereturn (nreverse (cdr (reverse ans)))))

    ;; Return whatever we have left.
    ans))

;;; DIRECTORY DB TABLE
;;
;; Here is a database table for searching a directory where any .java file
;; is also a class tag.
(defvar semanticdb-javap-directory-tracker nil
  "Tracker for directory tables.")

(defclass semanticdb-table-java-directory (semanticdb-abstract-table
					   eieio-instance-tracker)
  ((tracking-symbol :initform 'semanticdb-javap-directory-tracker)
   (major-mode :initform 'java-mode)
   (directory :initarg :directory
	      :type string
	      :documentation
	      "The directory this table is trying to represent.")
   (proxy :initarg :proxy
	  :type symbol
	  :documentation
	  "The proxy symbol used when creating proxy tags based on files.
See `semantic-create-proxy-tag' for details.")
   )
  "Table which represents the classes found in files in series of diretories.")

(defmethod semanticdb-table-java-package ((table semanticdb-table-java-directory))
  "Get the package name to use for this database as a directory."
  (oref table directory))

(defmethod initialize-instance :AFTER ((table semanticdb-table-java-directory) &rest args)
  "After TABLE is initialized, make sure basic features are set."
  (oset table proxy (semantic-create-tag-proxy 'semanticdb-javap-resolve-proxy table))
  )

(defmethod semanticdb-refresh-table ((obj semanticdb-table-java-directory) &optional force)
  "Java Directories should be refreshed when files in the directory hange.
No nothing for now."
  nil)

(defmethod semanticdb-needs-refresh-p ((obj semanticdb-table-java-directory))
  "Return nil, we don't need a refresh until we figure out directory tracking."
  nil)

(defmethod semanticdb-equivalent-mode ((table semanticdb-table-java-directory) &optional buffer)
  "Return non-nil if TABLE's mode is equivalent to BUFFER.
Equivalent modes are specified by the `semantic-equivalent-major-modes'
local variable."
  t
  )

;;; Tag Normailzation
;;
;; The tags produced are faux tags that have a file, but no details.  They
;; need to be normalized, or converted into a full formed tag by reading in
;; the file they are originated from.
(defmethod semanticdb-normalize-tags ((obj semanticdb-table-java-directory) tags)
  "Convert tags found by our java directory table into a complete tag.
The default tag just has a name, type, and the filename.  Normalize by
loading in the file it belongs to, and looking up that symbol in the file
and returning that tag instead."
  ;; This was copied from semanticdb-ebrowse, and modified for this use.
  (let ((tagret nil)
	)
    (dolist (T tags)

      (let* ((tfn (semantic-tag-file-name T))
	     (realtable (semanticdb-file-table-object
			 tfn t))
	     (foundtags (semanticdb-find-tags-by-name-method
			 realtable (semantic-tag-name T))))
	(dolist (FT foundtags)
	  (semantic--tag-put-property FT :filename tfn)
	  (setq tagret (cons FT tagret)))))
    tagret))

(defmethod semanticdb-javap-resolve-proxy ((obj semanticdb-table-java-directory) tag)
  "For the javap table OBJ, Resolve the proxy in TAG."
  (car (semanticdb-normalize-tags obj (list tag))))

;;; Typecache support
;;
;; The best way to get new types easilly discovered is to directly support the
;; typecache.  Sine out database is full of file names with one class per file,
;; we can easily create the tags based on the files.
(defmethod semanticdb-typecache-file-tags ((table semanticdb-table-java-directory))
  "Create a list of tags from the files in the directory represented by this table."
  (let* ((dir (oref table directory))
	 (files (directory-files dir t "\\.java$"))
	 (tags nil))
    (dolist (F files)
      (setq tags (cons (semanticdb-javap-file-to-tag F table) tags)))
    ;; The typecache can't use our usual tags, but we gave them proxies
    ;; so the typecache can fix them.
    tags))

;;; Search Utils
;;
(defun semanticdb-javap-file-to-tag (file db)
  "Convert the FILE name into a tag.
DB is the database or table with a proxy to place on the tag.
Assume a file foo.java will become a class called foo."
  (when (stringp file)
    (let* ((base (file-name-sans-extension (file-name-nondirectory file)))
	   (tag (semantic-tag-new-type base "class" nil nil)))
      (semantic--tag-put-property tag :packagedir (semanticdb-table-java-package db))
      (semantic-tag-set-proxy tag (oref db :proxy) file)
      tag)))

(defmethod semanticdb-find-tags-by-name-method ((table semanticdb-table-java-directory) name &optional tags)
  "In TABLE, find all occurrences of tags with NAME.
Optional argument TAGS is a list of tags to search.
Returns a list of all files in this table's directory that matches NAME."
  (if tags (call-next-method)
    ;; Look for a file matching NAME.
    (let* ((dir (oref table directory))
	   (files (directory-files dir t (concat "^" (regexp-quote name) "\\.java")))
	   (tags nil))
      (dolist (F files)
	(setq tags (cons (semanticdb-javap-file-to-tag F table) tags)))
      tags)))
	
(defmethod semanticdb-find-tags-by-name-regexp-method ((table semanticdb-table-java-directory) regexp &optional tags)
  "In TABLE, find all occurrences of tags matching REGEXP.
Optional argument TAGS is a list of tags to search.
Returns a list of all files in this table's directory that matches REGEXP."
  (if tags (call-next-method)
    ;; Look for a file matching NAME.
    (let* ((dir (oref table directory))
	   (files (directory-files dir t regexp))
	   (tags nil))
      (dolist (F files)
	(setq tags (cons (semanticdb-javap-file-to-tag F table) tags)))
      tags)))
	
(defmethod semanticdb-find-tags-for-completion-method ((table semanticdb-table-java-directory) prefix &optional tags)
  "In TABLE, find all occurrences of tags starting with PREFIX.
Optional argument TAGS is a list of tags to search.
Returns a list of all files in this table's directory that matches REGEXP."
  (if tags (call-next-method)
    ;; Look for a file matching NAME.
    (let* ((dir (oref table directory))
	   (files (directory-files dir t (concat "^" (regexp-quote prefix))))
	   (tags nil))
      (dolist (F files)
	(setq tags (cons (semanticdb-javap-file-to-tag F table) tags)))
      tags)))

;;; DIRECTORY DB TABLE IN A JAR FILE
;;
;; Here is a database table for searching a directory where any .java file
;; is also a class tag.
(defclass semanticdb-table-jar-directory (semanticdb-abstract-table)
  ((major-mode :initform 'java-mode)
   (directory :initarg :directory
	      :type string
	      :documentation
	      "The directory in the jar file this table is trying to represent.")
   (filenamecache :type list
		  :documentation
		  "The list of files in the jar file in directory.")
   (packagenamecache :type list
		     :documentation
		     "The list of subpackages in a jar file in directory.")
   (filetaghash :type hash-table
		:initform (make-hash-table :test 'equal :size 11)
		:documentation
		"As files are parsed w/ javap, hash the tag tables against
the file name form filenamecache.")
   (proxy :initarg :proxy
	  :type symbol
	  :documentation
	  "The proxy symbol used when creating proxy tags based on file names.
See `semantic-create-proxy-tag' for details.")
   )
  "Table which represents the classes found in files in series of diretories.")

(defmethod semanticdb-table-java-package ((table semanticdb-table-jar-directory))
  "Get the package name to use for this database."
  (oref table directory))

(defmethod initialize-instance :AFTER ((table semanticdb-table-jar-directory) &rest args)
  "After TABLE is initialized, make sure basic features are set."
  (oset table proxy (semantic-create-tag-proxy 'semanticdb-javap-resolve-proxy table))
  )

(defmethod semanticdb-refresh-table ((obj semanticdb-table-jar-directory) &optional force)
  "Java Directories should be refreshed when files in the directory hange.
No nothing for now."
  nil)

(defmethod semanticdb-needs-refresh-p ((obj semanticdb-table-jar-directory))
  "Return nil, we don't need a refresh until we figure out directory tracking."
  nil)

(defmethod semanticdb-equivalent-mode ((table semanticdb-table-jar-directory) &optional buffer)
  "Return non-nil if TABLE's mode is equivalent to BUFFER.
Equivalent modes are specified by the `semantic-equivalent-major-modes'
local variable."
  t
  )

(defmethod object-print ((obj semanticdb-table-jar-directory) &rest strings)
  "Pretty printer extension for `semanticdb-table-jar-directory'.
Adds the number of tags in this file to the object print name."
  (apply 'call-next-method obj
	 (cons (format " (%d Classes, %d Packages)"
		       (length (oref obj filenamecache))
		       (length (oref obj packagenamecache)))
	       strings)))

;;; Tag Normailzation
;;
;; The tags produced are faux tags that have a file, but no details.  They
;; need to be normalized, or converted into a full formed tag by reading in
;; the file they are originated from.
(defmethod semanticdb-jar-extract-and-save-tags ((obj semanticdb-table-jar-directory) tagfname)
  "Extract the tags from TAGFNAME from the database jar file.
Save the tags in our table in the tag hash.
If it is already in the tag hash, then just return that."
  (let* ((hash (oref obj filetaghash))
	 (realtable (gethash tagfname hash)))

    ;; If it isn't in the hash, then calculate it.
    (unless realtable
      (let* ((pdb (oref obj parent-db))
	     (jar (oref pdb reference-directory)))
	(setq realtable (semanticdb-javap-extract-tag-table jar tagfname))
	;; Save the real table into our tag hash.
	(if (not realtable) (setq realtable 'no-tags))
	(puthash tagfname realtable hash)))

    ;; Return the tag table, either from hash or extracted.  convert
    ;; the no-tags symbol into nil.
    (if (eq realtable 'no-tags)
	nil
      realtable)))

(defmethod semanticdb-normalize-tags ((obj semanticdb-table-jar-directory) tags)
  "Convert tags found by our java directory table into a complete tag.
The default tag just has a name, type, and the filename.  Normalize by
loading in the file it belongs to, and looking up that symbol in the file
and returning that tag instead."
  (let ((tagret nil)
	(parentdb (oref obj parent-db)))
    (dolist (T tags)
      (let* ((tfn (semantic-tag-file-name T))
	     (realtable (semanticdb-jar-extract-and-save-tags obj tfn))
	     (foundtags (semanticdb-find-tags-by-name-method
			 realtable (semantic-tag-name T))))
	(dolist (FT foundtags)
	  (semantic--tag-put-property FT :filename tfn)
	  (setq tagret (cons FT tagret)))))
    tagret))

(defmethod semanticdb-javap-resolve-proxy ((obj semanticdb-table-jar-directory) tag)
  "For the javap table OBJ, Resolve the proxy in TAG."
  (car (semanticdb-normalize-tags obj (list tag))))

;;; Typecache support
;;
;; The best way to get new types easilly discovered is to directly support the
;; typecache.  Sine out database is full of file names with one class per file,
;; we can easily create the tags based on the files.
(defmethod semanticdb-typecache-file-tags ((table semanticdb-table-jar-directory))
  "Create a list of tags from the files in the directory represented by this table."
  (let* ((dir (oref table directory))
	 (files (oref table filenamecache))
	 (tags nil))
    (dolist (F files)
      (setq tags (cons (semanticdb-javap-file-to-tag F table) tags)))
    ;; The typecache can't use our usual tags, but we gave them proxies
    ;; so the typecache can fix them.
    tags))

(defmethod semanticdb-table-javap-table-as-faux-tag ((table semanticdb-table-jar-directory))
  "Convert a database into a faux tag which child tags.
The child tags will NOT have additional child tags.  This is solely to solve
the problem of completion engines that only need to drop down a single level
for the typecache search routines.
NOTE: All tags are created as public.  This is probably incorrect.
Solve this some other time."
  (let* ((name (file-name-nondirectory
		(directory-file-name (oref table directory))))
	 (chilpkg (mapcar
		   (lambda (pkg)
		     (semantic-tag-new-type 
		      (file-name-nondirectory (directory-file-name pkg)) ;; name
		      "namespace" ;; namespace datatype
		      nil	  ;; no children
		      nil	  ;; no inheritance
		      :faux t
		      :typemodifiers '("public")))
		   (oref table packagenamecache)))
	 (chilclass (mapcar
		     (lambda (cls)
		       (semantic-tag-new-type
			(file-name-sans-extension cls)
			"class"
			nil ;; There are members, but we're making stuff up.
			nil ;; We won't know this either.
			:faux t
			:typemodifiers '("public")))
		     (oref table filenamecache))))
    ;; Create a new namespace full of our made up children.
    (semantic-tag-new-type name "namespace"
			   (append chilpkg chilclass)
			   nil
			   :faux t
			   :typemodifiers '("public"))))

;;; Search Utils
;;
(defmethod semanticdb-find-tags-by-name-method ((table semanticdb-table-jar-directory) name &optional tags)
  "In TABLE, find all occurrences of tags with NAME.
Optional argument TAGS is a list of tags to search.
Returns a list of all files in this table's directory that matches NAME."
  (if tags (call-next-method)
    ;; Look for a file matching NAME.
    (let* ((dir (oref table directory))
	   (files (oref table filenamecache))
	   (tags nil)
	   (regexp (concat "^" (regexp-quote name) "\\.class$")))
      (dolist (F files)
	(when (string-match regexp F)
	  (setq tags (cons (semanticdb-javap-file-to-tag F table) tags))))
      tags)))
	
(defmethod semanticdb-find-tags-by-name-regexp-method ((table semanticdb-table-jar-directory) regexp &optional tags)
  "In TABLE, find all occurrences of tags matching REGEXP.
Optional argument TAGS is a list of tags to search.
Returns a list of all files in this table's directory that matches REGEXP."
  (if tags (call-next-method)
    ;; Look for a file matching NAME.
    (let* ((dir (oref table directory))
	   (files (oref table filenamecache))
	   (tags nil))
      (dolist (F files)
	(when (string-match regexp F)
	  (setq tags (cons (semanticdb-javap-file-to-tag F table) tags))))
      tags)))
	
(defmethod semanticdb-find-tags-for-completion-method ((table semanticdb-table-jar-directory) prefix &optional tags)
  "In TABLE, find all occurrences of tags starting with PREFIX.
Optional argument TAGS is a list of tags to search.
Returns a list of all files in this table's directory that matches REGEXP."
  (if tags (call-next-method)
    ;; Look for a file matching NAME.
    (let* ((dir (oref table directory))
	   (files (oref table filenamecache))
	   (regexp (concat "^" (regexp-quote prefix)))
	   (tags nil))
      (dolist (F files)
	(when (string-match regexp F)
	  (setq tags (cons (semanticdb-javap-file-to-tag F table) tags))))
      tags)))

;;; ONE FILE IN A JAR FILE
;;
;; Here is a database that will represent a single file in a Jar file.
;; These tables will be created for "import" statements.
;;
;;@TODO - can I make them persistent like semanticdb-file.el?
;;
(defclass semanticdb-table-jar-file (semanticdb-abstract-table)
  ((major-mode :initform 'java-mode)
   (filename :type string
	     :initarg :filename
	     :documentation
	     "The full filename in the jar file.")
   (needsrefresh :initform t
		 :documentation
		 "Track if this table is up to date.")
   )
  "Table which represents the classes found in files in series of diretories.")

(defmethod semanticdb-table-java-package ((table semanticdb-table-jar-file))
  "Get the package name to use for this database as a directory."
  (file-name-directory (oref table filename)))

(defmethod semanticdb-refresh-table ((obj semanticdb-table-jar-file) &optional force)
  "Java Directories should be refreshed when files in the directory hange.
No nothing for now."
  (when (semanticdb-needs-refresh-p obj)
    (let* ((parent (oref obj parent-db))
	   (jar (oref parent reference-directory)))
      (oset obj tags (semanticdb-javap-extract-tag-table 
		      jar (oref obj filename)))
      (oset obj needsrefresh nil))))

(defmethod semanticdb-needs-refresh-p ((obj semanticdb-table-jar-file))
  "Return nil, we don't need a refresh until we figure out directory tracking."
  (oref obj needsrefresh))

(defmethod semanticdb-equivalent-mode ((table semanticdb-table-jar-file) &optional buffer)
  "Return non-nil if TABLE's mode is equivalent to BUFFER.
Equivalent modes are specified by the `semantic-equivalent-major-modes'
local variable."
  ;; @TODO - FIX - only match java & jdee
  t
  )

;;; TYPECACHE Overrides
;;
;; In order to expand a string like com.java.object.THING from a jar
;; file, we'll need to override the default typecache methods.  This
;; is because those depend on a complex typecache, whereas javap can
;; just do the expansion of the package for us w/out that.
(define-mode-local-override semanticdb-typecache-find 
  java-mode (type &optional path find-file-match)
  "For Java, try the default.  If nothing, look in our JAR files.
This is because a string such as com.java.pgk.Class doesn't show up
as a hierarchy from the JAR files in the regular typecache, but we
can find it as a string directly from our directory and jar files."
  (if (not (and (featurep 'semantic/db) semanticdb-current-database))
      nil ;; No DB, no search
    ;; Else, try a few things.
    (let ((lang-map (semanticdb-javap-get-java-lang-classes-map)))
      (when (and lang-map (stringp type))
	(setq type (gethash type lang-map type)))
      (when (and lang-map (listp type))
	(setq type (gethash (car type) lang-map type)))
      (or (semanticdb-typecache-find-default type path find-file-match)
	  (semanticdb-javap-typecache-find-by-include-hack
	   type (or path semanticdb-current-table) find-file-match)))))

(defun semanticdb-javap-typecache-find-by-include-hack (type &optional path find-file-match)
  "Search through java DIRECTORY databases for TYPE based on PATH.
PATH is a database for the buffer from which the references should be derived.
For each, ask if TYPE is found.  If TYPE is a fully qualified name, leave it alone.
If it is a list, glom it back into a string for the search.
Uses `semanticdb-find-table-for-include' to find the TYPE by fully qualified name
using the same utility as looking for includes which are also fully qualified names."
  (let* ((tname (cond ((stringp type) type)
		      ((listp type) (mapconcat #'identity type "."))))
	 (fauxtag (semantic-tag-new-include tname nil :faux t))
	 (table (semanticdb-find-table-for-include fauxtag path))
	 (ans nil))
    ;; Look the answer up in TABLE from include search.
    (if table
	(let* ((tlist (cond ((listp type) type)
			    ((stringp type) (semantic-analyze-split-name type))))
	       (searchname (if (listp tlist) (car (last tlist))
			     tlist))
	       ;; Get the typecache version of the tags from the table.
	       ;; That will allow us to do a typecache like search
	       (tabletags (semanticdb-typecache-file-tags table))
	       )
	  (setq ans (semantic-find-first-tag-by-name searchname tabletags))
	  )
      ;; If there was no table, then perhaps it is just a package name.
      ;; We can look up "java.com.*" instead of just "java.com.".
      (semantic-tag-set-name fauxtag (concat tname "*"))
      (setq table (semanticdb-find-table-for-include fauxtag path))

      (when (semanticdb-table-jar-directory-child-p table)
	(setq ans (semanticdb-table-javap-table-as-faux-tag table))
	)
      )
    ;; Return what we found.
    ans))

;;; Typecache support
;;
;; The best way to get new types easilly discovered is to directly support the
;; typecache.  Sine out database is full of file names with one class per file,
;; we can easily create the tags based on the files.
(defmethod semanticdb-typecache-file-tags ((table semanticdb-table-jar-file))
  "Create a list of tags from the files in the directory represented by this table."
  (semanticdb-refresh-table table)
  ;; For regular files, there is a pile of logic to deal with
  ;; "merging" in all the types from other files.  In Java there is no
  ;; recursion, and should only be one type we care about for each
  ;; class file, so we don't need to deal with that crud.
  (let ((tagstomerge (semantic-find-tags-by-class
		      'type (semanticdb-get-tags table))))
    (semanticdb-typecache-merge-streams tagstomerge nil)))

;;; CLASSPATH ROOT Databases
;;
;; These databases are used , where directories represent package
;; names.  We can fake out a Jar file through a database.  The
;; database will return specialized javap tables for extracting info
;; from the jar file.
(defclass semanticdb-java-jar-database (semanticdb-project-database)
  ((new-table-class :initform semanticdb-table-jar-file)
   (new-table-dir-class :allocation :class
			:initform semanticdb-table-jar-directory
			:documentation
			"The class to use for creating new dir tables.
Directory tables are used to simulate a tag table where file names match
some tags in those files.")
   ;; Note: reference-directory is the jar file.
   (jarfilecache :initform nil
		 :documentation
		 "A Cache of all the file names in the Jar file.
This is the cache searched for creating tables from package names
and file names.")
   )
  "Table which represents the classes found in files in series of diretories.")

(defmethod semanticdb-create-database :STATIC ((dbc semanticdb-java-jar-database)
					       jarfile)
  "Create a new semantic database for JARFILE and return it.
If a database for JARFILE has already been loaded, return it.
If no database for JARFILE exists, then extract the database basic contents
from JARFILE, and create a database for it."
  ;; Make sure this is fully expanded so we don't get duplicates.
  (setq jarfile (file-truename jarfile))
  (let* ((db (eieio-instance-tracker-find jarfile 'reference-directory
					  'semanticdb-database-list))
	 )
    (unless db
      (setq db (make-instance
		dbc
		(concat (file-name-nondirectory jarfile))
		:tables nil))
      ;; Set this up here.   We can't put it in the constructor because it
      ;; would be saved, and we want DB files to be portable.
      (oset db reference-directory jarfile)
      (semanticdb-java-jar-extract-names db))
    db))

(defmethod object-print ((obj semanticdb-java-jar-database) &rest strings)
  "Pretty printer extension for `semanticdb-java-jar-database'.
Adds the number of tags in this file to the object print name."
  (apply 'call-next-method obj
	 (cons (format " (%d Files)"
		       (length (oref obj jarfilecache)))
	       strings)))

(defmethod semanticdb-java-jar-extract-names ((dbc semanticdb-java-jar-database))
  "Extract the directory structure from the jar file associated with DBC.
Store the structure in our database cache."
  (let ((strs (cedet-jar-table-of-contents (oref dbc reference-directory))))
    (oset dbc jarfilecache strs)))

(defmethod semanticdb-file-table ((dbc semanticdb-java-jar-database) dir)
  "Return a database table from DBC for DIR (the file).
This overrides the default `semanticdb-file-table' as this database creates
tables of classes based on files, not files in a directory.
DIR is a package name, such as 'java/net' which contains classes.
We assume that java classes are rooted to be base of the jar file they
are extracted from, so no prefixes are added."
  (when (stringp dir)
    (or
     ;; Raw classes come in w/out the .class extension, so if there is an extension, remove it.
     (object-assoc (concat (file-name-sans-extension dir) ".class") :filename (oref dbc tables))
     ;; The dir may be a directory, so check that too.
     (object-assoc dir :directory (oref dbc tables)))))

(defmethod semanticdb-create-table ((db semanticdb-java-jar-database) dirorfile)
  "Create a new table in DB for DIR and return it.
This overrides the default `semanticdb-create-table' as this database
creates tables of classes based on files, not files in a directory.
The class of DB contains the class name for the type of table to create.
If the table for DIR exists, return it.
If the table for DIR does not exist, create one."
  (when (stringp dirorfile)
    (let ((newtab (semanticdb-file-table db dirorfile)))
      (unless newtab
	(let ((matchingfiles (or (semanticdb-java-jar-package-files db dirorfile)
				 (semanticdb-java-jar-package-one-file db dirorfile)))
	      (matchingpkg (semanticdb-java-jar-package-packages db dirorfile)))
	  (when matchingfiles
	    ;; Only make a table if there are any matching files in it.
	    (if (= (length matchingfiles) 1)
		;; If there is only one table, create a jar-file table.
		(setq newtab (funcall (oref db new-table-class)
				      (car matchingfiles)
				      :filename (car matchingfiles)))
	      ;; If there are multiple files, then we want a directory
	      ;; The file extractor restricts itself to .class, so no dups?
	      (setq newtab (funcall (oref db new-table-dir-class)
				    dirorfile
				    :directory dirorfile))
	      (oset newtab filenamecache
		    (mapcar 'file-name-nondirectory matchingfiles))
	      (oset newtab packagenamecache matchingpkg)
	      )
	    (oset newtab parent-db db)
	    (object-add-to-list db 'tables newtab t)
	    )))
      newtab)))

(defmethod semanticdb-java-jar-package-files ((dbc semanticdb-java-jar-database) dir)
  "Get the file class names from DBC that match DIR."
  (when (stringp dir)
    (setq dir (file-name-as-directory dir))
    (let ((ans nil))
      (dolist (F (oref dbc jarfilecache))
	(when (string-match (concat "^" (regexp-quote dir) "[a-zA-Z_]*\\.class$")
			    F)
	  (push F ans)))
      (nreverse ans))))

(defmethod semanticdb-java-jar-package-one-file ((dbc semanticdb-java-jar-database) file)
  "Get the file class names from DBC that match FILE.
File should exclude an extension, as .class will be added."
  (when (stringp file)
    (let ((ans nil))
      (dolist (F (oref dbc jarfilecache))
	(when (string-match (concat "^" (regexp-quote file) "\\.class$") F)
	  (push F ans)))
      (nreverse ans))))

(defmethod semanticdb-java-jar-package-packages ((dbc semanticdb-java-jar-database) dir)
  "Get the package names from DBC that match DIR.
DIR may already have some .class files in it (see `semanticdb-java-jar-package-files')
while also having sub-packages."
  (when (stringp dir)
    (let ((ans nil))
      (dolist (F (oref dbc jarfilecache))
	(when (string-match (concat "^" (regexp-quote dir) "\\w+/") F)
	  (add-to-list 'ans (match-string 0 F))))
      (nreverse ans))))

;;; JAVAP CALLS
;;
;; Given a .jar file, and a class file name, retrieve a detailed tag list
;; for the public and protected API.  (Assuming the private API is never accessible anyway.

(defun semanticdb-javap-extract-tag-table (jarfile qualifiedclassfile)
  "Within JARFILE, get QUALIFIEDCLASSFILE's tag table.
JARFILE is the full filename to some jar file.
QUALIFIEDCLASSFILE is a filename with package qualifiers
to some class in JARFILE."
  (when (not (file-exists-p jarfile))
    (error "Javap: Cannot find %S" jarfile))
  (let ((javapbuff (cedet-javap-get-class
		    jarfile
		    (file-name-sans-extension qualifiedclassfile))))
    (save-current-buffer
      (set-buffer javapbuff)
      (goto-char (point-min))
      ;; The first line can say "Compiled from ..." or some-such.
      (let* ((case-fold-search nil)
	     (p (search-forward "Compiled from" nil t)))
	(when (and p (numberp p))
	  (goto-char p)
	  (beginning-of-line)
	  (insert "// ")))
      
      ;; strip out fully qualified part of class- and interface names
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward "\\(class\\|interface\\) \\([^\\. ]*\\.\\)+" nil t)
	  (replace-match "\\1 " nil nil))
	)
      
      ;; Enable java mode and semantic parsing.
      (java-mode)
      (semantic-new-buffer-fcn)
      ;;Get the tags, and strip out buffer information.
      (let ((tagsout (semantic-fetch-tags)))
	;; Changes tags in place.
	(semantic--tag-unlink-list-from-buffer tagsout)
	;; Return the tag table
	tagsout))))

(defun semanticdb-javap-paths-for-package (buffer)
  "Return a list of database tables representing a path for tags the package in BUFFER.
In Java the current package is accissible as if you imported the whole thing.
This will look across the classpath for all occurances of your package."
  (save-current-buffer
    (set-buffer buffer)
    (let ((pk (car-safe (semantic-find-tags-by-class 'package buffer))))
      (when pk
	(let* (;; I know it isn't an include, but the work is the same for java
	       (pkfile (file-name-sans-extension (semantic-tag-include-filename pk)))
	       ;; Get this buffer's classpath strings and objects
	       (cpo (semanticdb-javap-classpath-objects buffer))
	       (ans nil)
	       )

	  ;; Loop over the classpath files and objects, and and those next
	  (dolist (P cpo)
	    (cond
	     ;; Strings are just compound tables, like the EDE paths above.
	     ;; We need to add the pkfile to the packdir P to see if
	     ;; our package is in there.
	     ((stringp P)
	      (let ((pksub (expand-file-name pkfile P))
		    (tab nil))
		(when (file-exists-p pksub)
		  (setq tab (semanticdb-javap-dir-to-compound-table pksub))
		  (push tab ans))))

	     ;; If it is a jar file, then extract a table for pkfile here.
	     ((semanticdb-java-jar-database-child-p P)
	      (let ((tab (semanticdb-create-table P pkfile)))
		(when tab (push tab ans))))

	     (t (error "Unknown object in classpath: %S" P))))

	  ans)))))

(defconst semanticdb-javap-java-lang-name "java/lang"
  "Name of java.lang package that is always imported")

(defvar semanticdb-javap-java-lang-table nil
  "Cached value of semanticdb table for java.lang package")

(defun semanticdb-javap-paths-for-java-lang ()
  "Scan the jdk jar file, if available, for java.lang.* classes and return a table for the package"
  (if semanticdb-javap-java-lang-table
      semanticdb-javap-java-lang-table
    (let ((core-jar (cedet-java-find-jdk-core-jar)))
      (when core-jar
	(let* ((core-db (semanticdb-create-database semanticdb-java-jar-database core-jar))
	       (java-lang-table (semanticdb-create-table core-db semanticdb-javap-java-lang-name)))
	  (when java-lang-table
	    (setq semanticdb-javap-java-lang-table java-lang-table))
	  java-lang-table)))))

(defvar semanticdb-javap-java-lang-classes nil
  "Cache to store list of classes from java.lang package")

;;;###autoload
(defun semanticdb-javap-get-java-lang-classes ()
  "Returns list of classes defined in java.lang package (in format for direct injection
into the tags)"
  (if semanticdb-javap-java-lang-classes
      semanticdb-javap-java-lang-classes
    (let ((core-db (semanticdb-create-database semanticdb-java-jar-database
					       (cedet-java-find-jdk-core-jar))))
      (when core-db
	(let ((res (mapcar (lambda (x)
			     (replace-regexp-in-string "/" "."
						       (substring x 0 (- (length x) 6)))
			     )
			   (semanticdb-java-jar-package-files core-db semanticdb-javap-java-lang-name))))
	  (setq semanticdb-javap-java-lang-classes res))))))

(defvar semanticdb-javap-java-lang-classes-map nil
  "")

(defun semanticdb-javap-get-java-lang-classes-map ()
  ""
  (if semanticdb-javap-java-lang-classes-map
      semanticdb-javap-java-lang-classes-map
    (let* ((lang-classes (semanticdb-javap-get-java-lang-classes))
	  (lang-map (make-hash-table :size (length lang-classes)
				     :test 'equal)))
      (when lang-classes
	(dolist (L lang-classes)
	  (puthash (replace-regexp-in-string ".*\\.\\([^.]*\\)$" "\\1" L) L lang-map))
	(setq semanticdb-javap-java-lang-classes-map lang-map)
	lang-map))))

(defun semanticdb-javap-file-is-jar (file-name)
  "Returns true if the file is java archive"
  (let ((file-ext (file-name-extension file-name)))
    (when (not (null file-ext))
      (let ((dfile-ext (downcase file-ext)))
	(or (string= "jar" dfile-ext)
	    (string= "zip" dfile-ext))))))

(defun semanticdb-javap-classpath-objects (buffer)
  "Return the classpath for BUFFER as a list of semanticdb objects and strings.
Strings are directories which can be searched.  Database objects
represent jar files."
  (save-current-buffer
    (set-buffer buffer)
    (let* (;; Try EDE to see if it responds.
	   (edepaths (when ede-object-project
		       (ede-source-paths ede-object-project 'java-mode)))
	   ;; Try EDE's classpath feature
	   (edeclasspath (when ede-object-project
			   (ede-java-classpath ede-object-project)))
	   (core-jar (cedet-java-find-jdk-core-jar))
	   ;; Try JDEE to see if it knows
	   ;; (jdeep - what to put here??)
	   ;; Try our classpath
	   ;; (classp - what to put here??)
	   ;; Convert to a path
	   (cpaths nil)
	   (ans nil)
	   )
      ;; Get a list of paths together
      (dolist (P (append (list core-jar) edepaths edeclasspath semanticdb-javap-classpath))
	(cond
	 ;; Somtimes a null or non-string gets in.  Ignore it.
	 ((or (null P) (not (stringp P)))
	  nil)
	 ;; A directory can be returned as a string.  Should we make a
	 ;; special dir for this???  @TODO
	 ((file-directory-p P)
	  (push P cpaths))
	 ;; Jar files need a special database
	 ((and (semanticdb-javap-file-is-jar P)
	       (file-exists-p P))
	  (push
	   (semanticdb-create-database semanticdb-java-jar-database P)
	   cpaths))
	 ;; What else?  Ignore.
	 (t
	  (message "Classpath: %S not found" P))))
      cpaths)))

(define-mode-local-override semanticdb-find-table-for-include
  java-mode (importtag &optional table)
  "For an IMPORTTAG in Java, return a table object that contains its tags.
See `semanticdb-find-table-for-include' for details.
Note that for an import of *, this function will return an unusual table
that just points to a database, and must redirect all its calls to its decendents."
  ;; Remove the extension.  The java version of semantic-tag-include-filename
  ;; will glom the .java extension on it.
  (let* ((fname (file-name-sans-extension (semantic-tag-include-filename importtag)))
	 (def (semanticdb-find-table-for-include-default importtag table))
	 (starry (string-match "\\*$" fname)))
    ;; If the default implementation returns something, go with it.
    (if def def
      (let ((cpo (semanticdb-javap-classpath-objects (current-buffer))))
	;; Loop over the classpath, see if we can find it anywhere.
	(catch 'foo
	  (dolist (P cpo)
	    (let ((expanded (expand-file-name fname P)))
	      (cond
	       ((stringp P)
		(if starry
		    (progn
		      (setq def (semanticdb-javap-dir-to-compound-table (file-name-directory expanded)))
		      (when def
			(throw 'foo nil)))
		  ;; If it is a string, just glom the two together.
		  (let ((java  (concat expanded ".java"))
			(class (concat expanded ".class")))
		    (cond 
		     ((file-exists-p java)
		      (setq def (semanticdb-file-table-object java))
		      (throw 'foo nil))
		     ((file-exists-p class)
		      ;; need to javap the thing, but w/out the db...
		      nil)))))
	       
	       ;; For jar databases, we need to extract a table of classes first.
	       ((semanticdb-java-jar-database-child-p P)
		(let ((tab (semanticdb-create-table
			    P (if starry (file-name-directory fname) fname))))
		  (when tab
		    ;; We have a table, now we need to extract the tags from it.
		    ;; TODO - just return the table normally for now.  Fix later.
		    (setq def tab)
		    (throw 'foo nil))))
	       
	       ;; Default - do ntohing
	       (t nil)))))
	def))))

(defun semanticdb-javap-dir-to-compound-table (packagedir &optional classpath)
  "Convert the PACKAGEDIRORJAR into a compound table for PACKAGEDIR.
Represents all the files thereunder which can contain tags of interest.
If a CLASSPATH is provided, and if packagedir doesn't exist, search CLASSPATH
for PACKAGEDIR.  The CLASSPATH can contain strings or javap database objects."
  (let ((finddir (file-name-as-directory packagedir))) ;; force / at end every time
    (let ((found (eieio-instance-tracker-find
		  finddir 'directory
		  'semanticdb-javap-directory-tracker)))
      (cond
       ;; Return something from the cache.
       (found found)
       ;; Not found by default.  Does this thing exist at all?  Make one.
       ((and (file-exists-p packagedir) (file-directory-p packagedir))
	(semanticdb-table-java-directory
	 (file-name-nondirectory (directory-file-name finddir))
	 :directory finddir))
       ;; Should we search the classpath?
       ))))

(provide 'semantic/db-javap)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "semantic/db-javap"
;; End:

;;; semanticdb-javap.el ends here
