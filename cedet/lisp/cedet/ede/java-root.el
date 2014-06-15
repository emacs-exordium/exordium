;;; ede/java-root.el --- A simple way to wrap a java project with a single root

;; Copyright (C) 2012 Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <eric@siege-engine.com>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; NOTE: ede/java-root.el has been commented so as to also make it
;;       useful for learning how to make similar project types.
;;
;; There are a lot of java project types.  For pre-existing code, it
;; is often helpful jut to be able to wrap the whole thing up in as
;; simple a way as possible.
;;
;; The java-root project type will allow you to create a single object
;; with no save-file in your .emacs file that will be recognized, and
;; provide a way to easily allow EDE to provide Semantic with the
;; ability to use java features, and other various source files
;; quickly.
;;
;; The java-root class knows a few things about java projects, such as
;; how to update the classpath, or localclasspath for finding imports,
;; and enabling smart-completion.
;;
;;; EXAMPLE
;;
;; Add this to your .emacs file, modifying appropriate bits as needed.
;;
;; (ede-java-root-project "SOMENAME" :file "/dir/to/some/file")
;;
;; Replace SOMENAME with whatever name you want, and the filename to
;; an actual file at the root of your project.  It might be a
;; build file, a README file.  Whatever.  It doesn't matter.  It's just
;; a key to hang the rest of EDE off of.
;;
;; The most likely reason to create this project, is to help make
;; finding files within the project faster.  In conjunction with
;; Semantic completion, having a short include path is key.  You can
;; override the include path like this:
;;
;; (ede-java-root-project "NAME" :file "FILENAME"
;;     :srcroot '( "src" )
;;     :classpath '( "/absolute/path.jar" )
;;     :localclasspath '( "/relative/path.jar" )
;;     )
;;
;; In this example, the SRCROOT is where in your java project the root
;; of the java sources are.  For example, if you have a class named 
;; "com.fred.Fred.java", and the root of your source tree is in the
;; "src" subdirectory of your project, then
;; /myproject/src/com/fred/Fred.java would be where your class is.
;;
;; If you want to override the file-finding tool with your own
;; function you can do this:
;;
;; (ede-java-root-project "NAME" :file "FILENAME" :locate-fcn 'MYFCN)
;;
;; Where FILENAME is a file in the root directory of the project.
;; Where MYFCN is a symbol for a function.  See:
;;
;; M-x describe-class RET ede-java-root-project RET
;;
;; for documentation about the locate-fcn extension.
;;
;;; ADVANCED EXAMPLE
;;
;; If the java-root project style is right for you, but you want a
;; dynamic loader, instead of hard-coding values in your .emacs, you
;; can do that too, but you will need to write some lisp code.
;;
;; To do that, you need to add an entry to the
;; `ede-project-class-files' list, and also provide two functions to
;; teach EDE how to load your project pattern
;;
;; It would look like this:
;;
;; (defun MY-FILE-FOR-DIR (&optional dir)
;;   "Return a full file name to the project file stored in DIR."
;;   <write your code here, or return nil>
;;   )
;;
;; (defun MY-ROOT-FCN ()
;;   "Return the root directory for `default-directory'"
;;   ;; You might be able to use `ede-java-root-project-root'.
;;   )
;;
;; (defun MY-LOAD (dir)
;;   "Load a project of type `java-root' for the directory DIR.
;; Return nil if there isn't one."
;;   (ede-java-root-project "NAME" :file (expand-file-name "FILE" dir)
;;                                :locate-fcn 'MYFCN)
;;   )
;;
;; (add-to-list 'ede-project-class-files
;; 	     (ede-project-autoload "java-root"
;; 	      :name "JAVA ROOT"
;; 	      :file 'ede/java-root
;; 	      :proj-file 'MY-FILE-FOR-DIR
;;            :proj-root 'MY-ROOT-FCN
;; 	      :load-type 'MY-LOAD
;; 	      :class-sym 'ede-java-root-project
;;	      :safe-p t)
;; 	     t)
;;

(require 'ede/jvm-base)

;;; Code:

;;; PROJECT CACHE:
;;
;; java-root projects are created in a .emacs or other config file, but
;; there still needs to be a way for a particular file to be
;; identified against it.  The cache is where we look to map a file
;; against a project.
;;
;; Setting up a simple in-memory cache of active projects allows the
;; user to re-load their configuration file several times without
;; messing up the active project set.
;;
(defvar ede-java-root-project-list nil
  "List of projects created by option `ede-java-root-project'.")

(defun ede-java-root-file-existing (dir)
  "Find a java-root project in the list of java-root projects.
DIR is the directory to search from."
  (let ((projs ede-java-root-project-list)
	(ans nil))
    (while (and projs (not ans))
      (let ((root (ede-project-root-directory (car projs))))
	(when (string-match (concat "^" (regexp-quote root)) dir)
	  (setq ans (car projs))))
      (setq projs (cdr projs)))
    ans))

;;; PROJECT AUTOLOAD CONFIG
;;
;; Each project type registers itself into the project-class list.
;; This way, each time a file is loaded, EDE can map that file to a
;; project.  This project type checks files against the internal cache
;; of projects created by the user.
;;
;; EDE asks two kinds of questions.  One is, does this DIR belong to a
;; project.  If it does, it then asks, what is the ROOT directory to
;; the project in DIR.  This is easy for java-root projects, but more
;; complex for multiply nested projects.
;;
;; If EDE finds out that a project exists for DIR, it then loads that
;; project.  The LOAD routine can either create a new project object
;; (if it needs to load it off disk) or more likely can return an
;; existing object for the discovered directory.  java-root always uses
;; the second case.

(defun ede-java-root-project-file-for-dir (&optional dir)
  "Return a full file name to the project file stored in DIR."
  (let ((proj (ede-java-root-file-existing dir)))
    (when proj (oref proj :file))))

(defvar ede-java-root-count 0
  "Count number of hits to the java root thing.
This is a debugging variable to test various optimizations in file
lookup in the main EDE logic.")

;;;###autoload
(defun ede-java-root-project-root (&optional dir)
  "Get the root directory for DIR."
  (let ((projfile (ede-java-root-project-file-for-dir
		   (or dir default-directory))))
    (setq ede-java-root-count (1+ ede-java-root-count))
    ;(debug)
    (when projfile
      (file-name-directory projfile))))

(defun ede-java-root-load (dir &optional rootproj)
  "Return a JAVA root object if you created one.
Return nil if there isn't one.
Argument DIR is the directory it is created for.
ROOTPROJ is nil, since there is only one project."
  ;; Snoop through our master list.
  (ede-java-root-file-existing dir))

;;;###autoload
(ede-add-project-autoload
 (ede-project-autoload "java-root"
		       :name "JAVA ROOT"
		       :file 'ede-java-root
		       :proj-file 'ede-java-root-project-file-for-dir
		       :proj-root 'ede-java-root-project-root
		       :load-type 'ede-java-root-load
		       :class-sym 'ede-java-root
		       :new-p nil
		       :safe-p t)
 ;; When a user creates one of these, it should override any other project
 ;; type that might happen to be in this directory, so force this to the
 ;; very front.
 'unique)

;;; CLASSES
;;
;; EDE sets up projects with two kinds of objects.
;;
;; The PROJECT is a class that represents everything under a directory
;; hierarchy.  A TARGET represents a subset of files within a project.
;; A project can have multiple targets, and multiple sub-projects.
;; Sub projects should map to sub-directories.
;;
;; The JAVA-ROOT project maps any file in java mode to a target for
;; Java files.
;;
;; When creating a custom project the project developer an opportunity
;; to run code to setup various tools whenever an associated buffer is
;; loaded.  The JAVA-ROOT project spends most of its time setting up C
;; level include paths, and PreProcessor macro tables.

(defclass ede-java-root-target (ede-target)
  ()
  "EDE java-root project target.
All directories need at least one target.")

;;;###autoload
(defclass ede-java-root-project (ede-jvm-base-project eieio-instance-tracker)
  ((tracking-symbol :initform 'ede-java-root-project-list)
   (srcroot :initarg :srcroot
	    :initform nil
	    :type list
	    :documentation
	    "A list of roots of the java sources in this project.
Each directory is relative to the directory that :file is in.
This directory is used as part of the class path when searching for
symbols within this project.
Use this if the root of your project is not the same as the root of
your java sources.")
   (localclasspath :initarg :localclasspath
		   :initform nil
		   :type list
		   :documentation
		   "The default classpath used within a project of relative path names.
All files listed in the local class path are relative to this project's root.
This classpath is prepended to CLASSPATH when searching for symbols.
The current project's java source root is always search before this
classpath.")
   (locate-fcn :initarg :locate-fcn
	       :initform nil
	       :type (or null function)
	       :documentation
	       "The locate function can be used in place of
`ede-expand-filename' so you can quickly customize your custom target
to use specialized local routines instead of the EDE routines.
The function symbol must take two arguments:
  NAME - The name of the file to find.
  DIR - The directory root for this java-root project.

It should return the fully qualified file name passed in from NAME.  If that file does not
exist, it should return nil."
	       )
   )
  "EDE java-root project class.
Each directory needs a project file to control it.")

;;; INIT
;;
;; Most projects use `initialize-instance' to do special setup
;; on the object when it is created.  In this case, EDE-JAVA-ROOT can
;; find previous copies of this project, and make sure that one of the
;; objects is deleted.

(defmethod initialize-instance ((this ede-java-root-project)
				&rest fields)
  "Make sure the :file is fully expanded."
  ;; Add ourselves to the master list
  (call-next-method)
  (let ((f (expand-file-name (oref this :file))))
    ;; Remove any previous entries from the main list.
    (let ((old (eieio-instance-tracker-find (file-name-directory f)
					    :directory 'ede-java-root-project-list)))
      ;; This is safe, because :directory isn't filled in till later.
      (when (and old (not (eq old this)))
	(delete-instance old)))
    ;; Basic initialization.
    (when (or (not (file-exists-p f))
	      (file-directory-p f))
      (delete-instance this)
      (error ":file for ede-java-root must be a file"))
    (oset this :file f)
    (oset this :directory (file-name-directory f))
    (ede-project-directory-remove-hash (file-name-directory f))
    (ede-add-project-to-global-list this)
    (unless (slot-boundp this 'targets)
      (oset this :targets nil))
    ;; We need to add ourselves to the master list.
    ;;(setq ede-projects (cons this ede-projects))
    ))

;;; SUBPROJ Management.
;;
;; This is a way to allow a subdirectory to point back to the root
;; project, simplifying authoring new single-point projects.

(defmethod ede-find-subproject-for-directory ((proj ede-java-root-project)
					      dir)
  "Return PROJ, for handling all subdirs below DIR."
  proj)

;;; TARGET MANAGEMENT
;;
;; Creating new targets on a per directory basis is a good way to keep
;; files organized.  See ede-emacs for an example with multiple file
;; types.
(defmethod ede-find-target ((proj ede-java-root-project) buffer)
  "Find an EDE target in PROJ for BUFFER.
If one doesn't exist, create a new one for this directory."
  (let* ((targets (oref proj :targets))
	 (dir default-directory)
	 (ans (object-assoc dir :path targets))
	 )
    (when (not ans)
      (setq ans (ede-java-root-target dir
                 :name (file-name-nondirectory
			(directory-file-name dir))
		 :path dir
		 :source nil))
      (object-add-to-list proj :targets ans)
      )
    ans))

;;; FILE NAMES
;;
;; One of the more important jobs of EDE is to find files in a
;; directory structure.  java-root has tricks it knows about how most java
;; projects are set up with include paths.
;;
;; This tools also uses the ede-locate setup for augmented file name
;; lookup using external tools.
(defmethod ede-expand-filename-impl ((proj ede-java-root-project) name)
  "Within this project PROJ, find the file NAME.
This knows details about or source tree."
  (let ((ans (call-next-method))) ;; using locatedb, etc
    (unless ans
      (let* ((lf (oref proj :locate-fcn))
	     (dir (ede-project-root-directory proj)))
	(if lf
	    (setq ans (funcall lf name dir))
	  (let ((src (oref proj :srcroot))
		(tmp nil))

	    ;; Search srcroot
	    (while (and (not ans) src)
	      ;; Translate into SRCROOT
	      (setq tmp (expand-file-name (car src) dir)) ; expand src 
	      ;; Test this name.
	      (setq tmp (expand-file-name name tmp))
	      (if (file-exists-p tmp)
		  (setq ans tmp))
	      (setq src (cdr src)))

	    (when (not ans)
	      ;; Translate from top of project.
	      (setq tmp (expand-file-name name dir))
	      (if (file-exists-p tmp)
		  (setq ans tmp)))
	    ))))
    ans))

(defmethod ede-project-root ((this ede-java-root-project))
  "Return my root."
  this)

(defmethod ede-project-root-directory ((this ede-java-root-project))
  "Return my root."
  (file-name-directory (oref this :file)))

;;; JAVA SPECIFIC CODE
;;
;; The following code is specific to setting up header files,
;; include lists, and Preprocessor symbol tables.

;; @TODO: should we cache result? or calculate it on project's creation?
(defmethod ede-java-classpath ((proj ede-java-root-project))
  "Return the classpath for this project."
  (let ((lf (or (oref proj :locate-fcn) #'expand-file-name))
	(dir (file-name-directory (oref proj :file)))
	(ret nil))
    (dolist (P (oref proj :localclasspath))
      (if (string= "/" (substring P 0 1))
	  (setq ret (cons (funcall lf (substring P 1) dir) ret))
	(setq ret (cons (funcall lf P dir) ret))))
    (append (nreverse ret) (oref proj :classpath))))

(defmethod ede-source-paths ((proj ede-java-root-project) mode)
  "Get the base to all source trees in the current project."
  (let ((dir (file-name-directory (oref proj :file))))
    (mapcar (lambda (x) (concat dir x)) (oref proj :srcroot))))

(provide 'ede/java-root)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "ede/java-root"
;; End:

;;; ede/java-root.el ends here
