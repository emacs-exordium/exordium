;; Copyright (C) 2012, 2013 Free Software Foundation, Inc.

;; Author: Alex Ott <alexott@gmail.com>

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

(require 'ede/jvm-base)

(defgroup ede-ant nil
  "Emacs Development Environment. Ant options"
  :group 'ede
  :group 'tools
  :group 'extensions)

(defcustom ede-ant-ant-command "ant"
  "Executabe, that will be executed as Ant"
  :group 'ede-ant
  :require  'ede/ant
  :type 'string)

(defcustom ede-ant-ant-options '("-noinput" "-e")
  "Ant's command-line options"
  :group 'ede-ant
  :require 'ede/ant
  :type 'list)

(defcustom ede-ant-execute-ant-to-get-info t
  "Defines, should we execute Ant to get project's information or not."
  :group 'ede-ant
  :require 'ede/ant
  :type 'boolean)

;;;###autoload
(defconst ede-ant-project-file-name "build.xml"
  "name of project file for Ant projects")

;;;###autoload
(defun ede-ant-project-root (&optional dir)
  "Get the Ant root directory for DIR."
  (ede-find-project-root ede-ant-project-file-name dir))

(defvar ede-ant-project-list nil
  "List of projects created by option `ede-ant-project'.")

;;;###autoload
(defun ede-ant-load (dir &optional rootproj)
  "Return a Leiningen Project object if there is a match.
Return nil if there isn't one.
Argument DIR is the directory it is created for.
ROOTPROJ is nil, since there is only one project."
  (or (ede-files-find-existing (file-truename dir) ede-ant-project-list)
      ;; Doesn't already exist, so lets make one.
      (let ((this
             (ede-ant-project "Ant"
			      :name "Ant dir" ; make fancy name from dir here.
			      :directory (file-truename dir)
			      :file (file-truename (expand-file-name ede-ant-project-file-name dir))
			      )))
	(ede-add-project-to-global-list this)
	this)))

;;;###autoload
(defclass ede-ant-project (ede-jvm-base-project eieio-instance-tracker)
  ((tracking-symbol :initform 'ede-ant-project-list)
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
   )
  "EDE Ant project class."
  :method-invocation-order :depth-first)

(defmethod initialize-instance ((this ede-ant-project)
                                &rest fields)
  "Make sure the :targets is setup."
  (call-next-method)
  (ede-normalize-file/directory this ede-ant-project-file-name)
  )

(defmethod project-compile-project ((proj ede-ant-project) &optional command)
  "Compile the entire current project Proj.
Argument COMMAND is the command to use when compiling."
  ;; we need to be in the proj root dir for this to work
  (let ((default-directory (ede-project-root-directory proj)))
    (compile (combine-and-quote-strings
	      (append (list ede-ant-ant-command)
		      ede-ant-ant-options
		      (list (oref proj :current-target))
		      (oref proj :target-options))))))

;;; Classpath-related stuff
;; TODO: how to find include .jars? cedet-files-list-recursively is tooo slow for big file
;; trees. Maybe it's better to use find instead?
(defmethod ede-java-classpath ((proj ede-ant-project))
  "Get classpath for Ant project"
  (let ((dir (ede-project-root-directory proj))
	(ret nil))
    ;; TODO: do this expansion only once?
    (dolist (P (oref proj :localclasspath))
      (if (string= "/" (substring P 0 1))
	  (setq ret (cons (expand-file-name (substring P 1) dir) ret))
	(setq ret (cons (expand-file-name P dir) ret))))
    (let ((cp (append (nreverse ret) (oref proj :classpath))))
      (if cp cp
	;; hack, many Ant projects have libraries in 'lib' directory
	(let ((jar-files (cedet-files-list-recursively (concat dir "lib") ".*\.jar$")))
	  (when jar-files
	    (oset proj :classpath jar-files))
	  jar-files)))))

(defmethod ede-source-paths ((proj ede-ant-project) mode)
  "Get the base to all source trees in the current project."
  (let ((dir (ede-project-root-directory proj)))
    (if (oref proj :srcroot)
	(mapcar (lambda (x) (concat dir x)) (oref proj :srcroot))
      ;; hack, many Ant projects have src & test dirs, or src/java & src/test
      (let* (src-dirs
	     (src-dir1 (concat dir "src/"))
	     (test-dir1 (concat dir "test/"))
	     (src-dir2 (concat dir "src/java/"))
	     (test-dir2 (concat dir "src/test/"))
	     )
	(if (file-exists-p src-dir2)
	    (progn
	      (add-to-list 'src-dirs src-dir2)
	      (when (file-exists-p test-dir2) (add-to-list 'src-dirs test-dir2)))
	    (progn
	      (when (file-exists-p src-dir1) (add-to-list 'src-dirs src-dir1))
	      (when (file-exists-p test-dir1) (add-to-list 'src-dirs test-dir1))))
	;; TODO: add cache?
	(nreverse src-dirs)))))

;; TODO: extract targets, etc.
(defmethod project-rescan ((proj ede-ant-project))
  "Rescan the EDE proj project THIS."
  (when (ede-jvm-base-file-updated-p proj)
    ;; TODO: fill information
    ))

;; How to handle, when we have several files with the same name?
(defmethod ede-expand-filename-impl ((proj ede-ant-project) name)
  "Within this project PROJ, find the file NAME.
This knows details about or source tree."
  (let ((ans (call-next-method))) ;; using locatedb, etc
    (unless ans
      (let* ((dir (ede-project-root-directory proj))
	     (src (or (oref proj :srcroot) '("")))
	     (tname (expand-file-name name dir)))
	(if (file-exists-p tname)
	    (setq ans tname))
	
	;; Search srcroot
	(while (and (not ans) src)
	  (let* ((tdir (expand-file-name (car src) dir))
		 (tname (expand-file-name name tdir)))
	    (if (file-exists-p tname)
		(setq ans tname)
	      (let ((files (cedet-files-list-recursively tdir name)))
		(when files
		  (setq ans (car files))))))
	  (setq src (cdr src)))
	))
    ans))

;;;###autoload
(ede-add-project-autoload
 (ede-project-autoload "ant"
		       :name "Ant"
		       :file 'ede/ant
		       :proj-file ede-ant-project-file-name
		       :proj-root 'ede-ant-project-root
		       :load-type 'ede-ant-load
		       :class-sym 'ede-ant-project
		       :new-p nil
		       :safe-p t
		       )
 'generic)

(provide 'ede/ant)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "ede/ant"
;; End:

;;; ant.el ends here
