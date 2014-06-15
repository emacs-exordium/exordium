;;; ede/android.el --- Support Android code projects
;;
;; Copyright (C) 2011, 2012, 2013 Eric M. Ludlam
;;
;; Author: Eric M. Ludlam <eric@siege-engine.com>
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
;; Support Android projects via EDE.

(require 'cedet-android)
(require 'ede)
(require 'semantic/analyze)
(require 'sgml-mode)
(require 'nxml-mode nil t)

;;; Code:
(defvar ede-android-project-list nil
  "List of projects created by option `ede-android-project'.")

(defun ede-android-file-existing (dir)
  "Find an Android project in the list of Android projects.
DIR is the directory to search from."
  (let ((projs ede-android-project-list)
	(ans nil))
    (while (and projs (not ans))
      (let ((root (ede-project-root-directory (car projs))))
	(when (string-match (concat "^" (regexp-quote root)) dir)
	  (setq ans (car projs))))
      (setq projs (cdr projs)))
    ans))

(defun ede-android-project-data (dir)
  "Find the Android data for the Android project in DIR.
Return (NAME VERSION PACKAGE)."
  (let ((buff (get-buffer-create " *android-query*")))
    (with-current-buffer buff
      (erase-buffer)
      (setq default-directory (file-name-as-directory dir))
      (insert-file-contents "AndroidManifest.xml" nil)
      (goto-char (point-min))
      (let (version package name)
	(re-search-forward "\\s-*package *= *\"\\([^\"\n]+\\)\"")
	(setq package (match-string 1))
	(condition-case nil
	    (progn
	      (re-search-forward "^ *android:versionName *= *\"\\([.0-9]+\\)\"")
	      (setq version (match-string 1)))
	  (error (setq version "1.0")))
	(re-search-forward "activity\\s-+android:name *= *\"\\([^\"\n]+\\)\"")
	(setq name (match-string 1))
	(prog1
	    (list name version package)
	  (kill-buffer buff)
	  )))))

;;;###autoload
(defun ede-android-load (dir &optional rootproj)
  "Return an Android Project object if there is a match.
Return nil if there isn't one.
Argument DIR is the directory it is created for.
ROOTPROJ is nil, since there is only one project."
  (or (ede-android-file-existing dir)
      ;; Doesn't already exist, so lets make one.
      (let* ((ad (ede-android-project-data dir))
	     (proj (ede-android-project
		    (car ad)
		    :name (car ad)
		    :version (car (cdr ad))
		    :directory (file-name-as-directory dir)
		    :file (expand-file-name "AndroidManifest.xml"
					    dir)
		    :package (car (cdr (cdr ad))))))
	(ede-add-project-to-global-list proj)
	)))

;;;###autoload
(ede-add-project-autoload
 (ede-project-autoload "android"
		       :name "ANDROID ROOT"
		       :file 'ede/android
		       :proj-file "AndroidManifest.xml"
		       :load-type 'ede-android-load
		       :class-sym 'ede-android-project
		       :new-p t
		       :safe-p t))

;;; CLASSES
;;
;; These are the classes for the EDE project system for tracking an android project.
(defclass ede-android-target-misc (ede-target)
  ()
  "EDE Android Project target for Misc files.
All directories with filesshould have at least one target.")

(defclass ede-android-target-java (ede-target)
  ((keybindings :initform (("D" . ede-debug-target)
			   ("E" . ede-android-visit-resource)
			   ))
   (menu :initform
	 (
	  [ "Debug target" ede-debug-target (ede-buffer-belongs-to-target-p) ]
	  [ "Visit Resrouce under Cursor" ede-android-visit-resource ede-object ]
	  ))
   )
  "EDE Android Project target for .java files.")

(defclass ede-android-target-xml (ede-target)
  ((keybindings :initform (("D" . ede-debug-target)
			   ("A" . ede-android-visit-activity)
			   ))
   (menu :initform
	 (
	  [ "Debug target" ede-debug-target (ede-buffer-belongs-to-target-p) ]
	  [ "Visit Activity Under Cursor" ede-android-visit-activity ede-object ]
	  ))
   )
  "EDE Android Project target for .xml files.")

(defclass ede-android-target-misc (ede-target)
  ()
  "EDE Android Project target for misc files.")


;;;###autoload
(defclass ede-android-project (ede-project eieio-instance-tracker)
  ((tracking-symbol :initform 'ede-android-project-list)
   (keybindings :initform (("S" . ede-android-visit-strings)
			   ("U" . ede-android-install)))
   (menu :initform
	 (
	  [ "Upload/Install to Device" ede-android-install ]
	  [ "Start Debug Proxy (DDMS)" cedet-android-start-ddms ]
	  "---"
	  [ "Visit strings.xml" ede-android-visit-strings ]
	  [ "Edit Projectfile" ede-edit-file-target
	    (ede-buffer-belongs-to-project-p) ]
	  "--"
	  [ "Update Version" ede-update-version ede-object ]
	  [ "Version Control Status" ede-vc-project-directory ede-object ]
	  [ "Android Shell" cedet-android-adb-shell ede-object ]
	  [ "Layout Optimizer" ede-android-layoutopt ede-object ]
	;;[ "Edit Project Homepage" ede-edit-web-page
	;;  (and ede-object (oref (ede-toplevel) web-site-file)) ]
	;;[ "Browse Project URL" ede-web-browse-home
	;;  (and ede-object
	;;	 (not (string= "" (oref (ede-toplevel) web-site-url)))) ]
	  "--"
	  [ "Rescan Project Files" ede-rescan-toplevel t ]
	  ))
   (package :initarg :package
	    :initform "com"
	    :type string
	    :documentation "The package extracted from the Manifest.")
   )
  "Project for Android applications.")

(defmethod initialize-instance ((this ede-android-project)
				&rest fields)
  "Make sure the targets slot is bound."
  (call-next-method)
  (unless (slot-boundp this 'targets)
    ;; @TODO - All android projects are the same, so we can probably
    ;; prepopulate this whole thing right off.
    (oset this :targets nil))
  ;; In case the defaults change, force the known configurations
  ;; of android to be setup here.
  (oset this configurations '("debug" "release" "instrument"))
  (oset this configuration-default "debug")
  ;; If our manifest file doesn't exist, then the user has called
  ;; ede-new, and we need to call android to fill in our template directory.
  (when (not (file-exists-p (oref this :file)))
    (if (y-or-n-p (format "No AndroidManifest.xml file exists in %s.  Create?"
			  (file-name-directory (oref this :file))))
	(call-interactively 'cedet-android-create-project)
      ;; Else, possible problems
      (message "You may run into problems in this project if there is no manifest file.")))
  )

(defmethod ede-commit-project ((proj ede-android-project))
  "Commit any change to the Android project PROJ to its file."
  ;; TODO - Can I save other suff, like version numbers?
  )

(defmethod project-rescan ((this ede-android-project))
  "Rescan the EDE proj project THIS."
  (let ((scan (ede-android-project-data (file-name-directory (oref this file)))))
    (oset this name (car scan))
    (oset this version (car (cdr scan)))
    (oset this package (car (cdr (cdr scan))))))

;;; File Stuff
;;
;; Android projects have a root and one project, but no sub projects.
;; These hacks allow this to work quickly and simply.
(defmethod ede-project-root-directory ((this ede-android-project)
				       &optional file)
  "Return the root for THIS Android project with file."
  (file-name-directory (oref this file)))

(defmethod ede-project-root ((this ede-android-project))
  "Return my root."
  this)

(defmethod ede-find-subproject-for-directory ((proj ede-android-project)
					      dir)
  "Return PROJ, for handling all subdirs below DIR."
  proj)

;;; TARGET MANAGEMENT
;;
(defun ede-android-find-matching-target (class dir targets)
  "Find a target that is a CLASS and is in DIR in the list of TARGETS."
  (let ((match nil))
    (dolist (T targets)
      (when (and (object-of-class-p T class)
		 (string= (oref T :path) dir))
	(setq match T)
      ))
    match))

(defmethod ede-find-target ((proj ede-android-project) buffer)
  "Find an EDE target in PROJ for BUFFER.
If one doesn't exist, create a new one for this directory."
  (let* ((ext (file-name-extension (buffer-file-name buffer)))
	 (cls (cond ((not ext)
		     'ede-android-target-misc)
		    ((string-match "java" ext)
		     'ede-android-target-java)
		    ((string-match "xml" ext)
		     'ede-android-target-xml)
		    (t 'ede-android-target-misc)))
	 (targets (oref proj targets))
	 (dir default-directory)
	 (ans (ede-android-find-matching-target cls dir targets))
	 )
    (when (not ans)
      (setq ans (make-instance 
		 cls 
		 :name (file-name-nondirectory
			(directory-file-name dir))
		 :path dir
		 :source nil))
      (object-add-to-list proj :targets ans)
      )
    ans))

;;; Include paths
;;
(defmethod ede-system-include-path ((this ede-android-target-java))
  "Get the system include path used by target THIS."
  ;; Get android.jar, and add it.  but how??
  nil)

(defun ede-android-fname-if-exists (name)
  "Return the file NAME if it exists as a file."
  (if (file-exists-p name) name))

(defmethod ede-expand-filename-impl ((proj ede-android-project) name)
  "Within this android project, expand filename NAME."
  (let ((ans (call-next-method)) ; locate feature
	)
    (unless ans
      (let ((pr (ede-project-root-directory proj))
	    (ext (file-name-extension name)))
	(setq ans
	      (or
	       (ede-android-fname-if-exists (expand-file-name name))
	       (when (string= ext "java")
		 (or
		  (ede-android-fname-if-exists (expand-file-name name (expand-file-name "src" pr)))
		  (ede-android-fname-if-exists (expand-file-name name (expand-file-name "gen" pr)))
		  ;; @TODO Look in all subdirs of src and gen if not fully qualified.
		  ))
	       (when (string= ext "xml")
		 (or
		  (ede-android-fname-if-exists (expand-file-name name (expand-file-name "res" pr)))
		  nil
		  ;; @TODO Look in all subdirs of res if not fully qualified.
		  ))
	       (when (not ext)
		 ;; No extension, perhaps a directory substruction??
		 ;; Lets expand it as if a java package name.
		 (or
		  (ede-android-fname-if-exists (expand-file-name name (expand-file-name "src" pr)))
		  (ede-android-fname-if-exists (expand-file-name name (expand-file-name "gen" pr))))
		 )
	       ))
	))
    ans))

(defmethod ede-source-paths ((proj ede-android-project) mode)
  "Get the base to all source trees in the current projet for MODE.
For java, this is both src and gen.  For xml, it is just res."
  (let ((pr (ede-project-root-directory proj)))
    (cond ((eq mode 'java-mode)
	   (list
	    (ede-android-fname-if-exists (expand-file-name "src" pr))
	    (ede-android-fname-if-exists (expand-file-name "gen" pr))))
	  ((or (eq mode 'nxml-mode) ;; emacs 23
	       (and (eq mode 'sgml-mode) sgml-xml-mode)) ;; emacs 22
	   (list
	    (ede-android-fname-if-exists (expand-file-name "res" pr))))
	  (t nil))))

(defmethod ede-java-classpath ((this ede-android-project))
  "Return the classpath for this project.
For Android projects, look to the SDK android.jar."
  ;; @TODO - does the local project get some libs or jars or something?
  (list (cedet-android-sdk-jar)))

;;; Extra Compile Commands
;;
(defun ede-android-install ()
  "Compile the current project, and install the result to a device.
Uses an active configuration and adds the INSTALL target."
  (interactive)
  (project-compile-project (ede-current-project) " install"))

;;; Basic Compile/Debug Commands
;;
(defmethod project-compile-project ((proj ede-android-project) &optional command)
  "Compile the Android project with ant.
Argument COMMAND is the command to use when compiling."
  (let ((default-directory (ede-project-root-directory proj)))
    (compile (concat "ant clean " (oref proj configuration-default)
		     (or command "")))))
  
(defmethod project-compile-target ((proj ede-android-target-java) &optional command)
  "Compile the current Android java target with ant on the project.
Argument COMMAND is the command to use when compiling."
  (project-compile-project (ede-current-project) command))
  
(defmethod project-compile-target ((proj ede-android-target-xml) &optional command)
  "Compile the current Android java target with ant on the project.
Argument COMMAND is the command to use when compiling."
  (project-compile-project (ede-current-project) command))

(defmethod project-debug-target ((targ ede-android-target-java))
  "Start debugging the current project."
  (ede-android-debug-project (ede-project-root-directory (ede-current-project))))

(defmethod project-debug-target ((targ ede-android-target-xml))
  "Start debugging the current project."
  (ede-android-debug-project (ede-project-root-directory (ede-current-project))))

(defvar android-jdb-port-history)
(declare-function android-jdb "android")
(defun ede-android-debug-project (startdir)
  "Start the android JDB debugger in a buffer.
STARTDIR is the directory to start jdb in.
Depends on `android.el' that comes with the SDK to get going."
  ;; Step one, make sure ddms is running.
  (when (not (cedet-android-ddms-active-p))
    (if (y-or-n-p "No DDMS process running in Emacs.  Start it? ")
	(progn
	  (cedet-android-start-ddms)
	  ;; Give it a little time.
	  (message "Starting DDMS ...")
	  (sit-for 10))
      (when (not (y-or-n-p "Start Debugger anyway? " ))
	  (signal 'quit nil))))
  ;; Step two, start jdb.
  (require 'android) ;; comes with SDK.
  ;; @TODO - the port should be selectable.
  (android-jdb (car android-jdb-port-history) startdir))

(defun ede-android-layoutopt ()
  "Run the layoutopt command on this project."
  (interactive)
  (cedet-android-layoutopt (ede-project-root-directory (ede-current-project))))

;;; XML Buffer
;;
(condition-case nil
    (require 'nxml-mode)
  ;; No nxml?  Use sgml instead.
  (error (require 'sgml-mode)))

(defun ede-android-xml-looking-at-elt-p (elt)
  "Return non-nil if POINT is in front of the element ELT.
ELT is a string representing the text right after the < of an element tag."
  (looking-at (concat "<" (regexp-quote elt) "\\>")))

(defun ede-android-find-element-above-cursor (elt)
  "Place the cursor on the element ELT that is at, or above the cursor.
Moves the cursor upward through nested elements until ELT is found, or
an error occurs."
  (while (not (ede-android-xml-looking-at-elt-p elt))
    (if (eq major-mode 'nxml-mode)
	(nxml-backward-up-element 1)
      (sgml-skip-tag-backward 1))))

(defun ede-android-find-attribute (attr)
  "Find the attribute ATTR in the element immediatly in front of the cursor.
Places the cursor on the = for that attribute, and returns the value."
  (let ((end (save-excursion (forward-sexp 1) (point))))
    (re-search-forward (concat "\\<" (regexp-quote attr) "\\s-*=") end)
    (looking-at "\\s-*\"\\([^\n\"]+\\)\"")
    (match-string 1)))

(defun ede-android-name-to-java (name)
  "Convert the object NAME into a java file in this project."
  (let* ((package (oref ede-object-project package))
	 (root (ede-project-root-directory ede-object-project))
	 (s (split-string package "\\." t))
	 (dir (mapconcat 'identity s "/"))
	 (fdir (expand-file-name dir (expand-file-name "src" root))))
    (expand-file-name (concat name ".java") fdir)))

(defun ede-android-visit-activity ()
  "Visit the activity specified in the XML file under point."
  (interactive)
  (let ((activityname nil))
    (save-excursion
      (ede-android-find-element-above-cursor "activity")
      ;; Record the activity
      (setq activityname (ede-android-find-attribute "android:name")))
    ;; Do something.
    (find-file (ede-android-name-to-java activityname))
    ))

;;; Java buffer features
;;

(defun ede-android-resource-file (where what)
  "Find the file associated with the resource in WHERE that is WHAT.
WHERE is something like menu or layout, and what is the name of the resource."
  (let* ((root (ede-project-root-directory ede-object-project))
	 (fdir (expand-file-name where (expand-file-name "res" root))))
    (expand-file-name (concat what ".xml") fdir)))

(defun ede-android-visit-resource ()
  "Visit the resource being referenced by R under point."
  (interactive)
  (let* ((p (semantic-ctxt-current-symbol)))
    (unless (string= (car p) "R")
      (error "Point is not on an XML based resource referred to by R"))
    (setq p (cdr p))
    ;; The next part refers to where in the resource hierarcy it is.
    (let ((fname (ede-android-resource-file (car p) (car (cdr p)))))
      (if (file-exists-p fname)
	  (find-file fname)
	(error "Calculated filename %s does not exist" fname)))))

(defun ede-android-visit-strings ()
  "Visit the strings resource for the current project."
  (interactive)
  (find-file (ede-android-resource-file "values" "strings")))

(provide 'ede/android)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "ede/android"
;; End:

;;; ede-android.el ends here
