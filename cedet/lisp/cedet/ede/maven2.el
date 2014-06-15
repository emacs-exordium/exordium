;; Copyright (C) 2008, 2009, 2012, 2013 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; Joakim Verona <joakim@verona.se>

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

;; Notes:

;; - Maven2 is very different from Maven1, so this file wont be
;; useful for Maven1 projects.

;; - Currently the maven2 ede support enables you to open a file in a
;;  maven2 project and build the project with "mvn install".

;; Introduction:

;; Projects that build with Maven2 have a pom.xml file that defines
;; the project.  Maven projects are used for Java projects, and have
;; many differences from Make projects. (Ant is another Java build
;; tool that is more like a Make project.)  With Maven2 you describe a
;; projects dependencies explicitly, and other things like which files
;; to compile is normaly deducted implicitly.

;; Maven2 projects can consist of any file types, but mostly java, xml
;; files(with different file name extensions), and Java property
;; files.

;; Maven2 + Emacs is a good combination, because team members can
;; choose to use Eclipse + Maven2, or Emacs + Maven2, and both can use
;; the workflow their comfortable with.


;; Implementation discussion:

;; Its not totaly clear how ede:s model map on maven2 projects, OTOH,
;; the basic useful functionality is simple:
;; - Identify the project root; the directory where the pom.xml file is
;; - Find source: <proj root>/src/<main|test>/<java|whatever/...
;; - Execute "mvn install" in a project root, to build the project


;;TODO

;;
;; BUG: (this bug-description is somewhat based on a missunderstanding
;; of EDE, FIXME) in projects with a root pom and modules with poms
;; residing in child directories of the root pom, "compile" sometimes
;; build the root project rather than the child project. This is not
;; what we want for maven.  maybe its a feature for makefile projects?

;;to reproduce:
;; - use a hierarchical maven project:
;;root/pom.xml
;;root/A/pom.xml
;;root/B/pom.xml
;; - open root/A/pom.xml, and build with ede. it will build the right project
;; - open root/pom.xml and build
;; - open root/B/pom.xml and ede build. it will build root/pom.xml instead!

;;project-compile-project seems to get the wrong directory to build
;;because ede-compile-project seems to travel up through a project
;;hierarchy and build the root project, rather than the current one.

;;should  ede-parent-project be made generic?
;; TODO: yes! in this case, it will possible to use information from parent tag in pom.xml

;; In the end more things are desired from proper emacs maven support.

;; - Create new maven2 projects with maven2 from archetypes

;; - Handle maven modules.

;; - make cedet know about the maven project source and class paths.

;; - handle maven build profiles.

;; - ede targets == maven goals ?
;; if so, maven projects will have a "install" goal to start with.
;; all files below the maven src dir, will belong to this target.
;; (this will be true also if we add more "targets" like mavens, "compile" goal.)
;; all src files will belong to all maven targets.

;; - an auxilary project file, like ede-simple could be an useful option

;; - ede configurations == maven profiles

(require 'ede/jvm-base)

;;; Code:

(defgroup ede-maven2 nil
  "Emacs Development Environment. Maven2 options"
  :group 'ede
  :group 'tools
  :group 'extensions)

(defcustom ede-maven2-execute-mvn-to-get-classpath t
  "Defines, should we execute Maven to get classpath information or not."
  :group 'ede-maven2
  :require 'ede/maven2
  :type 'boolean)

(defcustom ede-maven2-maven-command "mvn"
  "Executabe, that will be executed as maven"
  :group 'ede-maven2
  :require  'ede/maven2
  :type 'string)

(defcustom ede-maven2-maven-options '("-B")
  "Maven's command line options"
  :group 'ede-maven2
  :require  'ede/maven2
  :type 'list)

(defcustom ede-maven2-deps-plugin-options '("-DincludeTypes=jar")
  "Options for Maven's dependency plugin that is used to extract the classpath for project"
  :group 'ede-maven2
  :require  'ede/maven2
  :type 'list)

;; Because there is one root project file, and no sub project files,
;; we need a special root-finding function.

;;;###autoload
(defun ede-maven2-project-root (&optional dir)
  "Get the root directory for DIR."
  (ede-find-project-root "pom.xml" dir))

(defvar ede-maven2-project-list nil
  "List of projects created by option `ede-maven2'.")

;;;###autoload
(defun ede-maven2-load (dir &optional rootproj)
  "Return a Maven Project object if there is a match.
Return nil if there isn't one.
Argument DIR is the directory it is created for.
ROOTPROJ is nil, since there is only one project."
  (or (ede-files-find-existing dir ede-maven2-project-list)
      ;; Doesn't already exist, so lets make one.
       (let ((this
             (ede-maven2-project "Maven"
                                 :name "maven dir" ; TODO: make fancy name from dir here.
                                 :directory dir
                                 :file (expand-file-name "pom.xml" dir)
				 :current-target "package"
                                 )))
         (ede-add-project-to-global-list this)
         ;; TODO: the above seems to be done somewhere else, maybe ede-load-project-file
         ;; this seems to lead to multiple copies of project objects in ede-projects
	 ;; TODO: call rescan project to setup all data
	 this)))

;;;###autoload
(defclass ede-maven2-project (ede-jvm-base-project eieio-instance-tracker)
  ((tracking-symbol :initform 'ede-maven2-project-list)
   (file-header-line :initform ";; EDE Maven2 project wrapper")
   (pom :initform nil
	:initarg :pom
	:documentation "Parsed pom.xml file")
   )
  "Project Type for Maven2 based Java projects."
  :method-invocation-order :depth-first)

(defmethod initialize-instance ((this ede-maven2-project)
                                &rest fields)
  "Make sure the all targets as setup."
  (call-next-method)
  (ede-normalize-file/directory this "pom.xml")
  ;; TODO: add analysis of pom.xml
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;the 2 compile methods below currently do much the same thing.
;;  - 1st one tries to find the "root project" and compile it
;;  - 2nd compiles the child project the current file is a member of
;;maven error messages are recognized by emacs23

(defmethod project-compile-project ((proj ede-maven2-project) &optional command)
  "Compile the entire current project PROJ.
Argument COMMAND is the command to use when compiling."
  ;; we need to be in the proj root dir for this to work
  (let ((default-directory (ede-project-root-directory proj)))
    (compile (combine-and-quote-strings
	      (append (list ede-maven2-maven-command)
		      ede-maven2-maven-options
		      (list (oref proj :current-target))
		      (oref proj :target-options))))))

;;; Classpath-related...
(defconst maven2-outfile-name "mvn-classpath")

(defmethod ede-java-classpath ((proj ede-maven2-project))
  "Get classpath for maven project"
  (ede-jvm-get-classpath-from-command proj ede-maven2-execute-mvn-to-get-classpath
				      maven2-outfile-name ede-maven2-maven-command
				      (append `(,nil ,nil ,nil "--batch-mode" "dependency:build-classpath"
						     ,(concat "-Dmdep.outputFile=" maven2-outfile-name))
					      ede-maven2-deps-plugin-options)))

;; TODO: really should be based on content of pom.xml file. But we need parser for it...
;; TODO: add caching...
(defmethod ede-source-paths ((proj ede-maven2-project) mode)
  "Get the base to all source trees in the current project for MODE."
  (let ((dir (ede-project-root-directory proj)))
    (mapcar (lambda (x) (concat dir x))
	    (cond
	     ((eq mode 'java-mode) '("src/main/java" "src/test/java"))
	     ((eq mode 'clojure-mode) '("src/main/clojure" "src/test/clojure"))))))

;; TODO: re-implement when pom.xml parser will be available
(defmethod project-rescan ((proj ede-maven2-project))
  "Rescan the EDE proj project THIS."
  (when (ede-jvm-base-file-updated-p proj)
    ;; TODO: fill information
    (oset proj :pom nil)
    ))

;;; UTILITIES SUPPORT.
;;

;;;###autoload
(ede-add-project-autoload
 (ede-project-autoload "maven2"
		       :name "MAVEN2"
		       :file 'ede/maven2
		       :proj-file "pom.xml"
		       :proj-root 'ede-maven2-project-root
		       :load-type 'ede-maven2-load
		       :class-sym 'ede-maven2-project
		       :new-p nil
		       :safe-p t
		       )
 'generic)

(provide 'ede/maven2)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "ede/maven2"
;; End:

;;; ede/maven2.el ends here

