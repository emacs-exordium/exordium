;;; cedet-build.el --- Build CEDET within Emacs.

;; Copyright (C) 2008, 2009, 2012, 2013 Eric M. Ludlam

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
;; Build all the CEDET parts interactively through EDE.
;;
;; NOTE: This does not support XEmacs, which cannot use
;;       `batch-update-autoloads' in interactive mode.
;;
;;; USAGE:
;;
;;     emacs -Q -l cedet-build.el -f cedet-build
;;     
;;       or, if -Q isn't supported
;;
;;     emacs -q --no-site-file -l cedet-build.el -f cedet-build


;;; Code:
(defvar cedet-build-location
  (let ((dir (file-name-directory
	      (or load-file-name (buffer-file-name)))))
    ;; (add-to-list 'load-path dir)
    dir)
  "Root of the CEDET tree.")

(defun cedet-build-in-default-emacs()
  "Build CEDET in a new Emacs instance started with -Q."
  (interactive)
  (let ((default-directory cedet-build-location))
    (call-process (expand-file-name invocation-name invocation-directory)
		  nil 0 nil
                  "-Q" "-l" "cedet-build.el" "-f" "cedet-build")
    (message "Started new Emacs instance to build CEDET ...")))

(defun cedet-build-in-this-emacs ()
  "Build CEDET in this version of Emacs.
This only works if EIEIO does not need to be compiled."
  (interactive)
  (let ((src "eieio/eieio.el") (dst "eieio/eieio.elc"))
    (if (file-newer-than-file-p src dst)
	(when (y-or-n-p "EIEIO needs to be recompiled.  Use subprocess? ")
	  (cedet-build-in-default-emacs))
      (cedet-build t))))

(defun cedet-build-msg (fmt &rest args)
  "Show a build message."
  (if noninteractive
      (princ (apply 'format fmt args) t)
    (switch-to-buffer "*CEDET BYTECOMPILE*" t)
    (goto-char (point-max))
    (insert (apply 'format fmt args))
    (delete-other-windows)
    (sit-for 0)))

(defun cedet-build-autoloads-for-dir (basedir &rest dirs)
  "Create loaddefs.el for basedir, including all DIRS in the loaddefs."
  (save-excursion
    (let* ((default-directory (expand-file-name basedir))
	   (genfile (expand-file-name "loaddefs.el"))
	   (load-path (cons default-directory load-path)))
      ;; Delete it if it is there.  There is a bug in updating
      ;; an existing file.
      (when (file-exists-p genfile) (delete-file genfile))
      (apply 'call-process
	     (expand-file-name invocation-name invocation-directory)
	     nil nil nil
	     "-Q" "-batch"
	     "-L" default-directory
	     "--eval" (concat "(setq generated-autoload-file \""
			      genfile
			      "\")")
	     "-f" "batch-update-autoloads" dirs)
      )))

(defun cedet-build (&optional override-check)
  "Build CEDET via EDE.
OVERRIDE-CHECK to override cedet short-cicuit."
  (setq inhibit-splash-screen t)

  ;; Make sure CEDET is not loaded
  (if (and (not override-check) (featurep 'cedet))
      (error "To use cedet-build, start Emacs with -q"))

  ;; Setup a logging buffer
  (switch-to-buffer "*CEDET BYTECOMPILE*")
  (delete-other-windows)
  (erase-buffer)
  (cedet-build-msg "CEDET BYTE COMPILATION STATUS:\n\n")
  (cedet-build-msg "Step 1: Byte compile EIEIO...")

  ;; Get EIEIO built first.
  (save-excursion
    (let ((src "lisp/eieio/eieio.el") (dst "lisp/eieio/eieio.elc")
	  (core "lisp/eieio/eieio-core.el") (coredst "lisp/eieio/eieio-core.elc"))
      (if (file-newer-than-file-p core coredst)
	  (progn
	    (when (featurep 'eieio-core)
	      (error "You should not recompile EIEIO after it has been loaded"))
	    (byte-compile-file core)
	    (load-file coredst)
	    (cedet-build-msg "(core) done ..."))
	(cedet-build-msg "(core) not needed...")
	(load-file coredst))

      (if (file-newer-than-file-p src dst)
	  (progn
	    (when (featurep 'eieio)
	      (error "You should not recompile EIEIO after it has been loaded"))
	    (byte-compile-file src)
	    (cedet-build-msg "(eieio) done\n"))
	(cedet-build-msg "(eieio) not needed\n"))
      ))

  ;; Get eieio loaddefs
  (cedet-build-msg "Step 2: Creating autoloads ...\n")
  (cedet-build-msg "Step 2.1: EIEIO Autoloads...")
  (cedet-build-autoloads-for-dir "lisp/eieio/" ".")
  (cedet-build-msg "done.\n")

  ;; Get core CEDET autoloads built...
  (cedet-build-msg "Step 2.2: CEDET Autoloads...")
  (cedet-build-autoloads-for-dir "lisp/cedet/" ".")
  (cedet-build-msg "done.\n")

  ;; Get EDE autoloads built...
  (cedet-build-msg "Step 2.3: EDE Autoloads...")
  (cedet-build-autoloads-for-dir "lisp/cedet/ede/" ".")
  (cedet-build-msg "done.\n")

  ;; Get Semantic autoloads built...
  (cedet-build-msg "Step 2.4: Semantic Autoloads...")
  (cedet-build-autoloads-for-dir "lisp/cedet/semantic/" "." "./bovine" "./wisent" "./analyze" "./decorate" "./ectags" "./symref")
  (cedet-build-msg "done.\n")

  ;; Get SRecode autoloads built...
  (cedet-build-msg "Step 2.5: SRecode Autoloads...")
  (cedet-build-autoloads-for-dir "lisp/cedet/srecode/" ".")
  (cedet-build-msg "done.\n")

  ;; Get Cogre autoloads built...
  (cedet-build-msg "Step 2.6: COGRE Autoloads...")
  (cedet-build-autoloads-for-dir "lisp/cedet/cogre/" ".")
  (cedet-build-msg "done.\n")

  ;; Speedbar
  (cedet-build-msg "Step 2.7: Speedbar Autoloads...")
  (cedet-build-autoloads-for-dir "lisp/speedbar/" ".")
  (cedet-build-msg "done.\n")

  ;; Fire up CEDET and EDE
  (cedet-build-msg "Step 3: initialize CEDET from external repository ...")
  
  (setq cedet-minimum-setup t)
  (load-file (expand-file-name "cedet-devel-load.el" cedet-build-location))

  (cedet-build-msg "done\nStep 4: Turning on EDE and Semantic ...")
  (save-excursion
    ;; Disable saving EDE's cache file.
    (setq ede-project-placeholder-cache-file nil)
    ;; Enable EDE
    (global-ede-mode 1)
    ;; Set srecode-map-load-path to nil, otherwise the setter function
    ;; for it will break the build.
    (setq srecode-map-load-path nil)
    ;; Disable all saves from SRecode
    (setq srecode-map-save-file nil)
    ;;Disable most new buffer setup functions to speed things up.
    (setq semantic-new-buffer-setup-functions nil)
    ;; Disable using cached files for parse results.
    (setq-default semanticdb-new-database-class 'semanticdb-project-database)    
    ;; Enable Semantic
    (semantic-mode 1)
    ;; Require grammar compilation.
    (require 'semantic/ede-grammar)
    (require 'semantic/wisent))
  (cedet-build-msg "done.\n\n")

  ;; Load in the Makefile
  (let ((buf (get-buffer-create "CEDET MAKE"))
	(pkgs nil)
	(subdirs '("lisp"))
	)
    (cedet-build-msg "Step 5: Build Targets in: ")
    (cedet-build-msg "%S\n\n" subdirs)

    (cedet-build-msg "Build Emacs Lisp Targets:\n-------------------------\n")
    (dolist (d subdirs)
      ;; For each directory, get the project, and then targets
      ;; and run a build on them.
      (cedet-build-msg "Building directory %s\n" d)

      (let* ((ede-project-directories t)
	     (Tproj (ede-load-project-file (file-name-as-directory
					    (expand-file-name
					     d cedet-build-location))))
	     )
	(cedet-build-project Tproj))
      (cedet-build-msg "\n\nDone.\n")
      )))

(defun cedet-build-project (Tproj)
  "Build the project TPROJ.  Recurse into sub projects."
  (cedet-build-msg "  Project: %s\n" (object-name-string Tproj))
  (dolist (targ (oref Tproj targets))
    (when (and (or (ede-proj-target-elisp-p targ)
		   ;;(ede-proj-target-elisp-autoloads-p targ)
		   (semantic-ede-proj-target-grammar-p targ))
	       (condition-case nil
		   (oref targ :partofall)
		 (error nil)))
      
      (let ((ns (object-name-string targ)))
	(cedet-build-msg "   Target %s...%s" ns
			 (make-string (- 20 (length ns)) ? )))
      
      ;; If it is an elisp target, then do that work here.
      (let ((ans (save-excursion
		   (project-compile-target targ))))
	(switch-to-buffer "*CEDET BYTECOMPILE*")
	(delete-other-windows)
	(redisplay)
	(if (and (consp ans)
		 (numberp (car ans)))
	    (cedet-build-msg "%d compiled, %d up to date.\n"
			     (car ans) (cdr ans))
	  (cedet-build-msg "done.\n"))
	)) )

  ;; Recurse, and build all the sub projects.
  (dolist (proj (oref Tproj subproj))
    (cedet-build-project proj)))


(provide 'cedet-build)
;;; cedet-build.el ends here
