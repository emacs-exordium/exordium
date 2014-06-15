;;; cedet-android.el --- Misc android external application support
;;
;; Copyright (C) 2011, 2013 Eric M. Ludlam
;;
;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id$
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
;; Support for Android development external applications.
;; Android shell command stuff.
;;  ADB - Android Debug Bridge.

;;; Code:

(require 'inversion)

(eval-when-compile
  (require 'cedet-java))

(defvar cedet-android-sdk-adb-min-version "1.0.26"
  "Minimum version of the Android SDB ADB program required.")

(defcustom cedet-android-sdk-root "~/src/android-sdk-linux/"
  "The root to the android SDK."
  :group 'android
  :type 'file)

(defvar cedet-android-sdk-adb (expand-file-name "platform-tools/adb" cedet-android-sdk-root)
  "Location of the android debug bridge program (adb).)")

(defvar cedet-android-sdk-android (expand-file-name "tools/android" cedet-android-sdk-root)
  "Location of the android sdk program.")

(defvar cedet-android-sdk-layoutopt (expand-file-name "tools/layoutopt" cedet-android-sdk-root)
  "Location of the android layoutopt program.")

(defvar cedet-android-sdk-ddms (expand-file-name "tools/ddms" cedet-android-sdk-root)
  "Location of the android layoutopt program.")

;;; ANDRIOD project program support
;;
;;;###autoload
(defun cedet-android-create-project (name package target &optional dir)
  "Create an android project with NAME.
Your activity class will be created in the java PACKAGE.
You need to specify a TARGET, which is a number specifying the desired type
of package you intend to build.
Create the project in optional DIR, or in the default directory if not specified.
NAME will be used as the name of the project."
  (interactive
   (list (read-string "Android Project and Activity Name: ")
	 (read-string "Java Package: ")
	 (progn
	   (cedet-android-target-list)
	   (read-number "Target id: "))))
  (if (or dir
	  (y-or-n-p (format "Create project in: %s? " default-directory)))
      (cedet-android-android-show-output
       (list 
	"create" "project" 
	"--target" (if (numberp target) (number-to-string target) target)
	"--name" name "--path" (expand-file-name (or dir default-directory))
	"--activity" (concat "A" name)
	"--package" package))))

;;;###autoload
(defun cedet-android-target-list ()
  "Get the list of available targets for an android environment."
  (interactive)
  (cedet-android-android-show-output '("list" "targets")))

(defun cedet-android-android-show-output (flags)
  "Show output from some Android call with FLAGS."
  (let ((b (condition-case nil
	       (cedet-android-android-call flags)
	     (error nil))))
    (switch-to-buffer b)
    (goto-char (point-min))))

(defun cedet-android-android-call (flags)
  "Call Android with the list of FLAGS."
  (let ((b (get-buffer-create "*Android SDK android*"))
	(cd default-directory)
	)
    (with-current-buffer b
      (setq default-directory cd)
      (erase-buffer))
    (apply 'call-process cedet-android-sdk-android
	   nil b nil
	   flags)
    b))

;;; Layoutopt
;;
;;;###autoload
(defun cedet-android-layoutopt (projectroot)
  "Get the list of available targets for an android environment.
Argument PROJECTROOT is the directory root of some project to be optimized."
  (interactive (list "."))
  (cedet-android-layoutopt-show-output (list (or projectroot "."))))

(defun cedet-android-layoutopt-show-output (flags)
  "Show output from some Android layoutopt call with FLAGS."
  (let ((b (condition-case nil
	       (cedet-android-layoutopt-call flags)
	     (error nil))))
    (switch-to-buffer b)
    (goto-char (point-min))))

(defun cedet-android-layoutopt-call (flags)
  "Call Android layoutopt with the list of FLAGS."
  (let ((b (get-buffer-create "*Android SDK layoutopt*"))
	(cd default-directory)
	)
    (with-current-buffer b
      (setq default-directory cd)
      (erase-buffer))
    (apply 'call-process cedet-android-sdk-layoutopt
	   nil b nil
	   flags)
    b))

;;; Layoutopt
;;
;;;###autoload
(defun cedet-android-start-ddms ()
  "Start Android's ddms debugging proxy."
  (interactive)
  (cedet-android-ddms-call nil))

(defun cedet-android-ddms-call (flags)
  "Call Android ddms with the list of FLAGS."
  (let ((b (get-buffer-create "*Android SDK ddms*"))
	(cd default-directory)
	)
    (with-current-buffer b
      (setq default-directory cd)
      (erase-buffer))
    (apply 'start-process "ddms" b cedet-android-sdk-ddms
	   flags)
    b))

(defun cedet-android-ddms-active-p ()
  "Return non-nil if ddms is active in a ddms buffer."
  (let ((b (get-buffer "*Android SDK ddms*"))
	(p nil))
    (if (not b)
	nil
      (setq p (get-buffer-process b))
      ;; If there is a running process, return t
      (and p (eq (process-status p) 'run)))))


;;; ADB support
;;
;;;###autoload
(defun cedet-android-adb-help ()
  "Get help for Android Debug Bridge."
  (interactive)
  (cedet-android-adb-show-output (list "help")))

;;;###autoload
(defun cedet-android-adb-devices ()
  "The the list of attached devices from Android Debug Bridge."
  (interactive)
  (cedet-android-adb-show-output (list "devices")))

(defun cedet-android-adb-start-server ()
  "Start the Android Debug Bridge server."
  (interactive)
  (cedet-android-adb-show-output (list "start-server")))

(defun cedet-android-adb-kill-server ()
  "Kill the Android Debug Bridge server."
  (interactive)
  (cedet-android-adb-show-output (list "kill-server")))

(defun cedet-android-adb-show-output (flags)
  "Show output from some Android Debug Bridge FLAGS settings."
  (let ((b (condition-case nil
	       (cedet-android-adb-call flags)
	     (error nil))))
    (switch-to-buffer b)
    (goto-char (point-min))))

(defun cedet-android-adb-call (flags)
  "Call Android Debug Bridge with the list of FLAGS."
  (let ((b (get-buffer-create "*Android SDK ADB*"))
	(cd default-directory)
	)
    (with-current-buffer b
      (setq default-directory cd)
      (erase-buffer))
    (apply 'call-process cedet-android-sdk-adb
	   nil b nil
	   flags)
    b))

;;;###autoload
(defun cedet-android-adb-version-check (&optional noerror)
  "Check the version of the installed Android ADB command.
If optional programatic argument NOERROR is non-nil, then
instead of throwing an error if Global isn't available, then
return nil."
  (interactive)
  (let ((b (condition-case nil
	       (cedet-android-adb-call (list "version"))
	     (error nil)))
	(rev nil))
    (if (not b)
	(progn
	  (when (called-interactively-p 'interactive)
	    (message "adb not found."))
	  nil)
      (with-current-buffer b
	(goto-char (point-min))
	(re-search-forward "Android Debug Bridge Version \\([0-9.]+\\)" nil t)
	(setq rev (match-string 1))
	(if (inversion-check-version rev nil cedet-android-sdk-adb-min-version)
	    (if noerror
		nil
	      (error "Version of Android Debug Bridge is %s.  Need at least %s"
		     rev cedet-android-sdk-adb-min-version))
	  ;; Else, return TRUE, as in good enough.
	  (when (called-interactively-p 'interactive)
	    (message "Android Debug Bridge %s  - Good enough." rev))
	  t)))))

;;; Android Shell
;;
;; Use a classic shell buffer to control your device.

(declare-function shell-mode "shell")
(declare-function comint-check-proc "comint")

;;;###autoload
(defun cedet-android-adb-shell ()
  "Create an inferior shell for Android Debug Bridge."
  (interactive)
  (require 'shell)
  (require 'comint)
  (let ((buffer (get-buffer-create "*Android Debug Bridge*")))
    ;; Pop to buffer, so that the buffer's window will be correctly set
    ;; when we call comint (so that comint sets the COLUMNS env var properly).
    (pop-to-buffer buffer)
    (unless (comint-check-proc buffer)
      (apply 'make-comint-in-buffer "adb" buffer cedet-android-sdk-adb
	     nil '("shell"))
      (shell-mode))
    buffer))

;;; Examples in SDK
;;
(defcustom cedet-android-current-version "8"
  "The preferred android version when looking up sample code.
This is the number used in directory names, like android-8, which is android version 2.2."
  :group 'android
  :type 'string)

(defun cedet-android-get-samples-alist ()
  "Get a list of sample projects from the Android SDK."
  (let ((files (directory-files
		(expand-file-name (concat "samples/android-" cedet-android-current-version)
				  cedet-android-sdk-root)
		t "^[A-Za-z][A-Za-z0-9]+$"))
	(out nil))
    (dolist (F files)
      (push (cons (file-name-nondirectory F) F) out))
    (nreverse out)))
    
(defun cedet-android-visit-sample (sample)
  "Visit SAMPLE code in the Android SDK samples directory."
  (interactive (list (completing-read "Sample: " (cedet-android-get-samples-alist)
				      nil t "ApiDemos")))
  (let ((ff (assoc sample (cedet-android-get-samples-alist))))
    (find-file (expand-file-name "AndroidManifest.xml" (cdr ff)))))

;;; The platform JAR file
;;
(defun cedet-android-sdk-jar ()
  "Get a filename to the .jar being developed."
  (let ((platform (concat "platforms/android-" cedet-android-current-version "/"))
	(jar "android.jar"))
    (expand-file-name jar (expand-file-name platform cedet-android-sdk-root))))

;;;###autoload
(defun cedet-android-sdk-update-classpath ()
  "Update the classpath for `cedet-java' to include the android compile-time libraries."
  (interactive)
  (require 'cedet-java)
  (let ((aj (cedet-android-sdk-jar)))
    (add-to-list 'cedet-java-classpath-extension aj t)))
	  

(provide 'cedet-android)

;;; cedet-android.el ends here
