;;; cit-arduino.el --- Arduino testing.
;;
;; Copyright (C) 2012 Eric M. Ludlam
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
;; Test suite for the EDE/Arduino project type.  Don't run this test
;; if the arduino SDK isn't installed.
;;

;;; Code:

(require 'ede/arduino)

(condition-case nil
    (require 'arduino-mode)
  (error (condition-case nil
	     (load-file "~/lisp/arduino-mode.el")
	   (error nil))))

(defvar cit-integ-arduino-sketchdir
  (expand-file-name "edeprojearduino" cedet-integ-base)
  "Root of arduino sketches during the execution of this test.")

(defvar cit-integ-arduino-preftext
  (format "
serial.port=/dev/ttyBOGUS
sketchbook.path=%s
board=uno
" cit-integ-arduino-sketchdir)
  "Text for the temporary preferences file we use to fake out ede-arduino.")

(defvar cit-integ-arduino-preffile
  (expand-file-name "preferences.txt" cit-integ-arduino-sketchdir)
  "Preferences file we will create to override the default.")

(defvar cit-integ-arduino-testsketch 
  (expand-file-name "testsketch" cit-integ-arduino-sketchdir)
  "Test sketch directory.")

(defvar cit-integ-arduino-servolibtags
  (list
   (semantic-tag-new-include "Servo.h" t)
   (semantic-tag-new-variable "myservo" "Servo")
   )
  "Tags that force the compiler to link in the Servo library.")

(defun cit-ede-arduino-test ()
  "Test the EDE Arduino based project."
  
  ;; Check for the SDK
  (if (or (not (ede-arduino-find-install))
	  (not (featurep 'arduino-mode)))
	 
      (if (not (ede-arduino-find-install))
	  (message "Cannot run Arduino tests without arduino SDK installed.")
	(if (not (featurep 'arduino-mode))
	    (message "Cannot run Arduino tests without arduino-mode. Install in ~/lisp/arduino-mode.el")
	  ))

    ;; Reset the preferences file.
    (cit-make-dir cit-integ-arduino-sketchdir)
    (save-current-buffer
      (find-file cit-integ-arduino-preffile)
      (erase-buffer)
      (insert cit-integ-arduino-preftext)
      (save-buffer))

    ;; Force ede-arduino to investigate, and test that we read the
    ;; file correctly.
    (setq ede-arduino-preferences-file cit-integ-arduino-preffile)
    (let ((prefs (ede-arduino-sync)))
      (unless (and (string= (oref prefs board) "uno")
		   (string= (oref prefs port) "/dev/ttyBOGUS")
		   (string= (file-name-as-directory (oref prefs sketchbook))
			    (file-name-as-directory cit-integ-arduino-sketchdir)))
	(error "Preferences redirect read failed.")))

    ;; Create the sketch directory
    (cit-make-dir cit-integ-arduino-testsketch)
    
    ;; Load in the sketch file.
    (let* ((vers (ede-arduino-Arduino-Version))
	   (fn (expand-file-name (concat "testsketch."
					 (if (version< vers "1.0")
					     "pde"
					   "ino"))
				 cit-integ-arduino-testsketch)))
      (cit-srecode-fill-with-stuff fn nil)
      (revert-buffer t t);; Force EDE detection.

      ;; Try compiling the empty sketch.
      (cit-compile-and-wait-using-ede-command)

      ;; Test the ability to identify libraries, and link them in
      ;; correctly via the Makefile.
      (find-file fn)

      (let ((foundtags (semantic-fetch-tags)))
	;; The first tag is the setup fcn.
	(semantic-go-to-tag (car foundtags))
	(forward-char -1))
      
      (cit-srecode-insert-taglist cit-integ-arduino-servolibtags)

      ;; Try compiling the empty sketch.
      (cit-compile-and-wait-using-ede-command)
      )))


(provide 'cit-arduino)

;;; cit-arduino.el ends here
