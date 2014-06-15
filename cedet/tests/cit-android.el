;;; cit-android.el --- Test an android project.
;;
;; Copyright (C) 2011 Eric M. Ludlam
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
;; Android projects depend on the android SDK.  Don't run this test if
;; the android SDK is not available.
;;
;; This test has the benefit of of also testing the javap database
;; capabilities.  It has the detrement that to test javap, you need
;; the android JDK, which most machines probably don't have.

;;; Code:
(require 'cedet-android)
(require 'ede/android)

;;; Setup
(defvar cit-integ-target-android
  (expand-file-name "edeproj_ede_Android" cedet-integ-base)
  "Root of the EDE project integration tests for Android.")

(defun cit-ede-android-test ()
  "Test EDE Android based Project."

  ;; Check for the SDK
  (if (not (cedet-android-adb-version-check t))
      (message "Cannot run android tests due to missing Android SDK.")

    ;; Create file parts
    (cit-make-dir cit-integ-target-android)
    (cedet-android-create-project "CEDET_TEST" "com.cedet.test" 4 cit-integ-target-android)
    (sit-for 2) ; @TODO : should be a wait till process ends

    ;; Pull in the manifest to force EDE to load.
    (find-file (cit-file-android "AndroidManifest.xml"))

    ;; Load in the created src file.
    (find-file (cit-file-android "src/com/cedet/test/ACEDET_TEST.java"))

    ;; Try compiling the project.
    (cit-compile-and-wait-using-ede-command)
    ))

(defun cit-file-android (filename)
  "Return a testing filename.
Append FILENAME to the target directory."
  (expand-file-name filename cit-integ-target-android))

(provide 'cit-android)

;;; cit-android.el ends here
