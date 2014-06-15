;;; cit-gnustep.el --- Test EDE GNUstep Project

;; Copyright (C) 2009, 2010 Eric M. Ludlam
;; Copyright (C) 2008 "Marco (Bj) Bardelli"

;; Author: Marco (Bj) Bardelli <bardelli.marco@gmail.com>

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
;; EDE GNUstep for the cedet integration tests.

;; TODO:
;; - fix scanner mode (paths etc.)
;; - maybe clean between compilations
;; - test equivalence between modes, via diffing ProjStep.ede .
;; - test RootProjStep.ede

;;; Code:

;(require 'ede-pmake "../ede/ede-pmake.el" t)
(require 'ede/gnustep)

(defvar cedet-integ-target-gnustep
  (expand-file-name "edeproj_ede_GSMake" cedet-integ-base)
  "Root of the EDE project integration tests.")

(defun cit-ede-step-test ()
  "Test EDE GNUstep-Make Project"

  ;; Create file parts.
  (cit-make-dir cedet-integ-target-gnustep)
  (find-file (cit-step-file "main.c"))
  (erase-buffer)
  (insert
   "#include <stdio.h>\nint main (){ printf(\"Hello CEDET!\"); }\n")
  (save-buffer)

  ;; Create the project and target in writer mode.
  (ede-new "GNUstep-Make" "EDE GNUstep Integration Test, writer-mode")
  (ede-new-target "test0" "tool" "n")
  (ede-add-file "test0")
  (ede-commit-project (ede-current-project))
  (find-file "ProjStep.ede")
  (ede-proj-regenerate)

  ;; Test compilation
  (cit-step-test-compilation)

  ;; Rename ProjStep.ede to ProjStep.ede.writer
  ;; to test scanner mode
  (rename-file "ProjStep.ede" "ProjStep.ede.writer" t)

  ;; To prevent cacheing of writer-mode project
  (and (get-buffer "ProjStep.ede") (kill-buffer "ProjStep.ede"))
  (and (get-buffer "edeproj_ede_GSMake") (kill-buffer "edeproj_ede_GSMake"))
  (setq ede-projects nil)

  (find-file cedet-integ-target-gnustep)
  ;; Create the project and target in scanner mode.
  (ede-new "GNUstep-Make in scanner mode"
	   "EDE GNUstep Integration Test, scanner-mode")
  (let ((ede-deep-rescan t))
    (project-rescan (ede-current-project)))

  ;; Test compilation
  (cit-step-test-compilation)

  ;; uncomment next-line to stop test and verify status
  ;; of buffers etc. to verify cit.
  ;(error t)
  )

(defun cit-step-test-compilation ()
  "Test compilation, sitting for 1 second during compilation."
  ;; test some stuff related to gnustep-make package, else use gcc.
  (if (or
       (getenv "GNUSTEP_MAKEFILES")
       (not (equal "" (shell-command-to-string "which gnustep-config"))))
      (ede-compile-project)
    (progn
      (message "I noticed that you didn't load `GNUstep.sh' for the GNUstep-Make Environment ... neither gnustep-make seems installed.")
      (message "I'll compile this simple example via gcc ... but, use gnustep ... is better ;)")
      (compile "sh -c \"gcc -o Prog main.c\"")

      (cit-wait-for-compilation)
      (cit-check-compilation-for-error)
      ))

  ;; Let Compilation finish
  (and compilation-in-progress
       (switch-to-buffer-other-window "*compilation*"))
  (cit-wait-for-compilation)
  )

(defun cit-step-file (filename)
  "Return a testing filename.
Append FILENAME to the target directory."
  (expand-file-name filename cedet-integ-target-gnustep))


(provide 'cit-gnustep)
