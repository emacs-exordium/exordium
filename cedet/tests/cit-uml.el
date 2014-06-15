;;; cit-uml.el --- Test COGRE as a part of the CEDET suite.
;;
;; Copyright (C) 2009 Eric M. Ludlam
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
;; Use COGRE as part of the Cedet integration tests.
;;
;; Create a graph, generate code, and compile that code.  Use Semantic to validate
;; that we created the correct objects.

;;; Code:

(require 'cogre/uml)

(defvar cit-uml-cogre-file "cit-uml.cgr"
  "The file read in for a COGRE diagram.")

;; We need some sort of main.
(defvar cit-uml-main-cpp-tags
  (list
   (semantic-tag-new-function
    "main" "int"
    (list (semantic-tag-new-variable "argc" "int")
	  (semantic-tag-new-variable "argv" "char"
				     nil
				     :pointer 2 )))
   )
  "Tags to be inserted into main.")

(defun cit-fill-uml (make-type)
  "Fill a buffer with code based on a UML representation."
  
  ;; 5 a) Read in the cogre UML file.
  ;; It should automatically switch to cogre mode.
  (let ((ll (or (locate-library "cit-uml.cgr")
		(error "Could not locate 'cit-uml.cgr'."))))
    (find-file ll))
  (sit-for 0)

  ;; 5 b) Generate code for the graph.
  (cogre-export-code (cit-file "uml/umltest.cpp"))
  (sit-for 0)
  (save-buffer)

  ;; 5 c) Compile sources.
  ;;  This needs a few EDE steps.
  (ede-new make-type "UML")
  (ede-new-target "UML" "program" "n")
  (ede-add-file "UML")

  ;; Need some sort of main for a successful compilation step.
  (cit-srecode-fill-with-stuff "uml/main.cpp" cit-uml-main-cpp-tags)
  (ede-add-file "UML")

  (ede-commit-project (ede-current-project))

  (cit-compile-and-wait)
  )



(provide 'cit-uml)
;;; cit-uml.el ends here
