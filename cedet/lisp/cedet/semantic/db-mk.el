;;; semantic/db-mk.el --- Command line database builder

;;; Copyright (C) 2002, 2004, 2007, 2008, 2009 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: tags

;; This file is not part of GNU Emacs.

;; Semanticdb is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;; 
;;; Commentary:
;;
;; For use by semanticdb.sh for building tag files.
;;
;; @todo - support loading configurations from the users .emacs file.

;;; Code
;;
(if (not noninteractive)
    (error "You should not load semanticdb-mk interactively."))

;; Load cedet-devel-load.el
(load-file
 (expand-file-name
  "cedet-devel-load.el"
  (concat
   (file-name-directory load-file-name) "../../../")))

;; Activate Semantic
(semantic-mode 1)

;; Process loaded buffers from the command line.
(let ((args command-line-args))
  ;; Move past this load file being loaded.
  (while (and args
	      (not
	       (progn
		 ;(message "Compare %s to %s" (car args) "-l")
		 (string= (car args) "-l"))))
    (setq args (cdr args)))
  (when args
    (setq args (cdr (cdr args)))
    ;; Grab the rest of the file names.
    ;; For each file, load it in, let semantic evaluate it for tags.
    (while args
      (princ (concat "Loading " (car args) "... "))
      (save-window-excursion
	;; @TODO - RE-WRITE THIS with code from the idle-work process.
	(let* ((buffer (find-file-noselect (car args)))
	       (tags nil))
	  (set-buffer buffer)
	  (condition-case nil
	      (setq tags (semantic-fetch-tags))
	    (error nil))
	  (princ (length tags))
	  (princ " tags found .\n")
	  (kill-buffer buffer))
	(setq args (cdr args))))))

;; Save the databases.
(semanticdb-save-all-db)

;; Done

;; Local variables:
;; byte-compile-warnings: (not unresolved)
;; End:
