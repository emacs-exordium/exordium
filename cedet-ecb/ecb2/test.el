;;; test.el --- 

;; Copyright (C) 2001 by Free Software Foundation, Inc.

;; Author:  <Sune Mangs@MAYHEM>
;; Keywords: 

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; 

;;; Code:

(require 'jn-tree-view)
(require 'jn-file-tree)

(defvar jn-test-view nil)

(defun jn-create-nodes (node depth name)
  (let ((child (jn-tree-node-new name node)))
    (when (< depth 3)
      (let ((i 0))
	(while (< i 2)
	  (jn-create-nodes child (1+ depth) (concat name "." (int-to-string i)))
	  (setq i (1+ i)))))))

(defun jn-print-2 (node prefix)
  (insert (concat prefix (jn-get-name node) "\n"))
  (dolist (child (jn-get-children node))
    (jn-print-2 child (concat prefix "  "))))

(defun jn-print (node)
  (set-buffer (get-buffer-create "*test*"))
  (jn-print-2 node ""))

(defun jn-test ()
  (interactive)
  (let ((window (jn-view-window "Window" :emacs-window (selected-window)))
	(node (jn-tree-node-new "root")))
    (setq jn-test-view (jn-tree-view-new))
    (jn-set-view window jn-test-view)
    (jn-create-nodes node 0 "0")
    (jn-print node)
    (jn-set-node (jn-get-root jn-test-view) node)))
	   
(defun jn-test-file-tree ()
  (interactive)
  (let ((tree (jn-file-tree-node-new "c:/t2")))
    (jn-print tree)))
	   
(defun jn-test-file-tree-view ()
  (interactive)
  (let ((tree (jn-file-tree-node-new "c:/"))
	(window (jn-view-window "Window" :emacs-window (selected-window)))
	(view (jn-tree-view-new)))
    (jn-set-view window view)
    (jn-set-node (jn-get-root view) tree)))
  
(defun jn-test-change ()
  (interactive)
  (jn-set-parent
   (car (jn-get-children
	 (car (jn-get-children (jn-get-node (jn-get-root jn-test-view))))))
   nil))
  
;;; test.el ends here
