;;; window.el --- 

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

(require 'jn-window)
(require 'jn-tree-node)

;;====================================================
;; 
;;====================================================

(defclass jn-tree-view-node (jn-tree-node-decorator)
  ((expanded :type boolean
	     :initarg :expanded
	     :initform nil
	     :protection private)
   (lines :type integer :initform 0)
   )
  )

(defun jn-tree-view-node-new (&optional expanded)
  (let ((node (jn-tree-view-node "node" :expanded expanded)))
    (jn-init node)
    node))

(defmethod jn-get-display-name ((node jn-tree-view-node))
  (jn-get-name node))

(defmethod jn-selected ((node jn-tree-view-node))
  (message (oref node name)))

(defmethod jn-toggle-expanded ((node jn-tree-view-node))
  (oset node expanded (not (oref node expanded)))
  (jn-changed node))

(defmethod jn-is-expanded ((node jn-tree-view-node))
  (oref node expanded))

(defmethod jn-create-child ((node jn-tree-view-node))
  (jn-tree-view-node-new))

(defmethod jn-selected ((node jn-tree-view-node))
  (message (concat (jn-get-name node) " selected!")))

(defmethod jn-get-view ((node jn-tree-view-node))
  (jn-get-view (oref node parent)))

(defmethod jn-changed ((node jn-tree-view-node))
  (let ((view (jn-get-view node)))
    (when view
      (jn-update-node view node))))


;;====================================================
;; 
;;====================================================

(defclass jn-tree-view-root-node (jn-tree-view-node)
  ((view :initarg :view :type (or null jn-tree-view) :initform nil)
   )
  )

(defmethod jn-get-view ((node jn-tree-view-root-node))
  (oref node view))


;;====================================================
;; 
;;====================================================

(defclass jn-tree-view (jn-general-view)
  ((root-node ;;:type (or null jn-tree-view-node)
    :initarg :root-node
    :initform nil)
   (nodes :type list
	  :initform nil))
  "A tree view that contains jn-tree-view-node's. When a node is changed the
node's lines in the view are updated."
  )

(defun jn-tree-view-new ()
  (let ((view (jn-tree-view "View"
			    :root-node
			    (jn-tree-view-root-node "root"))))
    (oset (jn-get-root view) view view)
    (jn-init-1 view "Tree View")
    view))

(defmethod jn-get-root ((view jn-tree-view))
  (oref view root-node))

(defun jn-tree-view-child-at-line (node line)
  (dolist (child (jn-get-children node))
    (if (= line 0)
	(throw 'exit child)
      (when (< line (oref child lines))
	(jn-tree-view-child-at-line child (1- line))))
    (setq line (- line (oref child lines)))))
      
(defmethod jn-get-node-at-point ((view jn-tree-view))
  (let ((linenr (+ (count-lines 1 (point)) (if (= (current-column) 0) 0 -1))))
    (catch 'exit
      (jn-tree-view-child-at-line (jn-get-root view) linenr))))

(defmethod jn-selected ((view jn-tree-view))
  (let ((node (jn-get-node-at-point view)))
    (when node
      (if (> (current-column)
	     (+ (length (jn-get-display-name node))
		(* 2 (- (jn-get-depth node) 1))))
	  (jn-toggle-expanded node)
	(jn-selected node)))))

(defun jn-buffer-insert-text (text)
  (let ((p (point)))
    (insert text)
    (put-text-property p (+ p (length text)) 'mouse-face 'highlight)))

(defun jn-tree-view-get-node-line (node)
  "Returns the line number for a node in the view." 
  (let ((parent (jn-get-parent node)))
    (if parent
	(+ 1
	   (jn-tree-view-get-node-line parent)
	   (catch 'exit
	     (let ((line 0))
	       (dolist (child (jn-get-children parent))
		 (when (eq child node)
		   (throw 'exit line))
		 (incf line (oref child lines))))))
      0)))

(defun jn-tree-view-update-parents-lines (node dl)
  (let ((parent (jn-get-parent node)))
    (when parent
      (oset parent lines (+ (oref parent lines) dl))
      (jn-tree-view-update-parents-lines parent dl))))

(defun jn-tree-view-update--internal (view node depth)
  (insert (make-string (* depth 2) ? ))
  (jn-buffer-insert-text
   (jn-get-display-name node))
  (when (jn-has-children node)
    (insert " ")
    (jn-buffer-insert-text
     (concat "["
	     (if (jn-is-expanded node)
		 "-" "+")
	     "]")))
  (insert "\n")
  (let ((lines 1))
    (when (jn-is-expanded node)
      (dolist (child (jn-get-children node))
	(incf lines (jn-tree-view-update--internal view child (1+ depth)))))
    (oset node lines lines)
    lines))

(defmethod jn-update ((view jn-tree-view))
  (jn-update-node view (jn-get-root view)))

(defmethod jn-update-node ((view jn-tree-view) node)
  (let ((win (jn-get-window view)))
    (when win
      (save-current-buffer
	(let* ((b (jn-get-emacs-buffer view))
	       (w (jn-get-emacs-window win))
	       (ws (window-start w))
	       (p (point)))
	  (set-buffer b)
	  (goto-char (point-min))
	  (if (eq node (jn-get-root view))
	      (let ((lines 0))
		(dolist (child (jn-get-children node))
		  (incf lines (jn-tree-view-update--internal view child 0)))
		(oset node lines lines))
	    (let* ((line (- (jn-tree-view-get-node-line node) 1))
		   (pos (line-beginning-position (1+ line)))
		   (lines (oref node lines)))
	      (goto-char pos)
	      (delete-region pos
			     (line-beginning-position (1+ lines)))
	      (jn-tree-view-update-parents-lines
	       node
	       (- (jn-tree-view-update--internal view
						 node
						 (- (jn-get-depth node) 1))
		  lines))))
	  (goto-char p)
	  (set-window-start w ws)
	  )))))

(provide 'jn-tree-view)

;;; window.el ends here