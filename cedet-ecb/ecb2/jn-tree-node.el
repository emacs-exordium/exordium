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

(require 'eieio)
(require 'jn-utils)

;;====================================================
;; 
;;====================================================

(defclass jn-abstract-tree-node () ())

(defun jn-tree-node-iterate--internal (node fn)
  (funcall fn node)
  (dolist (child (jn-get-children node))
    (jn-tree-node-iterate--internal child fn)))

(defun jn-tree-node-conditional-iterate--internal (node fn depth)
  (when (funcall fn node depth)
    (dolist (child (jn-get-children node))
      (jn-tree-node-conditional-iterate--internal child fn (1+ depth)))))

(defmethod jn-iterate ((node jn-abstract-tree-node) (fn function))
  (jn-tree-node-iterate--internal node fn))
  
(defmethod jn-conditional-iterate ((node jn-abstract-tree-node) (fn function))
  (jn-tree-node-conditional-iterate--internal node fn 0))
  
(defmethod jn-find-first ((node jn-abstract-tree-node) (test-fn function))
  (catch 'exit
    (jn-iterate node
		(function (lambda (node)
			    (if (funcall test-fn node)
				(throw 'exit node)))))))

(defmethod jn-find-first-with-name ((node jn-abstract-tree-node) (name string))
  (jn-find-first node (function (lambda (node)
				  (string= name (oref node name))))))

(defmethod jn-get-depth ((node jn-abstract-tree-node))
  (let ((parent (oref node parent)))
    (if (not parent)
	0
      (1+ (jn-get-depth parent)))))

(defmethod jn-has-children ((node jn-abstract-tree-node))
  (< 0 (length (jn-get-children node))))


;;====================================================
;; 
;;====================================================

(defclass jn-changable-tree-node (jn-abstract-tree-node)
  ((change-listeners :type list
		     :initform nil)
;;		     :protection private)
   )
  )

(defun jn-tree-node-changed--internal (node)
  (dolist (listener (oref node change-listeners))
    (funcall listener node)))

(defmethod jn-changed ((node jn-changable-tree-node))
  (jn-tree-node-changed--internal node))

(defmethod jn-add-change-listener ((node jn-changable-tree-node) listener)
  (oset node change-listeners (cons listener (oref node change-listeners))))

(defmethod jn-remove-change-listener ((node jn-changable-tree-node) listener)
  (oset node change-listeners (delq listener (oref node change-listeners))))


;;====================================================
;; 
;;====================================================

(defclass jn-tree-node-base (jn-changable-tree-node)
  ((parent :type (or null jn-abstract-tree-node)
	   :initform nil)
;;	   :protection private)
   (children :type list
	     :initform nil)
;;	     :protection private)
   )
  )

(defmethod jn-get-children ((node jn-tree-node-base))
  (oref node children))

(defmethod jn-get-parent ((node jn-tree-node-base))
  (oref node parent))

(defmethod jn-set-parent ((node jn-tree-node-base) (parent jn-tree-node-base))
  (let ((old-parent (oref node parent)))
    (when old-parent
      (oset old-parent children (delq node (oref old-parent children)))
      (jn-tree-node-changed--internal old-parent))
    (oset node parent parent)
    (when parent
      (oset parent children (append (oref parent children) (list node)))
      (jn-tree-node-changed--internal parent))
    (jn-changed node)))


;;====================================================
;; 
;;====================================================

(defclass jn-dynamic-tree-node (jn-tree-node-base)
  ((updated :initform nil)
   )
  )

(defmethod jn-is-updated ((node jn-dynamic-tree-node))
  (oref node updated))

(defmethod jn-get-children ((node jn-dynamic-tree-node))
  (unless (oref node updated)
    (jn-update node))
  (call-next-method))

(defmethod jn-update--internal ((node jn-dynamic-tree-node) item-list match-fn
				new-fn)
  (let ((old-children (oref node children))
	changed
	children)
    (dolist (item item-list)
      (let ((c (find item old-children :test match-fn)))
	(setq children
	      (cons (if c
			c
		      (let ((new-child (funcall new-fn item)))
			(oset new-child parent node)
			(setq changed t)
			new-child))
		    children))))
    (oset node children (nreverse children))
    (oset node updated t)))
;    (when (or changed
;	      (> (length old-children)
;		 (length children)))
;      (jn-changed node))))


;;====================================================
;; 
;;====================================================

(defclass jn-tree-node (jn-tree-node-base)
  ((name :type string
	 :initform ""
	 :initarg :name)
   )
  )

(defun jn-tree-node-new (name &optional parent)
  (let ((node (jn-tree-node name :name name)))
    (jn-set-parent node parent)
    node))

(defmethod jn-get-name ((node jn-tree-node))
  (oref node name))

(defmethod jn-clear-children ((node jn-tree-node))
  (oset node children nil)
  (jn-changed node))


;;====================================================
;; 
;;====================================================

(defclass jn-tree-node-decorator (jn-dynamic-tree-node)
  ((node :initform nil);; :type (or null jn-changable-tree-node))
   (updated :initform nil)
   (callback :initform nil)
   )
  )

(defun jn-tree-node-decorator-new (&optional par node)
  (let ((n (jn-tree-node-decorator "Arne")))
    (jn-init n)
    (jn-set-parent n par)
    (jn-set-node n node)
    n))

(defmethod jn-init ((dnode jn-tree-node-decorator))
  (oset dnode callback (jn-create-lambda-with-object-1
			'jn-tree-node-decorator-node-changed--internal dnode)))

(defmethod jn-get-name ((dnode jn-tree-node-decorator))
  (jn-get-name (oref dnode node)))

(defmethod jn-get-children ((dnode jn-tree-node-decorator))
  (unless (oref dnode updated)
    (jn-update dnode))
  (call-next-method))

(defmethod jn-has-children ((dnode jn-tree-node-decorator))
  (if (oref dnode updated)
      (call-next-method)
    (jn-has-children (oref dnode node))))

(defmethod jn-create-child ((dnode jn-tree-node-decorator))
  (jn-tree-node-decorator "node"))

(defmethod jn-set-node ((node jn-tree-node-decorator) (n jn-changable-tree-node))
  (let ((old-node (oref node node)))
    (when old-node
      (jn-remove-change-listener old-node (oref node callback)))
    (oset node node n)
    (oset node updated nil)
    (when n
      (jn-add-change-listener n (oref node callback)))
    (jn-changed node)))

(defmethod jn-get-node ((dnode jn-tree-node-decorator))
  (oref dnode node))

(defun jn-tree-node-decorator-node-changed--internal (dnode node)
  (oset dnode updated nil)
  (jn-changed dnode))

(defmethod jn-update ((node jn-tree-node-decorator))
  (jn-update--internal node
		       (jn-get-children (oref node node))
		       (function (lambda (item child) (eq (oref child node) item)))
		       (function
			(lambda (item)
			  (let ((child (jn-create-child node)))
			    (oset child node item)
			    (jn-add-change-listener item (oref child callback))
			    child)))
		       ))
  
(provide 'jn-tree-node)

;;; window.el ends here