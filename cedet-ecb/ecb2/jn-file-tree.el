;;; jn-file-tree.el --- 

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

(require 'jn-tree-node)

(defclass jn-file-tree-node (jn-dynamic-tree-node)
  ((file-path :initarg :file-path :initform nil)
   )
  )

(defun jn-file-tree-node-new (file-path)
  (let ((node (jn-file-tree-node file-path :file-path file-path)))
    (unless (file-directory-p file-path)
      (oset node updated t))
    node))

(defmethod jn-has-children ((node jn-file-tree-node))
  (if (jn-is-updated node)
      (call-next-method)
    (file-directory-p (oref node file-path))))

(defmethod jn-get-name ((node jn-file-tree-node))
  (file-name-nondirectory (oref node file-path)))

(defmethod jn-update ((node jn-file-tree-node))
  (jn-update--internal node
		       (directory-files (oref node file-path) t "[^\\.].*")
		       (function (lambda (item child)
				   (string= (oref child file-path) item)))
		       (function (lambda (item)
				   (jn-file-tree-node-new item)))
		       ))
  
(provide 'jn-file-tree)

;;; jn-file-tree.el ends here