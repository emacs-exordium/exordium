;;; jn-utils.el --- 

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

(defun jn-create-lambda-with-object (fn obj)
  `(lambda () (,fn ,obj)))

(defun jn-create-lambda-with-object-1 (fn obj)
  `(lambda (a1) (,fn ,obj a1)))

(defun jn-create-lambda-with-object-2 (fn obj)
  `(lambda (a2) (,fn ,obj a1 a2)))

(provide 'jn-utils)

;;; jn-utils.el ends here