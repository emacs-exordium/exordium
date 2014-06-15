;;; ede/m3.el --- Project options in CEDET m3
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
;; Supply options from the CEDET m3 option specific to the current project.

;;; Code:

(require 'ede)
(require 'cedet-m3)

(defun ede-m3-ede-items ()
  "Return a list of menu items based on EDE project stats."
  ;; Only create items if EDE is active.
  (when ede-object
    (let ((objs (if (eieio-object-p ede-object)
		    (list ede-object)
		  ede-object))
	  (items nil))
      ;; Do this for every target.
      (dolist (OBJ objs)
	;; If the active item is a PROJECT, provide a project level compile.
	(if (ede-project-child-p OBJ)
	    (setq items
		  (cons (cedet-m3-menu-item
			 (concat "Compile Project: (" (ede-name OBJ) ")")
			 'ede-compile-project
			 :active t
			 :help "Compile the current project with EDE.")
			items))
	  ;; Else, we can compile a target.
	  (setq items
		(cons (cedet-m3-menu-item
		       (concat "Compile Target: (" (ede-name OBJ) ")")
		       `(lambda () (interactive) (project-compile-target ,OBJ))
		       :active t
		       :help "Compile the current target with EDE.")
		      items))))
      items)))

;;;###autoload
(defun ede-m3-install ()
  (add-hook 'cedet-m3-menu-query-hooks 'ede-m3-ede-items))

(provide 'ede/m3)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "ede/m3"
;; End:

;;; ede-m3.el ends here
