;;; cedet-remove-builtin.el --- Remove CEDET version shipping with Emacs
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

;;; Code:

(defvar cedet-remove-builtin-package-list
  '(eieio semantic srecode ede data-debug)
  "CEDET packages part of Emacs proper that should be removed.
  The 'cedet' package itself is implicitly included.")

(defun cedet-remove-builtin ()
  "Force any CEDET package on load-path to be removed.
Force all symbols that belong to CEDET to be unloaded.  This
should also (hopefully) remove additional CEDET installations
installed through package managers or similar.  This is a needed
first step in getting CEDET installed from outside sources."
  (interactive)
  (when (featurep 'cedet)
    (error "Cannot unload builtin CEDET since it is already loaded."))

  (dolist (package cedet-remove-builtin-package-list)
    (when (featurep package)
      (error "%s is already loaded.  Removing CEDET now would be unwise."
	     (symbol-name package))))

  ;; Remove from load-path.
  (let (clp)
    (while (setq clp (locate-library "cedet"))
      (setq load-path (delete (directory-file-name (file-name-directory clp))
			      load-path))
      (setq load-path (delete (file-name-directory (file-name-directory clp))
			      load-path))))

  ;; Find ALL autoloaded symbols related to CEDET, and delete them.
  (dolist (R (append '(cedet) cedet-remove-builtin-package-list))
    (dolist (S (append (apropos-internal (concat "^" (symbol-name R) "-"))
		       (apropos-internal (concat "^global-" (symbol-name R) "-"))))
      (when (and (fboundp S)
		 (let ((sf (symbol-function S)))
		   (and (listp sf) (eq (car sf) 'autoload))))
	(fmakunbound S))))
  )

(cedet-remove-builtin)

(provide 'cedet-remove-builtin)

;;; cedet-remove-builtin.el ends here
