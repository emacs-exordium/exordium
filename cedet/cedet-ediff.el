;;; cedet-ediff --- Ediffing utilities for CEDET maintenance.

;;; Copyright (C) 2008, 2009, 2010 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>

;; This file is not part of GNU Emacs.

;; Semantic is free software; you can redistribute it and/or modify
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

;;; Commentary:
;;
;; M-x cedet-ediff-emacs
;;
;; To compare the current buffer vs a matching buffer in CEDET/Emacs
;; repositories.  Set the repository locations to start using this
;; utility to run ediff between repository versions.
;;

;;; Code:
(defvar cedet-ediff-emacs-repository (expand-file-name "~/src/emacs.cedet/lisp")
  "Location of the Emacs repository.")

(defvar cedet-ediff-cedet-repository (expand-file-name "~/cedet")
  "Location of the Emacs repository.")

(defun cedet-ediff-emacs ()
  "Ediff the current buffer to a match in the Emacs repository."
  (interactive)
  (let ((src (buffer-file-name (current-buffer)))
	(destfile nil)
	(buff nil))

    ;; Find a repository match
    (cond ((string-match (concat "^" cedet-ediff-cedet-repository) src)
	   (setq destfile (cedet-repository-map-cedet->emacs src)))
	  ((string-match (concat "^" cedet-ediff-emacs-repository) src)
	   (setq destfile (cedet-repository-map-emacs->cedet src)))
	  )

    (when (not destfile)
      (error "No destination found"))

    (setq buff (find-file-noselect destfile))
    
    (ediff-buffers (current-buffer) buff)

    ))

(defvar cedet-ediff-file-map
  '(
    ;; CEDET regexp    .    Emacs regexp
    ("ede/ede-" . "cedet/ede/")
    ("ede/project-am.el" . "cedet/ede/project-am.el")
    ("ede/ede\\.el" . "cedet/ede\\.el")
    ("srecode/srecode-" . "cedet/srecode")
    ("ede/srecode\\.el" . "cedet/srecode\\.el")
    ("semantic/semantic-" . "cedet/semantic/")
    ("semantic/wisent/wisent/wisent.el" . "cedet/semantic/wisent.el")
    ("semantic/wisent/wisent-" . "cedet/semantic/wisent/")
    ("semantic/semanticdb-" . "cedet/semantic/db-")
    ("common/" . "cedet/")
    ("eieio/" . "emacs-lisp/")
    )
  "Map files names in the CEDET repository to files in the Emacs repository.")

(defun cedet-repository-map-cedet->emacs (&optional file)
  "Map FILE from a CEDET repository name to an Emacs repository name."
  (interactive)
  (when (not file) (setq file (buffer-file-name (current-buffer))))

  (if (not (string-match (concat "^" cedet-ediff-cedet-repository) file))
      (error "Not in your cedet repository"))

  (setq file (replace-match cedet-ediff-emacs-repository t t file))

  (let ((map cedet-ediff-file-map)
	ans)
    (while (and map (not ans))
      (when (string-match (car (car map)) file)
	(setq ans (replace-match (cdr (car map)) nil t file))
	)
      (setq map (cdr map)))
    (when (interactive-p) (message "Translation: %S" ans))
    ans))

(defun cedet-repository-map-emacs->cedet (&optional file)
  "Map FILE from a Emacs repository name to an CEDET repository name."
  (interactive)
  (when (not file) (setq file (buffer-file-name (current-buffer))))

  (if (not (string-match (concat "^" cedet-ediff-emacs-repository) file))
      (error "Not in your cedet repository"))

  (setq file (replace-match cedet-ediff-cedet-repository t t file))

  (let ((map cedet-ediff-file-map)
	ans)
    (while (and map (not ans))
      (when (string-match (cdr (car map)) file)
	(setq ans (replace-match (car (car map)) nil t file))
	)
      (setq map (cdr map)))
    (when (interactive-p) (message "Translation: %S" ans))
    ans))


(provide 'cedet-ediff)

;;; cedet-ediff.el ends here
