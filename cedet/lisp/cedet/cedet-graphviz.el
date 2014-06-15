;;; cedet-graphviz.el --- Support for running graphviz programs for CEDET.
;;
;; Copyright (C) 2009, 2010 Eric M. Ludlam
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
;; COGRE uses graphviz to export to some kinds of image formats.
;;
;; This file supports running various graphviz programs, such as dot.

;;; Code:

(require 'inversion)

(defvar cedet-graphviz-min-version "2.8"
  "Minimum version of Graphviz DOT program required.")

;;;###autoload
(defcustom cedet-graphviz-dot-command "dot"
  "Command name for the Graphviz DOT executable."
  :type 'string
  :group 'cedet)

;;;###autoload
(defcustom cedet-graphviz-neato-command "neato"
  "Command name for the Graphviz NEATO executable."
  :type 'string
  :group 'cedet)

(defun cedet-graphviz-dot-call (flags)
  "Call Graphviz DOT with the list of FLAGS."
  (let ((b (get-buffer-create "*CEDET graphviz dot*"))
	(cd default-directory)
	)
    (with-current-buffer b
      (setq default-directory cd)
      (erase-buffer))
    (apply 'call-process cedet-graphviz-dot-command
	   nil b nil flags)
    b))

(defun cedet-graphviz-neato-call (flags)
  "Call Graphviz DOT with the list of FLAGS."
  (let ((b (get-buffer-create "*CEDET graphviz neato*"))
	(cd default-directory)
	)
    (with-current-buffer b
      (setq default-directory cd)
      (erase-buffer))
    (apply 'call-process cedet-graphviz-neato-command
	   nil b nil flags)
    b))

(defun cedet-graphviz-translate-file (bufferin fileout &optional outputformat &rest flags)
  "Translate BUFFERIN to FILEOUT with OUTPUTFORMAT.
If FILEOUT is nil, then the output of neato is the translation.
The OUTPUTFORMAT is one of the dot names for an output, such as png.
The -T is appended in this function.  If OUTPUTFORMAT is not supplied, then
ps is assumed.
The rest of the argument FLAGS are more flags to pass to dot."
  (let* ((T (or outputformat "ps"))
	 (infile (buffer-file-name bufferin))
	 (allflags (append flags
			   (list (concat "-T" T))
			   (if fileout (list (concat "-o" fileout)))
			   (list infile))
		   ))
    (if (member "-n" flags)
	(cedet-graphviz-neato-call allflags)
      (cedet-graphviz-dot-call allflags))))

;;;###autoload
(defun cedet-graphviz-dot-version-check (&optional noerror)
  "Check the version of the installed Graphviz dot command.
If optional programatic argument NOERROR is non-nil, then
instead of throwing an error if Global isn't available, then
return nil."
  (interactive)
  (let ((b (cedet-graphviz-dot-call (list "-V")))
	(rev nil))
    (with-current-buffer b
      (goto-char (point-min))
      (re-search-forward "dot \\(?:- graphviz \\)?version \\([0-9.]+\\)" nil t)
      (setq rev (match-string 1))
      (if (inversion-check-version rev nil cedet-graphviz-min-version)
	  (if noerror
	      nil
	    (error "Version of Graphviz 'dot' is %s.  Need at least %s"
		   rev cedet-graphviz-min-version))
	;; Else, return TRUE, as in good enough.
	(when (called-interactively-p 'interactive)
	  (message "Graphviz Version %s  - Good enough for CEDET." rev))
	t))))

(provide 'cedet-graphviz)
;;; cedet-graphviz.el ends here
