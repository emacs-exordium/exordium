;;; cedet-contrib-load.el --- Autoload definitions for cedet contrib

;;; Copyright (C) 2004, 2012, 2013 Eric Ludlam

;; Author: Eric Ludlam <zappo@gnu.org>

;; CEDET is free software; you can redistribute it and/or modify
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
;; Initialize CEDET's contributed libraries for all supported
;; conditions.

;;; Code:
;;

;;; Contrib autoloads
;;
(unless (featurep 'cedet-devel-load)
  (error "CEDET must be loaded to use CEDET's contrib utilities."))

;; This file must be in the same directory as all the files that
;; it is preparing for use.
(let ((CEDETCONTRIBDIR (file-name-directory
			(or load-file-name (buffer-file-name)))))

  (add-to-list 'load-path CEDETCONTRIBDIR)

  (message "Installing CEDET contrib packages in %s" CEDETCONTRIBDIR)

)

(load "contrib-loaddefs" nil t)

(provide 'cedet-contrib-load)

;;; cedet-contrib-load.el ends here
