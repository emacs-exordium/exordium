;;; cedet-devel-load.el --- Use CEDET from SourceForge, not Emacs

;; Copyright (C) 2011, 2013 by Eric M. Ludlam

;; This file is not part of Emacs, and will STAY a part of CEDET/Standalone

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
;; This file is for using the CEDET from the CEDET standalone bzr
;; repository, replacing the version that comes with Emacs 23.x and greater.

;;; Code:
(when (featurep 'cedet-devel-load)
  (error "CEDET Version %s already loaded." cedet-version))

;; This file must be in "<INSTALL-DIR>" where 'cedet.el' that
;; comes with the associated repository is in: "<INSTALL-DIR>/lisp/cedet/cedet.el".
(let ((CEDETDIR (file-name-directory
		 (or load-file-name (buffer-file-name)))))

  (unless (boundp 'cedet-bootstrap-in-progress)
  ;; Remove builtin CEDET from load path and autoloaded symbols
    (load-file (expand-file-name "cedet-remove-builtin.el" CEDETDIR)))

  ;; SETUP LOAD PATHS
  (add-to-list 'load-path CEDETDIR)
  (add-to-list 'load-path (expand-file-name "lisp/cedet" CEDETDIR))
  (add-to-list 'load-path (expand-file-name "lisp/eieio" CEDETDIR))
  (add-to-list 'load-path (expand-file-name "lisp/speedbar" CEDETDIR))

  (require 'eieio)
  (require 'ede)

  ;; Load in all the loaddefs unless we're bootstrapping the system
  (unless (boundp 'cedet-bootstrap-in-progress)
    (message "Loading autoloads from CEDET development.")
    (load (expand-file-name "lisp/eieio/loaddefs.el" CEDETDIR) nil t t)
    (load (expand-file-name "lisp/speedbar/loaddefs.el" CEDETDIR) nil t t)
    (load (expand-file-name "lisp/cedet/loaddefs.el" CEDETDIR) nil t t)
    (load (expand-file-name "lisp/cedet/ede/loaddefs.el" CEDETDIR) nil t t)
    (load (expand-file-name "lisp/cedet/cogre/loaddefs.el" CEDETDIR) nil t t)
    (load (expand-file-name "lisp/cedet/srecode/loaddefs.el" CEDETDIR) nil t t)
    (load (expand-file-name "lisp/cedet/semantic/loaddefs.el" CEDETDIR) nil t t)
    (setq Info-directory-list
	  (cons (expand-file-name "doc/info" CEDETDIR)
		Info-default-directory-list)))

  ;; Load in COMPAT code - This is because NEW CEDET code may use this
  ;; for compatibility reasons, but Emacs integrated code removes it.
  (require 'cedet-compat)
  )

;; Skip the rest if we just want to absolute minimum (during compilation).
(unless (bound-and-true-p cedet-minimum-setup)

  (require 'cedet) ;; Get standard CEDET variables loaded.

  ;; Load the canned configurations
  (require 'semantic/canned-configs)

  ;; Add some autoloads by hand due to:
  ;;  New code
  ;;  Things disabled by core Emacs
  ;;
  ;;  @TODO - generate autoloads.
  ;;(autoload 'semantic-default-elisp-setup "semantic/bovine/el"
  ;;  "Setup hook function for Emacs Lisp files and Semantic.")

  ;; Get SRecode initialized
  (require 'srecode)
  (require 'srecode/map) ;; Get the srecode load-path filled in.

  (let ((CEDETDIR (file-name-directory
		   (or load-file-name (buffer-file-name))))
	(REMOVEME (expand-file-name "srecode" data-directory)))
    ;; Add in the devel data directory.
    (add-to-list 'srecode-map-load-path (expand-file-name "etc/srecode" CEDETDIR))
    ;; Remove the system level data directory.
    (setq srecode-map-load-path (remove REMOVEME srecode-map-load-path)))

  ;; Currently, Emacs proper doesn't track EIEIO methods.  Until it
  ;; does, we have to advice `describe-variable' and `describe-function'
  ;; for EIEIO methods to get better help buffers.

  (require 'advice)
  (require 'eieio-opt)

  (defadvice describe-variable (around eieio-describe activate)
    "Display the full documentation of FUNCTION (a symbol).
Returns the documentation as a string, also."
    (if (class-p (ad-get-arg 0))
	(eieio-describe-class (ad-get-arg 0))
      ad-do-it))

  (defadvice describe-function (around eieio-describe activate)
    "Display the full documentation of VARIABLE (a symbol).
Returns the documentation as a string, also."
    (if (generic-p (ad-get-arg 0))
	(eieio-describe-generic (ad-get-arg 0))
      (if (class-p (ad-get-arg 0))
	  (eieio-describe-constructor (ad-get-arg 0))
	ad-do-it)))

  ;; This adds further formatting and hyperlinks.
  (add-hook 'temp-buffer-show-hook 'eieio-help-mode-augmentation-maybee t)
)

(provide 'cedet-devel-load)

;; End
