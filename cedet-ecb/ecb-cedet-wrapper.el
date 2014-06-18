;;; ecb-cedet-wrapper.el -- define wrappers for all cedet funcs/vars

;; Copyright (C) 2000 - 2009 Klaus Berndl,
;;                           Free Software Foundation, Inc.

;; Author: Klaus Berndl <klaus.berndl@sdm.de>
;; Maintainer: Klaus Berndl <klaus.berndl@sdm.de>
;; Keywords: browser, code, programming, tools
;; Created: 2009

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;; $Id$

;;; Commentary:

;; This file contains wrappers for every cedet-function and -variable used
;; by ECB independent which cedet version is used. So the ECB-code is
;; independent from the fact, if cedet offers backward-compatibility or
;; not. This library offers for each used variable V of cedet a getter-function
;; named "ecb--V" and for each function F an alias named "ecb--F". V and F
;; follow the naming conventiones of cedet 1.0 but the resulting functions
;; always point to the correct variable or function of cedet independent
;; which cedet version is loaded. ECB only uses the functions exported from
;; ecb-cedet-wrapper.el!
;;
;; In addition this file defines all requirements ECB needs of CEDET.


(eval-when-compile
  (require 'silentcomp))

(eval-when-compile
  ;; to avoid compiler grips
  (require 'cl))

(require 'ecb-util)

(defconst ecb-cedet-old-sourcetree-structure-detected-p
  (locate-library "semantic-ctxt")
  "Not nil if old cedet sourcetree-structure is detected.")

;; Additonal cedet libraries needed by ecb must be added here!!
;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: We have to pay attention if there
;; are changes when Eric marriages the two styles for his CVS-repository
(defconst ecb-cedet-lib-registry '((cedet . cedet)
                                   (semantic . semantic)
                                   (semantic-ctxt . semantic/ctxt)
                                   (semantic-analyze . semantic/analyze)
                                   (semanticdb . semantic/db)
                                   (semanticdb-find . semantic/db-find)
                                   (semanticdb-mode . semantic/db-mode)
                                   (semantic-el . semantic/bovine/el)
                                   (eieio . eieio)
                                   (speedbar . speedbar))
  "Maps the old library-structure of cedet to the equivalent libs
of Emacs >= 23.2. The value is an assoc list where the car is the
lib-symbol of an old-style cedet-library and the cdr is the
corresponding lib-symbol of the new style library \(as with the
cedet-suite integrated into Emacs >= 23.2).

ALL CEDET-LIBRARIES NEEDED BY ECB MUST BE REGISTERED HERE!")

(defconst ecb-cedet-required-version-min '(1 0 2 6)
  "Minimum version of cedet needed by ECB.
The meaning is as follows:
1. Major-version
2. Minor-version
3. 0 = alpha, 1 = beta, 2 = pre, 3 = nothing \(e.g. \"1.4\"), 4 = . \(e.g. \"1.4.3\"
4. Subversion after the alpha, beta, pre or .")

(defun ecb-cedet-missing-libs ()
  "Return a list of names of missing cedet-libs.
If no cedet libs are missing nil is returned."
  (let ((missing-libs-list nil)
        (lib nil))
    (dolist (l-elem ecb-cedet-lib-registry)
      (setq lib (symbol-name (if ecb-cedet-old-sourcetree-structure-detected-p
                                 (car l-elem)
                               (cdr l-elem))))
      (when (not (locate-library lib))
        (push lib missing-libs-list)))
    missing-libs-list))

(defun ecb-cedet-require (old-style-lib)
  "Loads a cedet-library CVS-LIB into Emacs.
OLD-STYLE-LIB is the symbol-name of the cedet-library as in the cvs-version of
cedet in feb 2010 \(ie. there is a lib semantic-ctxt instead of semantic/ctxt).
All cedet libaryies needed by ECB must be loaded with this function! Do not
use `require' for looading a cedet-library into Emacs!"
  (require (if ecb-cedet-old-sourcetree-structure-detected-p
               old-style-lib
             (cdr (assoc old-style-lib ecb-cedet-lib-registry)))))

;; With old style CEDET-load-mechanism cedet.el must be loaded "by
;; hand" to setup load-path correctly for cedet.
;; + Old-style CEDET-loader: The following require for 'cedet fails if
;;   cedet.el is either not loaded or cedet/common is not contained in the
;;   load-path. For these cases we encapsulate it with ignore-errors.
;; + New-style CEDET-loader (as in Emacs >= 23.2): cedet.el is not needed to
;;   setup the load-path but it contains the costant `cedet-version' which is
;;   needed by ECB.
(ignore-errors (ecb-cedet-require 'cedet))

(defconst ecb-cedet-missing-libraries
  (ecb-cedet-missing-libs)
  "List of library-names of CEDET missed by ECB.
Nil if all libs needed by ECB are found.")

(unless ecb-cedet-missing-libraries
  (ecb-cedet-require 'semantic)
  (ecb-cedet-require 'semantic-ctxt)
  (ecb-cedet-require 'semantic-analyze)
  (ecb-cedet-require 'semanticdb)
  (ecb-cedet-require 'semanticdb-find)
  (ecb-cedet-require 'semanticdb-mode)
  (ecb-cedet-require 'eieio))

(defconst ecb-compiled-in-semantic-version
  (eval-when-compile (ignore-errors semantic-version))
  "Semantic-version used for byte-compiling ECB. Either nil when no semantic
is loaded or the value of `semantic-version' at ECB-compilation time.")

(defconst ecb-compiled-in-cedet-version
  (eval-when-compile (ignore-errors cedet-version))
  "Cedet-version used for byte-compiling ECB. Either nil when no semantic
is loaded or the value of `cedet-version' at ECB-compilation time.")

(defun ecb-check-semantic-load ()
  "Checks if cedet is correctly loaded if semantic 2.X is used and if the same
semantic-version has been used for byte-compiling ECB and loading into Emacs.
If ECB detects a problem it is reported and then an error is thrown."
  (when (boundp 'semantic-version)
    (let ((err-msg
           (cond ;; Different semantic-version used for byte-compiling ECB and
            ;; loading into Emacs.
            ((not (string= semantic-version ecb-compiled-in-semantic-version))
             (concat "ECB has been byte-compiled with another semantic-version than currently\n"
                     "loaded into Emacs:\n"
                     (format "  + Semantic used for byte-compiling ECB: %s\n"
                             ecb-compiled-in-semantic-version)
                     (format "  + Semantic currently loaded into Emacs: %s\n"
                             semantic-version)
                     "Please ensure that ECB is byte-compiled with the same semantic-version as you\n"
                     "you load into your Emacs. Check if you have byte-compiled ECB with the cedet-\n"
                     "suite but loaded old semantic 1.X into Emacs or vice versa.\n\n"
                     "In general it is recommended to start ECB first-time not byte-compiled\n"
                     "and then call the command `ecb-byte-compile'. This ensures you byte-compile ECB\n"
                     "with the same library-versions \(semantic etc.) as you load into Emacs.\n"
                     "If you use the Makefile check the variables CEDET before compiling!\n"
                     ))
            (t ""))))
      (unless (= 0 (length err-msg)) 
        (with-output-to-temp-buffer "*ECB semantic-load problems*"
          (princ "Currently ECB can not be activated cause of the following reason:\n\n")
          (princ err-msg)
          (princ "\n\nPlease fix the reported problem and restart Emacs\n"))
        (ecb-error "Please fix the reported problem and restart Emacs!")))))

(defun ecb-check-cedet-load ()
  "Checks if cedet is correctly loaded if semantic 2.X is used and if the same
semantic-version has been used for byte-compiling ECB and loading into Emacs.
If ECB detects a problem it is reported and then an error is thrown."
  (when (boundp 'cedet-version)
    (let ((err-msg
           (cond ;; cedet was not compiled into ECB
            ((null ecb-compiled-in-cedet-version)
             (concat (format "Currently CEDET %s is loaded but ECB has been byte-compiled without\n"
                             cedet-version)
                     "any CEDET. Please either use ECB un-byte-compiled \(remove all *.elc\n"
                     "files from the ECB-directory) or byte-compile ECB correctly with CEDET!\n"
                     "In the later case it is recommended to start ECB first-time not byte-compiled\n"
                     "and then call the command `ecb-byte-compile'. This ensures you byte-compile ECB\n"
                     "with the same CEDET-library-version as you load into Emacs.\n"
                     "If you use the Makefile check the variable CEDET before compiling!\n"
                     ))
            ;; Different cedet-version used for byte-compiling ECB and
            ;; loading into Emacs.
            ((not (string= cedet-version ecb-compiled-in-cedet-version))
             (concat "ECB has been byte-compiled with another cedet-version than currently\n"
                     "loaded into Emacs:\n"
                     (format "  + CECET used for byte-compiling ECB: %s\n"
                             ecb-compiled-in-cedet-version)
                     (format "  + CEDET currently loaded into Emacs: %s\n"
                             cedet-version)
                     "Please ensure that ECB is byte-compiled with the same cedet-version as you\n"
                     "you load into your Emacs.\n\n"
                     "In general it is recommended to start ECB first-time not byte-compiled\n"
                     "and then call the command `ecb-byte-compile'. This ensures you byte-compile ECB\n"
                     "with the same CEDET-library-version as you load into Emacs.\n"
                     "If you use the Makefile check the variable CEDET before compiling!\n"))
            (t ""))))
      (unless (= 0 (length err-msg)) 
        (with-output-to-temp-buffer "*ECB cedet-load problems*"
          (princ "Currently ECB can not be activated cause of the following reason:\n\n")
          (princ err-msg)
          (princ "\n\nPlease fix the reported problem and restart Emacs\n"))
        (ecb-error "Please fix the reported problem and restart Emacs!")))))

(defconst ecb-semantic-2-loaded (ignore-errors
                                  (string-match "^2" semantic-version)))

;; -- getter functions for all variables of cedet currently used by ECB ---

(defsubst ecb--semantic-symbol->name-assoc-list ()
  "Return the value of `semantic-symbol->name-assoc-list'."
  (symbol-value 'semantic-symbol->name-assoc-list))

(defsubst ecb--semantic-symbol->name-assoc-list-for-type-parts ()
  "Return the value of `semantic-symbol->name-assoc-list-for-type-parts'."
  (symbol-value 'semantic-symbol->name-assoc-list-for-type-parts))

(defsubst ecb--semantic-format-tag-functions ()
  "Return value of `semantic-format-tag-functions'."
  (symbol-value 'semantic-format-tag-functions))

(defsubst ecb--semantic-orphaned-member-metaparent-type ()
  "Return the value of `semantic-orphaned-member-metaparent-type'."
  (symbol-value 'semantic-orphaned-member-metaparent-type))

(defsubst ecb--semantic-uml-colon-string ()
  "Return the value of `semantic-uml-colon-string'."
  (symbol-value 'semantic-uml-colon-string))

(defsubst ecb--semantic-format-face-alist ()
  "Return the value of `semantic-format-face-alist'."
  (symbol-value 'semantic-format-face-alist))

(defsubst ecb--semantic-after-toplevel-cache-change-hook ()
  "Return the hook-symbol `semantic-after-toplevel-cache-change-hook'."
  'semantic-after-toplevel-cache-change-hook)

(defsubst ecb--semantic-after-partial-cache-change-hook ()
  "Return the hook-symbol `semantic-after-partial-cache-change-hook'."
  'semantic-after-partial-cache-change-hook)

(defsubst ecb--semantic--before-fetch-tags-hook ()
  "Return the hook-symbol `semantic--before-fetch-tags-hook'."
  'semantic--before-fetch-tags-hook)

(defsubst ecb--ezimage-use-images ()
  (if (boundp 'ezimage-use-images)
      ezimage-use-images))

(defsubst ecb--semantic-format-use-images-flag ()
  (if (boundp 'semantic-format-use-images-flag)
      semantic-format-use-images-flag))

;; -- an alias for all functions of cedet currently used by ECB ---

;; (delq nil (mapcar (function (lambda (f)
;;                               (if (not (fboundp f))
;;                                   f)))
;;                   ecb--cedet-function-list))

(defconst ecb--cedet-function-list
  '(
    semantic--format-colorize-text
    semantic--tag-get-property
    semantic--tag-overlay-cdr
    semantic--tag-put-property
    semantic--tag-set-overlay
    semantic-active-p
    semantic-adopt-external-members
    semantic-analyze-current-context
    semantic-analyze-find-tag
    semantic-analyze-possible-completions
    semantic-analyze-tag-type
    semantic-brute-find-first-tag-by-name
    semantic-bucketize
    semantic-c-template-string
    semantic-calculate-scope
    semantic-clear-toplevel-cache
    semantic-current-tag
    semantic-current-tag-parent
    semantic-dependency-tag-file
    semantic-documentation-for-tag
    semantic-equivalent-tag-p
    semantic-fetch-available-tags
    semantic-fetch-tags
    semantic-find-tag-by-overlay
    semantic-find-tags-by-class
    semantic-find-tags-by-name
    semantic-flatten-tags-table
    semantic-get-local-arguments
    semantic-go-to-tag
    semantic-lex-token-start
    semantic-overlay-live-p
    semantic-overlay-p
    semantic-something-to-tag-table
    semantic-tag
    semantic-tag-abstract-p
    semantic-tag-bounds
    semantic-tag-buffer
    semantic-tag-calculate-parent
    semantic-tag-children-compatibility
    semantic-tag-class
    semantic-tag-components
    semantic-tag-components-with-overlays
    semantic-tag-end
    semantic-tag-faux-p
    semantic-tag-function-arguments
    semantic-tag-function-constructor-p
    semantic-tag-function-destructor-p
    semantic-tag-function-parent
    semantic-tag-get-attribute
    semantic-tag-name
    semantic-tag-named-parent
    semantic-tag-new-variable
    semantic-tag-overlay
    semantic-tag-p
    semantic-tag-protection
    semantic-tag-prototype-p
    semantic-tag-start
    semantic-tag-static-p
    semantic-tag-type
    semantic-tag-type-interfaces
    semantic-tag-type-members
    semantic-tag-type-superclasses
    semantic-tag-with-position-p
    semanticdb-brute-deep-find-tags-by-name
    semanticdb-deep-find-tags-by-name
    semanticdb-find-result-length
    semanticdb-find-result-nth
    semanticdb-find-tags-by-name
    semanticdb-full-filename
    semanticdb-minor-mode-p
    semanticdb-strip-find-results
    )
)

(defconst ecb--semantic-format-function-list
  '(
    semantic-format-tag-abbreviate
    semantic-format-tag-concise-prototype
    semantic-format-tag-name
    semantic-format-tag-prin1
    semantic-format-tag-prototype
    semantic-format-tag-summarize
    semantic-format-tag-uml-abbreviate
    semantic-format-tag-uml-concise-prototype
    semantic-format-tag-uml-prototype
    ))

;; new let us create the aliase. Each alias has the name "ecb--"<function of
;; cedet >= 1.0
(unless ecb-cedet-missing-libraries
  (dolist (f-elem  (append ecb--cedet-function-list ecb--semantic-format-function-list))
    (defalias (intern (concat "ecb--" (symbol-name f-elem))) f-elem)))

(silentcomp-provide 'ecb-cedet-wrapper)

;;; ecb-cedet-wrapper.el end here
