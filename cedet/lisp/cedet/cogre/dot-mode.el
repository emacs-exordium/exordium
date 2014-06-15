;;; cogre/dot-mode.el --- Mini-mode for Graphviz DOT files.
;;
;; Copyright (C) 2009 Eric M. Ludlam
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
;; A mini-mode for Graphviz DOT files.
;;
;; If graphviz-dot-mode.el by Pieter Pareit is available, use that,
;; otherwise supply the minimum features needed to parse dot files.

(require 'mode-local)
;;; Code:

;; Silence the byte compiler
(declare-function graphviz-dot-mode "graphviz-dot-mode")
(defvar graphviz-dot-mode-syntax-table)

;;; Syntax table
(defcustom cogre-dot-mode-hook nil
  "Hook called when cogre-dot mode starts.
This hook is not called if graphviz-dot-mode is used
instead."
  :group 'cogre
  :type 'hook)

(defvar cogre-dot-mode-syntax-table 
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?/  ". 124b" st)
    (modify-syntax-entry ?*  ". 23"   st)
    (modify-syntax-entry ?\n "> b"    st)
    (modify-syntax-entry ?=  "."      st)
    (modify-syntax-entry ?_  "_"      st)
    (modify-syntax-entry ?-  "_"      st)
    (modify-syntax-entry ?>  "."      st)
    (modify-syntax-entry ?<  "."      st)
    (modify-syntax-entry ?[  "("      st)
    (modify-syntax-entry ?]  ")"      st)
    (modify-syntax-entry ?\" "\""     st)
    (setq graphviz-dot-mode-syntax-table st)
  )
  "Syntax table for `cogre-dot-mode'.")

(defvar cogre-dot-font-lock-keywords
  `(("\\(:?di\\|sub\\)?graph \\(\\sw+\\)"
     (2 font-lock-function-name-face))
    )
  "Font lock keywords for the cogre dot mini-mode.")

;;;###autoload
(defun cogre-dot-mode ()
  "Major mode for the dot language.
This is a mini-mode that will first attempt to load and install
`graphviz-dot-mode' in this buffer.  If that fails, it installs
the syntax table, and runs a hook needed to get Semantic working
as a parsing engine."
  (interactive)

  ;; Force graphviz mode to be loaded.  If it fails, then continue
  ;; to install the cogre version.
  (condition-case nil
      (progn
	;; Uncomment this to force cogre-dot-mode to go active.
	;;(error "TEST MINI MODE")

	;; graphviz-dot-mode doesn't have a provide statement
	(when (not (fboundp 'graphviz-dot-mode))
	  (load-library "graphviz-dot-mode"))
	(inversion-test 'graphviz-dot-mode "0.3.2")
	(graphviz-dot-mode))
    (error
     ;; We found an error.  Do the setup needed to produce
     ;; a mini-mode here.

     (kill-all-local-variables)
     (setq major-mode 'cogre-dot-mode)
     (setq mode-name "C-dot")
     (set-syntax-table cogre-dot-mode-syntax-table)
     (set (make-local-variable 'comment-start) "//")
     (set (make-local-variable 'comment-start-skip) "/\\*+ *\\|//+ *")
     (set (make-local-variable 'font-lock-defaults)
	  '(cogre-dot-font-lock-keywords))
     (run-hooks 'cogre-dot-mode-hook)
     )))
;;
;; This major-mode change doesn't conflict with graphviz, since
;; this major mode will start the graphviz one if it can be found.
;;

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.dot\\'" . cogre-dot-mode))

;;
;; This isn't really true, but if we use the mini-mode,
;; this allows SRecode to find the graphviz templates.
;;
(set-mode-local-parent 'cogre-dot-mode 'graphviz-dot-mode)

(provide 'cogre/dot-mode)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "cogre/dot-mode"
;; End:

;;; cogre/dot-mode.el ends here
