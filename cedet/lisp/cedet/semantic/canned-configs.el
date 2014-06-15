;;; semantic/canned-configs.el --- Canned configurations for Semantic

;;; Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>

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
;; These are some helper functions which setup Semantic with some
;; canned configurations.  This file is only used for compatibility
;; with The Olde CEDET Ways.  New users are encouraged to frob
;; `semantic-default-submodes' and simply enable `semantic-mode'.

;;; Code:
;;

(require 'semantic/fw)

(eval-when-compile
  (require 'semantic/idle)
  (require 'semantic/edit))

(declare-function semantic-mode "semantic")
(declare-function semantic-m3-install "semantic/m3")
(declare-function semantic-toggle-decoration-style "semantic/decorate/mode")
(declare-function global-semantic-highlight-edits-mode "semantic/util-modes")
(declare-function global-semantic-show-unmatched-syntax-mode "semantic/util-modes")
(declare-function global-semantic-show-parser-state-mode "semantic/util-modes")
(declare-function semantic-load-enable-primary-ectags-support "semantic/ectags/lang")
(declare-function semantic-load-enable-secondary-ectags-support "semantic/ectags/lang2")

;;; Some speedbar major modes
(eval-after-load "speedbar"
  '(progn
     (require 'semantic/ia-sb)))

;;; Useful predefined setup
;;
(defvar semantic-load-imenu-string "TAGS"
  "String used in `semantic/canned-configs' startup for the Imenu menu item.")

(defvar semantic-load-system-cache-loaded nil
  "Non nil when the system caches have been loaded.
Prevent this load system from loading files in twice.")

(defun semantic-load-enable-minimum-features ()
  "Enable the minimum number of semantic features for basic usage.
This includes:
 `semantic-idle-scheduler-mode' - Keeps a buffer's parse tree up to date.
 `semanticdb-minor-mode' - Stores tags when a buffer is not in memory.
 `semanticdb-load-ebrowse-caches' - Loads any ebrowse dbs created earlier."
  (interactive)

  (setq semantic-default-submodes
	'(global-semantic-idle-scheduler-mode
	  global-semanticdb-minor-mode))
  (semantic-mode 1))

(defun semantic-load-enable-code-helpers ()
  "Enable some semantic features that provide basic coding assistance.
This includes `semantic-load-enable-minimum-features' plus:
  `imenu' - Lists Semantic generated tags in the menubar.
  `semantic-idle-summary-mode' - Show a summary for the tag indicated by
                                 code under point.  (intellisense)
  `semantic-mru-bookmark-mode' - Provides a `switch-to-buffer' like
                       keybinding for tag names.
  `global-cedet-m3-minor-mode' - A mouse-3 (right-click) context menu.

This also sets `semantic-idle-work-update-headers-flag' to t to
pre-build your database of header files in idle time for features
such as idle summary mode."
  (interactive)

  (setq semantic-default-submodes
	'(global-semantic-idle-scheduler-mode
	  global-semanticdb-minor-mode
	  global-semantic-idle-summary-mode
	  global-semantic-mru-bookmark-mode
	  global-cedet-m3-minor-mode))
  
  (semantic-mode 1)
  (semantic-load-code-helpers-1))

(defun semantic-load-code-helpers-1 ()
  ;; This enables parsing of header files.
  (setq semantic-idle-work-update-headers-flag t)

  (when (and (eq window-system 'x)
	     (locate-library "imenu"))
    (add-hook 'semantic-init-hook (lambda ()
				    (condition-case nil
					(imenu-add-to-menubar
					 semantic-load-imenu-string)
				      (error nil)))))
  )

(defun semantic-load-enable-gaudy-code-helpers ()
  "Enable semantic features that provide gaudy coding assistance.
This includes `semantic-load-enable-code-helpers'.
  `semantic-stickyfunc-mode' - Tracks current function in header-line
                               (when available).
  `semantic-decoration-mode' - Decorate tags based on various attributes.
  `semantic-decoration-on-includes' - Decoration style for include files.
  `semantic-idle-completions-mode' - Provide smart symbol completion
                                 automatically at idle time.

This also sets `semantic-idle-work-parse-neighboring-files-flag' to t
to pre-build your databases in idle time."
  (interactive)

  (setq semantic-default-submodes
	'(global-semantic-idle-scheduler-mode
	  global-semanticdb-minor-mode
	  global-semantic-idle-summary-mode
	  global-semantic-mru-bookmark-mode
	  global-cedet-m3-minor-mode
	  global-semantic-decoration-mode
	  global-semantic-stickyfunc-mode
	  global-semantic-idle-completions-mode))

  (semantic-mode 1)
  (semantic-load-code-helpers-1)
  (semantic-load-enable-gaudy-code-helpers-1))

(defun semantic-load-enable-gaudy-code-helpers-1 ()
  ;; Enable preparsing many neighboring files.
  (setq semantic-idle-work-parse-neighboring-files-flag t)
  )

(defun semantic-load-enable-excessive-code-helpers ()
  "Enable all semantic features that provide coding assistance.
This includes all features of `semantic-load-enable-gaudy-code-helpers' plus:
  `semantic-highlight-func-mode' - Highlight the current tag.

  `semantic-decoration-on-*-members' - Two decoration modes that
                     color the background of private and protected methods.

  `semantic-idle-local-symbol-highlight-mode' - Highlight references of the
                     symbol under point."
  (interactive)

  (setq semantic-default-submodes
	'(global-semantic-idle-scheduler-mode
	  global-semanticdb-minor-mode
	  global-semantic-idle-summary-mode
	  global-semantic-mru-bookmark-mode
	  global-cedet-m3-minor-mode
	  global-semantic-decoration-mode
	  global-semantic-stickyfunc-mode
	  global-semantic-idle-completions-mode
	  global-semantic-highlight-func-mode
	  global-semantic-idle-local-symbol-highlight-mode))

  (semantic-mode 1)
  (semantic-load-code-helpers-1)
  (semantic-load-enable-gaudy-code-helpers-1)
  (semantic-load-enable-excessive-code-helpers-1))

(defun semantic-load-enable-excessive-code-helpers-1 ()
  ;; Enable preparsing many neighboring files.
  (setq semantic-idle-work-parse-neighboring-files-flag t)
  (require 'semantic/decorate/mode)
  (semantic-toggle-decoration-style "semantic-decoration-on-private-members" t)
  (semantic-toggle-decoration-style "semantic-decoration-on-protected-members" t))

(defun semantic-load-enable-semantic-debugging-helpers ()
  "Enable all semantic features that assist with debugging semantic.
It does not include `semantic-load-enable-minimum-features'.
These modes include:
  `semantic-highlight-edits-mode' - Highlight text that has been edited
                            since the last parse step.
  `semantic-show-unmatched-syntax-mode' - Highlight lexical tokens which
                            failed to be parsed.
  `semantic-show-parser-state-mode' - Show the current buffer state via
                            small indicators in the mode line."
  (interactive)

  (require 'semantic/edit)
  (setq semantic-default-submodes
	(append semantic-default-submodes
		'(global-semantic-highlight-edits-mode
		  ;; This ought to be a code helper, but it is still
		  ;; a bit on the lame side.  Opinions?
		  global-semantic-show-unmatched-syntax-mode
		  global-semantic-show-parser-state-mode)))
  (semantic-mode 1)

  ;; This enables debug output from the incremental parser.
  ;; Perhaps a mode for that dumps stuff in a `messages' like buffer
  ;; would be better?
  (setq semantic-edits-verbose-flag t)
  )

(defun semantic-load-enable-all-ectags-support ()
  "Enable all exuberent ctags extensions.
See the functions:
   `semantic-load-enable-primary-ectags-support'
   `semantic-load-enable-secondary-ectags-support'
If you just want to add new languages, use
   `semantic-load-enable-primary-ectags-support'."
  (interactive)
  (semantic-load-enable-primary-ectags-support)
  (semantic-load-enable-secondary-ectags-support)
  )

(semantic-alias-obsolete
 'semantic-load-enable-all-exuberent-ctags-support
 'semantic-load-enable-all-ectags-support
 "CEDET 1.2")

(provide 'semantic/canned-configs)

;;; semantic/canned-configs.el ends here
