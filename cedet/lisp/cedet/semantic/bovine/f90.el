;;; semantic/bovine/f90.el --- Semantic details for f90

;;; Copyright (C) 2010 David Engster

;; Author: David Engster <dengste@eml.cc>

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify
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
;; Parsing F90 is done mostly through special lexers and
;; tag-expansion. Therefore, most of the meat is contained in
;; semantic-f90-expand-tag and in the lexers. Also, Fortran is
;; case-insensitive, so some hacks had to be made here.
;;
;; Please see testf90.f90 in semantic/tests to see what kind of
;; completions the F90 parser can currently provide, since I try to
;; add a new test for everything that is implemented.

;;; History:
;;

(require 'semantic/bovine/f90-by)
(require 'semantic/ctxt)
(require 'semantic/decorate/mode)

(eval-when-compile
  (require 'semantic/dep)
  (require 'f90))

;;; Code:

;; Lexers:

(define-lex-regex-analyzer semantic-lex-f90-program
  "Detect program and return 'PROGRAM symbol semantic-list.
The semantic-list spans the program body."
  "^\\s-*\\(program\\)\\s-*\\(\\sw+\\)"
  (semantic-lex-push-token (semantic-lex-token 'PROGRAM
					       (match-beginning 1)
					       (match-end 1)))
  (semantic-lex-push-token (semantic-lex-token 'symbol
					       (match-beginning 2)
					       (match-end 2)))
  (goto-char (match-end 2))
  (forward-line 1)
  (beginning-of-line)
  (let ((bodystart (point)))
    (goto-char (point-max))
    (when (re-search-backward "^\\s-*end\\(\\s-*\\|\\s-+program\\(\\s-+.*\\)?\\)$"
			      nil t)
      (goto-char (match-end 0))
      (semantic-lex-push-token (semantic-lex-token
				'semantic-list bodystart (point)))))
  (setq semantic-lex-end-point (point)))

(define-lex-regex-analyzer semantic-lex-f90-subroutine
  "Detect subroutine and return 'SUBROUTINE symbol semantic-list semantic-list'.
The last semantic-list spans the subroutine body."
  "^\\s-*\\(subroutine\\)\\s-+[^\0]*?\\(\\sw+\\)\\(([^)]*)\\)?"
  (semantic-lex-push-token (semantic-lex-token 'SUBROUTINE
					       (match-beginning 1)
					       (match-end 1)))
  (semantic-lex-push-token (semantic-lex-token 'symbol
					       (match-beginning 2)
					       (match-end 2)))
  (if (match-string 3)
      (progn
	(semantic-lex-push-token (semantic-lex-token 'semantic-list
						     (match-beginning 3)
						     (match-end 3)))
	(goto-char (match-end 3)))
    (semantic-lex-push-token (semantic-lex-token 'semantic-list
						 (match-end 2)
						 (match-end 2)))
    (goto-char (match-end 2)))
  (forward-line 1)
  (beginning-of-line)
  (let ((bodystart (point))
	(subname (match-string 2)))
    (when (re-search-forward (concat "end\\s-+subroutine\\s-*" subname) nil t)
      (semantic-lex-push-token (semantic-lex-token
				'semantic-list bodystart (point)))))
  (setq semantic-lex-end-point (point)))

(define-lex-regex-analyzer semantic-lex-f90-function
  "Detect functions and return 'FUNCTION symbol semantic-list semantic-list'.
The last semantic-list spans the function body."
  "\\(function\\)\\s-*\\([^(]*\\)\\((.*)\\)"
  (semantic-lex-push-token
   (semantic-lex-token 'FUNCTION (match-beginning 1) (match-end 1)))
  (semantic-lex-push-token
   (semantic-lex-token 'symbol (match-beginning 2) (match-end 2)))
  (semantic-lex-push-token
   (semantic-lex-token 'semantic-list (match-beginning 3) (match-end 3)))
  (goto-char (match-end 3))
  (forward-line 1)
  (beginning-of-line)
  (let ((bodystart (point))
	(funname (match-string-no-properties 2)))
    (when (re-search-forward (concat "end function\\s-*" funname) nil t)
      (semantic-lex-push-token (semantic-lex-token
				'semantic-list bodystart (point)))))
  (setq semantic-lex-end-point (point)))

(define-lex-regex-analyzer semantic-lex-f90-module
  "Detect modules and return 'MODULE symbol semantic-list'.
The semantic-list spans the module body."
  "^\\s-*\\(module\\)\\s-+\\(\\sw+\\)\\s-*$"
  (semantic-lex-push-token
   (semantic-lex-token 'MODULE (match-beginning 1) (match-end 1)))
  (semantic-lex-push-token
   (semantic-lex-token 'symbol (match-beginning 2) (match-end 2)))
  (goto-char (match-end 2))
  (forward-line)
  (beginning-of-line)
  (let ((bodystart (point)))
    (re-search-forward "^\\s-*end\\s-+module\\(\\s-+.*\\)?$" nil t)
    (semantic-lex-push-token (semantic-lex-token
			      'semantic-list bodystart (point))))
  (setq semantic-lex-end-point (point)))

(define-lex-regex-analyzer semantic-lex-f90-type
  "Detect type definitions and return 'TYPE symbol semantic-list'.
The semantic-list contains the type members."
  "^\\s-*\\(type\\)\\s-*::\\s-*\\(\\sw+\\)"
  (semantic-lex-push-token
   (semantic-lex-token 'TYPE (match-beginning 1) (match-end 1)))
  (semantic-lex-push-token
   (semantic-lex-token 'symbol (match-beginning 2) (match-end 2)))
  (goto-char (match-end 2))
  (forward-line 1)
  (beginning-of-line)
  (let ((start (point))
	(typename (match-string-no-properties 2) ))
    (when (re-search-forward (concat "end type\\s-*" typename) nil t)
      (end-of-line)
      (semantic-lex-push-token (semantic-lex-token
				'semantic-list start (point))))
    (setq semantic-lex-end-point (point))))

(define-lex-regex-analyzer semantic-lex-f90-newline
  "Detect newlines for F90 code.
This special lexer is necessary to deal with the continuation
character '&'."
  "\\s-*\\(\n\\|\\s>\\)"
  (if (not (looking-back "&\\s-*"))
      (semantic-lex-push-token (semantic-lex-token 'newline (point) (1+ (point)))))
  (forward-line 1)
  (beginning-of-line)
  (if (looking-at "^\\s-*&")
      (goto-char (match-end 0)))
  (setq semantic-lex-end-point (point)))

(defun semantic-f90-skip-specs ()
  "Skip to the end of the specification part."
  (condition-case nil
      (let ((case-fold-search t))
	(while (or (looking-at "^\\s-*\\(implicit\\s-\\|use\\s-\\|!\\)")
		   (looking-at "^\\s-*\\(integer\\|real\\|double precision\\|char\\|logical\\|complex\\),?\\s-+")
		   (looking-at ".*::.*")
		   (looking-at "^\\s-*end\\s-+type\\s-*")
		   (looking-at "^\\s-*$"))
	  (semantic-end-of-command)
	  (forward-line)
	  (beginning-of-line)))
    (error t)))

(define-lex-regex-analyzer semantic-lex-f90-symbol-or-keyword
  "Detect and create symbol and keyword tokens.
This lexer uses `downcase' to make matching case-insensitive."
  "\\(\\sw\\|\\s_\\)+"
  (semantic-lex-push-token
   (semantic-lex-token
    (or (semantic-lex-keyword-p (downcase (match-string 0))) 'symbol)
    (match-beginning 0) (match-end 0))))

(define-lex-simple-regex-analyzer semantic-lex-f90-double-precision
  "Detect 'double precision' type."
  "\\(double\\s-+precision\\|DOUBLE\\s-+PRECISION\\)"
  'DPREC)

;; Define the lexial analyzer
(define-lex semantic-f90-lexer
  "Lexical Analyzer for F90 code."
  semantic-lex-f90-program
  semantic-lex-f90-subroutine
  semantic-lex-f90-type
  semantic-lex-f90-function
  semantic-lex-f90-module
  semantic-lex-ignore-whitespace
  semantic-lex-f90-newline
  semantic-lex-number
  semantic-lex-f90-double-precision
  semantic-lex-f90-symbol-or-keyword
  semantic-lex-charquote
  semantic-lex-string
  semantic-lex-paren-or-list
  semantic-lex-close-paren
  semantic-lex-ignore-comments
  semantic-lex-punctuation
  semantic-lex-default-action)

;; Tag expansion:

(defun semantic-f90-expand-tag (tag)
  "Expand TAG obtained from the f90 bovine parser.
All kinds of things are done here:
- Types of fnc/subr arguments are determined from spec-part
- Function return types are added if set in the spec-part
- 'use' statements are included in prog/fnc/subr tag
- fnc/subr in 'contains' sections are parsed and added as members
- variable lists are expanded into single tags
- members of derived types are parsed and added
- bodies of modules are parsed and added as members"
  (cond
   ((or (eq (semantic-tag-class tag) 'function)
	(eq (semantic-tag-class tag) 'subroutine))
    ;; Get the local variables for function/subroutine to get types of
    ;; arguments and used modules.
    (let* ((lv (semantic-f90-get-local-variables tag))
	   (args (semantic-tag-get-attribute tag :arguments))
	   (mods (semantic-find-tags-by-class 'use lv))
	   tags tmp newargs localfuncs)
      (when lv
	(dolist (cur lv)
	  (when (listp cur)
	    (setq tags
		  (append tags
			  (semantic-f90-expand-tag cur)))))
	(semantic-tag-put-attribute
	 tag
	 :arguments
	 (mapcar (lambda (argtag)
		   (assoc (semantic-tag-name argtag) tags))
		 args)))
      ;; Check if return type of function is set in the spec-part
      (when (and (eq (semantic-tag-class tag) 'function)
		 (setq tmp (semantic-find-tags-by-name (semantic-tag-name tag)
						       lv)))
	(semantic-tag-put-attribute tag :type (semantic-tag-type (car tmp))))
      ;; Check for used modules
      (when mods
	(semantic-tag-put-attribute tag :usedmodules mods))
      (when (setq localfuncs (semantic-f90-parse-contains tag))
	(semantic-tag-put-attribute tag :members
				    localfuncs))
      )
    nil)
   ((eq (semantic-tag-class tag) 'program)
    (let* ((lv (semantic-f90-get-local-variables tag))
	   (mods (semantic-find-tags-by-class 'use lv))
	   (bounds (semantic-tag-bounds tag))
	   localfuncs)
      ;; add modules in tag
      (when mods
	(semantic-tag-put-attribute tag :usedmodules mods))
      ;; add contained tags
      (when (setq localfuncs (semantic-f90-parse-contains tag))
	(semantic-tag-put-attribute tag :members
				    localfuncs)))
    nil)
   ((and (semantic-tag-of-class-p tag 'type)
	 (eq (semantic-tag-get-attribute tag :kind) 'derived-type))
    (semantic-tag-put-attribute tag :members
				(semantic-f90-get-type-members tag))
    nil)
   ((eq (semantic-tag-class tag) 'variable)
    (let ((names (semantic-tag-name tag))
	  (type (semantic-tag-type tag))
	  (attrs (semantic-tag-attributes tag))
	  tmp tags)
      (if (listp names)
	  (mapcar
	   (lambda (tmp2)
	     (semantic-tag-copy tag tmp2 t))
	   names)
	(list tag))))
   ((eq (semantic-tag-class tag) 'module)
    (semantic-f90-expand-module tag))
   (t
    (if (semantic-tag-p tag)
      (list tag)
      nil))))

(defun semantic-f90-expand-module (tag)
  "Expand module given in TAG.
All tags found in the modules' body are added as members."
    (let ((bounds (semantic-tag-bounds tag))
	  (case-fold-search t)
	  tmp localfuncs ifacename)
      (save-excursion
	(goto-char (car bounds))
	;; parse interface section
	(while (re-search-forward "^\\s-*interface\\(?:\\s-+\\(\\sw+\\)\\)?\\s-*$" (cadr bounds) t)
	  (setq ifacename (match-string-no-properties 1))
	  (forward-line)
	  (beginning-of-line)
	  (let ((ifacestart (point))
		(ifaceend
		 (progn (re-search-forward "^\\s-*end\\s-+interface\\s-*\\(\\sw+\\)?\\s-*$" (cadr bounds) t)
			(match-beginning 0))))
	    (when ifaceend
	      (setq tmp (semantic-parse-region ifacestart ifaceend))))
	  (when ifacename
	    (dolist (cur tmp)
	      (semantic-tag-set-name cur ifacename)))
	  (setq localfuncs (append localfuncs tmp)))
	(setq localfuncs (append localfuncs
				 (semantic-f90-parse-contains tag))))
      (when localfuncs
	(semantic-tag-put-attribute tag :members
				    (append (semantic-tag-get-attribute tag :members)
					    localfuncs))))
    (list tag))

(defun semantic-f90-parse-contains (tag)
  "Search TAG for a 'contains' section and return its tags."
  (let ((bounds (semantic-tag-bounds tag)))
    (save-excursion
      (goto-char (car bounds))
      (when (re-search-forward "^\\s-*contains\\s-*$" (cadr bounds) t)
	(forward-line)
	(beginning-of-line)
	(semantic-parse-region (point) (cadr bounds))))))

;;; Override methods & Variables
;;
(define-mode-local-override semantic-end-of-command f90-mode ()
  ""
  (beginning-of-line)
  (condition-case nil
      (while (looking-at ".*&\\s-*$")
	(forward-line 1))
    (error t))
  (end-of-line))

(define-mode-local-override semantic-beginning-of-command f90-mode ()
  ""
  (beginning-of-line)
  (condition-case nil
      (while
	  (progn
	    (forward-line -1)
	    (looking-at ".*&\\s-*$")))
    (error t))
  (forward-line 1))


(defcustom-mode-local-semantic-dependency-system-include-path
  f90-mode semantic-makefile-dependency-system-include-path
  nil
  "The system include path used by f90 langauge.")

;; Not sure the following movement functions are really needed, but we
;; keep them for now.

(define-mode-local-override semantic-get-local-variables f90-mode (&optional point)
  "Get the local variables based on POINT's context."
  (if point (goto-char point))
  (let* ((tag (or (semantic-current-tag-of-class 'program)
		  (semantic-current-tag-of-class 'function)
		  (semantic-current-tag-of-class 'subroutine))))
    (when tag
      (semantic-f90-get-local-variables tag))
    ))

(defun semantic-f90-get-local-variables (tag)
  "Get local variables from the spec-part of current TAG.
TAG has to be of class 'program, 'function or 'subroutine."
  (save-excursion
    (goto-char (car (semantic-tag-bounds tag)))
    (semantic-end-of-command)
    (forward-line)
    (beginning-of-line)
    (let ((start (point)))
      (semantic-f90-skip-specs)
      (semantic-parse-region start (point) 'bovine-inner-scope nil t))))

(defun semantic-f90-get-type-members (tag)
  "Get type members of dervived type given in TAG."
  (let ((bounds (semantic-tag-bounds tag)))
    (save-excursion
      (goto-char (car bounds))
      (forward-line)
      (beginning-of-line)
      (let ((start (point)))
	(when (re-search-forward "^\\s-*end\\s-+type" (cadr bounds) t)
	  (beginning-of-line)
	  (semantic-parse-region start (point) 'bovine-inner-scope nil t))))))

(define-mode-local-override semantic-ctxt-current-class-list f90-mode (point)
  "Return a list of tag classes that are allowed at POINT.
If POINT is nil, the current buffer location is used."
  (cond
   ((looking-back "call\\s-+\\sw*")
    '(subroutine))
   ((looking-at ".*=")
    '(variable))
   ((looking-back "type([^)]*")
    '(type))
   ((looking-back "\\sw+([^)]*")
    '(variable function))
   (t
    '(function type variable))))

(define-mode-local-override semantic-tag-protection f90-mode (tag &optional parent)
  "Return protection information about TAG with optional PARENT.
This function is currently a stub and always returns 'public for
  F90."  
  'public)


(define-mode-local-override semantic-parse-region f90-mode
  (start end &optional nonterminal depth returnonerror)
  "Parse the area between START and END, and return any tags found.
If END needs to be extended due to a lexical token being too large, it
will be silently ignored."
  ;; Hack: We'll first scan for 'use' statements and prepend them as
  ;; includes to the buffer's tags, so that they will be parsed by the
  ;; idle work function.
  (let (tags modulenames)
    (save-excursion
      (goto-char start)
      (while (re-search-forward "^\\s-*use\\s-+\\(\\sw+\\)" end t)
	(unless (member (match-string-no-properties 1) modulenames)
	  (setq tags
		(append tags
			(list (semantic-tag-new-include
			       (concat (match-string-no-properties 1) ".f90") nil))))
	  (setq modulenames (append modulenames (list (match-string-no-properties 1))))
	  )))
    ;; Now call the default
    (setq tags
	  (append tags
		  (semantic-parse-region-default start end
						 nonterminal depth
						 returnonerror)))))

(define-mode-local-override semantic-analyze-scoped-types f90-mode (pos)
  "Return a list of types currently in scope at POSITION.
This is based on what tags exist at POSITION, and any associated
types available."
  (save-excursion
    (goto-char pos)
    (let* ((tag (or (semantic-current-tag-of-class 'program)
		    (semantic-current-tag-of-class 'function)
		    (semantic-current-tag-of-class 'subroutine)))
	   (mods (semantic-tag-get-attribute tag :usedmodules))
	   (bounds (semantic-tag-bounds tag))
	   type)
      ;; search for CONTAINS and also parse that stuff there
      (save-excursion
	(when (re-search-forward "^\\s-*contains\\s-*$" (cadr bounds) t)
	  (setq type
		(append type (semantic-parse-region
			      (match-end 0) (cadr bounds))))))
      ;; add module members
      (dolist (cur mods)
	(let* ((name (semantic-tag-name cur))
	       (findres (semanticdb-find-tags-by-name name)))
	  (setq type
		(append type (semantic-tag-get-attribute
			      (car (semanticdb-find-result-nth findres 0))
			      :members)))))
      type)))


;; In addition to functions, also highlight subroutines, modules,
;; types and programs

(define-semantic-decoration-style semantic-tag-f90-boundary
  "Place an overline on every type, subroutine and function."
  :enabled nil)

(defun semantic-tag-f90-boundary-p-default (tag)
  "Return non-nil if TAG should be highlighted."
  (let ((c (semantic-tag-class tag)))
    (or
     (eq c 'type)
     (eq c 'module)
     (eq c 'function)
     (eq c 'program)
     (eq c 'subroutine))))

(defalias 'semantic-tag-f90-boundary-highlight-default
  'semantic-tag-boundary-highlight-default)

(define-mode-local-override semantic-tag-components f90-mode (tag)
  "Return a list of components for TAG.
A Component is a part of TAG which itself may be a TAG."
  (cond ((semantic-tag-of-class-p tag 'type)
	 (semantic-tag-type-members tag))
	((or (semantic-tag-of-class-p tag 'function)
	     (semantic-tag-of-class-p tag 'subroutine))
	 (append (semantic-tag-function-arguments tag)
		 (semantic-tag-get-attribute tag :members)))
	((or (semantic-tag-of-class-p tag 'program)
	     (semantic-tag-of-class-p tag 'module))
	 (semantic-tag-get-attribute tag :members))
	(t nil)))

;;; Setup function
;;
;;;###autoload
(defun semantic-default-f90-setup ()
  "Set up a buffer for semantic parsing of the F90 language."
  (semantic-f90-by--install-parser)
  (setq semantic-lex-syntax-modifications
	'((?% ".")
	  (?> ".")
	  (?& ".")))
  ;; Commented out lines below are generally considered optional
  ;; See the Emacs Doc for the symbols used below
  (setq semantic-symbol->name-assoc-list '( (variable . "Variables")
                                            (type     . "Types")
                                            (function . "Functions")
                                            (include  . "Includes")
                                            (package  . "Exports"))
        semantic-tag-expand-function 'semantic-f90-expand-tag
        ;;semantic-lex-extensions semantic-lex-f90-extensions
        ;;semantic-dependency-include-path semantic-default-f90-path
        imenu-create-index-function 'semantic-create-imenu-index
        semantic-type-relation-separator-character '("%")
        semantic-command-separation-character "\n"
        ;; Semantic navigation inside 'type children
        senator-step-at-tag-classes '(function variable)
        )
  (setq semantic-lex-analyzer #'semantic-f90-lexer)
  ;; enable our f90 tag-bounds highlighter instead of the default one
  (semantic-toggle-decoration-style "semantic-tag-boundary" nil)
  (semantic-toggle-decoration-style "semantic-tag-f90-boundary" t)
  )

(provide 'semantic/f90)

;; Local variables:
;; generated-autoload-file: "../loaddefs.el"
;; generated-autoload-load-name: "semantic/bovine/f90"
;; End:

;;; semantic/bovine/f90.el ends here
