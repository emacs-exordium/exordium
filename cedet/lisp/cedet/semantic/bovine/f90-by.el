;;; f90-by.el --- Generated parser support file

;; Copyright (C) 2010 Free Software Foundation

;; Author: Philippe <phil@citrus.home>
;; Created: 2014-06-05 21:33:03-0400
;; Keywords: syntax
;; X-RCS: $Id$

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; PLEASE DO NOT MANUALLY EDIT THIS FILE!  It is automatically
;; generated from the grammar file f90.by.

;;; History:
;;

;;; Code:

(require 'semantic/lex)
(eval-when-compile (require 'semantic/bovine))

;;; Prologue
;;

;;; Declarations
;;
(defconst semantic-f90-by--keyword-table
  (semantic-lex-make-keyword-table
   '(("end" . END)
     ("subroutine" . SUBROUTINE)
     ("function" . FUNCTION)
     ("module" . MODULE)
     ("use" . USE)
     ("contains" . CONTAINS)
     ("interface" . INTERFACE)
     ("integer" . INT)
     ("real" . REAL)
     ("char" . CHAR)
     ("logical" . LOGICAL)
     ("complex" . COMPLEX)
     ("parameter" . PARAMETER)
     ("allocatable" . ALLOCATABLE)
     ("dimension" . DIMENSION)
     ("external" . EXTERNAL)
     ("intent" . INTENT)
     ("intrinsic" . INTRINSIC)
     ("optional" . OPTIONAL)
     ("pointer" . POINTER)
     ("save" . SAVE)
     ("target" . TARGET)
     ("implicit" . IMPLICIT)
     ("end" . END)
     ("call" . CALL)
     ("type" . TYPE)
     ("only" . ONLY))
   'nil)
  "Table of language keywords.")

(defconst semantic-f90-by--token-table
  (semantic-lex-make-type-table
   '(("close-paren"
      (RPAREN . ")"))
     ("open-paren"
      (LPAREN . "("))
     ("punctuation"
      (OR . "\\`[|]\\'")
      (HAT . "\\`\\^\\'")
      (PERCENT . "\\`[%]\\'")
      (TILDE . "\\`[~]\\'")
      (COMMA . "\\`[,]\\'")
      (GREATER . "\\`[>]\\'")
      (LESS . "\\`[<]\\'")
      (EQUAL . "\\`[=]\\'")
      (BANG . "\\`[!]\\'")
      (MINUS . "\\`[-]\\'")
      (PLUS . "\\`[+]\\'")
      (DIVIDE . "\\`[/]\\'")
      (AMPERSAND . "\\`[&]\\'")
      (STAR . "\\`[*]\\'")
      (SEMICOLON . "\\`[;]\\'")
      (COLON . "\\`[:]\\'")
      (PERIOD . "\\`[.]\\'")
      (HASH . "\\`[#]\\'")
      (OPERATORS . "[-+*/%^|&]")
      (PERIOD . "\\`[.]\\'")))
   'nil)
  "Table of lexical tokens.")

(defconst semantic-f90-by--parse-table
  `(
    (bovine-toplevel 
     (starting_rule)
     ) ;; end bovine-toplevel

    (bovine-inner-scope 
     (spec-one-part)
     ) ;; end bovine-inner-scope

    (starting_rule
     (punit)
     ) ;; end starting_rule

    (punit
     (PROGRAM
      symbol
      semantic-list
      ,(semantic-lambda
	(semantic-tag
	 (nth 1 vals)
	 'program))
      )
     (SUBROUTINE
      symbol
      semantic-list
      semantic-list
      ,(semantic-lambda
	(semantic-tag
	 (nth 1 vals)
	 'subroutine :arguments
	 (semantic-parse-region
	  (car
	   (nth 2 vals))
	  (cdr
	   (nth 2 vals))
	  'funarg
	  1)))
      )
     (one-or-no-type
      FUNCTION
      symbol
      semantic-list
      semantic-list
      ,(semantic-lambda
	(semantic-tag
	 (nth 2 vals)
	 'function :arguments
	 (semantic-parse-region
	  (car
	   (nth 3 vals))
	  (cdr
	   (nth 3 vals))
	  'funarg
	  1) :type
	 (car
	  (nth 0 vals))))
      )
     (MODULE
      symbol
      semantic-list
      ,(semantic-lambda
	(semantic-tag
	 (nth 1 vals)
	 'module))
      )
     ) ;; end punit

    (local-vars
     (builtin-type
      punctuation
      "\\`[:]\\'"
      punctuation
      "\\`[:]\\'"
      var-decl
      newline
      spec-part)
     (builtin-type
      punctuation
      "\\`[,]\\'"
      attributes
      punctuation
      "\\`[:]\\'"
      punctuation
      "\\`[:]\\'"
      var-decl
      newline
      spec-part)
     ) ;; end local-vars

    (spec-part
     (newline
      ,(semantic-lambda)
      )
     (spec-one-part
      newline
      ,(semantic-lambda
	(nth 0 vals))
      )
     ) ;; end spec-part

    (spec-one-part
     (IMPLICIT
      symbol
      ,(semantic-lambda
	(semantic-tag
	 (nth 1 vals)
	 'implicit))
      )
     (USE
      symbol
      opt-only
      ,(semantic-lambda
	(semantic-tag
	 (nth 1 vals)
	 'use))
      )
     (TYPE
      symbol
      semantic-list
      ,(semantic-lambda
	(semantic-tag
	 (nth 1 vals)
	 'type :kind
	 'derived-type))
      )
     (END
      TYPE
      symbol
      ,(semantic-lambda
	(list nil))
      )
     (END
      TYPE
      ,(semantic-lambda
	(list nil))
      )
     (one-type
      punctuation
      "\\`[:]\\'"
      punctuation
      "\\`[:]\\'"
      var-decl
      ,(semantic-lambda
	(semantic-tag-new-variable
	 (nth 3 vals)
	 (car
	  (nth 0 vals)) nil))
      )
     (one-type
      punctuation
      "\\`[,]\\'"
      attribute-list
      punctuation
      "\\`[:]\\'"
      punctuation
      "\\`[:]\\'"
      var-decl
      ,(semantic-lambda
	(apply
	 'semantic-tag-new-variable
	 (nth 5 vals)
	 (car
	  (nth 0 vals)) nil
	 (nth 2 vals)))
      )
     (one-type
      var-decl
      ,(semantic-lambda
	(semantic-tag-new-variable
	 (nth 1 vals)
	 (car
	  (nth 0 vals)) nil))
      )
     ) ;; end spec-one-part

    (one-or-no-type
     (one-type)
     ( ;;EMPTY
      )
     ) ;; end one-or-no-type

    (one-type
     (single-type
      semantic-list
      ,(semantic-lambda
	(nth 0 vals))
      )
     (single-type)
     ) ;; end one-type

    (single-type
     (derived-type)
     (builtin-type)
     (symbol)
     ) ;; end single-type

    (opt-only
     (punctuation
      "\\`[,]\\'"
      ONLY
      punctuation
      "\\`[:]\\'"
      symbol
      punctuation
      punctuation
      symbol
      ,(semantic-lambda)
      )
     (punctuation
      "\\`[,]\\'"
      ONLY
      punctuation
      "\\`[:]\\'"
      symbol
      ,(semantic-lambda)
      )
     ( ;;EMPTY
      )
     ) ;; end opt-only

    (var-decl
     (single-variable
      punctuation
      "\\`[=]\\'"
      symbol-or-number
      punctuation
      "\\`[,]\\'"
      var-decl
      ,(semantic-lambda
	(append
	 (nth 0 vals)
	 (nth 4 vals)))
      )
     (single-variable
      punctuation
      "\\`[,]\\'"
      var-decl
      ,(semantic-lambda
	(append
	 (nth 0 vals)
	 (nth 2 vals)))
      )
     (single-variable
      punctuation
      "\\`[=]\\'"
      symbol)
     (single-variable)
     ) ;; end var-decl

    (single-variable
     (symbol
      semantic-list
      ,(semantic-lambda
	(list
	 (nth 0 vals)))
      )
     (symbol)
     ) ;; end single-variable

    (builtin-type
     (INT
      ,(semantic-lambda
	(list
	 "integer"))
      )
     (REAL
      ,(semantic-lambda
	(list
	 "real"))
      )
     (DPREC
      ,(semantic-lambda
	(list
	 "double precision"))
      )
     (CHAR
      ,(semantic-lambda
	(list
	 "character"))
      )
     (COMPLEX
      ,(semantic-lambda
	(list
	 "complex"))
      )
     (LOGICAL
      ,(semantic-lambda
	(list
	 "logical"))
      )
     ) ;; end builtin-type

    (derived-type
     (TYPE
      semantic-list
      ,(semantic-lambda
	(semantic-parse-region
	 (car
	  (nth 1 vals))
	 (cdr
	  (nth 1 vals))
	 'dtype
	 1))
      )
     ) ;; end derived-type

    (dtype
     (open-paren
      "("
      ,(semantic-lambda
	(list nil))
      )
     (close-paren
      ")"
      ,(semantic-lambda
	(list nil))
      )
     (symbol
      ,(semantic-lambda
	(semantic-tag
	 (nth 0 vals)
	 'type))
      )
     ) ;; end dtype

    (other-type
     (symbol)
     ( ;;EMPTY
      )
     ) ;; end other-type

    (attribute-list
     (single-attribute
      punctuation
      "\\`[,]\\'"
      attribute-list
      ,(semantic-lambda
	(append
	 (nth 0 vals)
	 (nth 2 vals)))
      )
     (single-attribute)
     ) ;; end attribute-list

    (single-attribute
     (attribute
      semantic-list
      ,(semantic-lambda
	(list
	 (car
	  (nth 0 vals))
	 (downcase
	  (buffer-substring-no-properties
	   (1+
	    (car
	     (nth 1 vals)))
	   (1-
	    (cdr
	     (nth 1 vals)))))))
      )
     (attribute
      ,(semantic-lambda
	(list
	 (car
	  (nth 0 vals))
	 't))
      )
     ) ;; end single-attribute

    (attribute
     (PARAMETER
      ,(semantic-lambda
	(list
	 ':parameter))
      )
     (ALLOCATABLE
      ,(semantic-lambda
	(list
	 ':allocatable))
      )
     (DIMENSION
      ,(semantic-lambda
	(list
	 ':dimension))
      )
     (EXTERNAL
      ,(semantic-lambda
	(list
	 ':external))
      )
     (INTENT
      ,(semantic-lambda
	(list
	 ':intent))
      )
     (INTRINSIC
      ,(semantic-lambda
	(list
	 ':intrinsic))
      )
     (OPTIONAL
      ,(semantic-lambda
	(list
	 ':optional))
      )
     (POINTER
      ,(semantic-lambda
	(list
	 ':pointer))
      )
     (SAVE
      ,(semantic-lambda
	(list
	 ':save))
      )
     (TARGET
      ,(semantic-lambda
	(list
	 ':target))
      )
     ) ;; end attribute

    (funarg
     (open-paren
      "("
      ,(semantic-lambda
	(list nil))
      )
     (punctuation
      "\\`[,]\\'"
      ,(semantic-lambda
	(list nil))
      )
     (close-paren
      ")"
      ,(semantic-lambda
	(list nil))
      )
     (symbol
      ,(semantic-lambda
	(semantic-tag-new-variable
	 (nth 0 vals) nil nil))
      )
     ) ;; end funarg

    (symbol-or-number
     (symbol)
     (number)
     ) ;; end symbol-or-number
    )
  "Parser table.")

(defun semantic-f90-by--install-parser ()
  "Setup the Semantic Parser."
  (setq semantic--parse-table semantic-f90-by--parse-table
	semantic-debug-parser-source "semantic/bovine/f90.by"
	semantic-debug-parser-class 'semantic-bovine-debug-parser
	semantic-debug-parser-debugger-source 'semantic/bovine/debug
	semantic-flex-keywords-obarray semantic-f90-by--keyword-table
	))


;;; Analyzers
;;

;;; Epilogue
;;

(provide 'semantic/bovine/f90-by)

;;; f90-by.el ends here
