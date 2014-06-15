;;; calc-wy.el --- Generated parser support file

;; Copyright (C) 2002, 2003, 2009 David Ponce

;; Author: Philippe <phil@citrus.home>
;; Created: 2014-06-05 21:32:39-0400
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
;; generated from the grammar file calc.wy.

;;; History:
;;

;;; Code:

(require 'semantic/lex)
(eval-when-compile (require 'semantic/bovine))

;;; Prologue
;;
(declare-function wisent-calc-= "semantic/wisent/calc")
(declare-function wisent-calc-not "semantic/wisent/calc")
(declare-function wisent-calc-factorial "semantic/wisent/calc")

;;; Declarations
;;
(defconst wisent-calc-wy--keyword-table
  (semantic-lex-make-keyword-table 'nil 'nil)
  "Table of language keywords.")

(defconst wisent-calc-wy--token-table
  (semantic-lex-make-type-table
   '(("number"
      (NUM)))
   'nil)
  "Table of lexical tokens.")

(defconst wisent-calc-wy--parse-table
  (progn
    (eval-when-compile
      (require 'semantic/wisent/comp))
    (wisent-compile-grammar
     '((NUM)
       ((nonassoc 61)
	(left 45 43)
	(left 42 47)
	(left NEG)
	(left FACT)
	(left NOT)
	(right 94))
       (input
	((line))
	((input line)
	 (format "%s %s" $1 $2)))
       (line
	((59)
	 (progn ";"))
	((exp 59)
	 (format "%s;" $1))
	((error 59)
	 (progn "Error;")))
       (exp
	((NUM)
	 (string-to-number $1))
	((exp 61 exp)
	 (wisent-calc-= $1 $3))
	((126 exp)
	 [NOT]
	 (wisent-calc-not $2))
	((exp 43 exp)
	 (+ $1 $3))
	((exp 45 exp)
	 (- $1 $3))
	((exp 42 exp)
	 (* $1 $3))
	((exp 47 exp)
	 (/ $1 $3))
	((45 exp)
	 [NEG]
	 (- $2))
	((33 exp)
	 [FACT]
	 (wisent-calc-factorial $2))
	((exp 94 exp)
	 (expt $1 $3))
	((40 exp 41)
	 (progn $2))))
     'nil))
  "Parser table.")

(defun wisent-calc-wy--install-parser ()
  "Setup the Semantic Parser."
  (semantic-install-function-overrides
   '((parse-stream . wisent-parse-stream)))
  (setq semantic-parser-name "LALR"
	semantic--parse-table wisent-calc-wy--parse-table
	semantic-debug-parser-source "calc.wy"
	semantic-flex-keywords-obarray wisent-calc-wy--keyword-table
	semantic-lex-types-obarray wisent-calc-wy--token-table)
  ;; Collect unmatched syntax lexical tokens
  (semantic-make-local-hook 'wisent-discarding-token-functions)
  (add-hook 'wisent-discarding-token-functions
	    'wisent-collect-unmatched-syntax nil t))


;;; Analyzers
;;

;;; Epilogue
;;
(defun wisent-calc-setup-parser ()
  "Setup buffer for parse."
  (wisent-calc-wy--install-parser)
  (setq semantic-number-expression
        (concat "\\([0-9]+\\([.][0-9]*\\)?\\([eE][-+]?[0-9]+\\)?"
                "\\|[.][0-9]+\\([eE][-+]?[0-9]+\\)?\\)")
        semantic-lex-analyzer 'wisent-calc-lexer
        semantic-lex-depth nil
        semantic-lex-syntax-modifications
        '((?\; ".") (?\= ".") (?\+ ".")
          (?\- ".") (?\* ".") (?\/ ".")
          (?^ ".") (?\( ".") (?\) ".")
	  (?! ".") (?~ ".")
          )
        )
  )

(provide 'semantic/wisent/calc-wy)

;;; calc-wy.el ends here
