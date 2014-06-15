;;; semantic/wisent/calc.el --- Infix notation calculator

;; Copyright (C) 2001, 2002, 2003, 2004, 2009, 2010 David Ponce

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 11 Sep 2001
;; Keywords: syntax

;; This file is not part of GNU Emacs.

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
;; This is a port of the Bison 1.28d Infix Calc sample program to the
;; elisp LALR parser Wisent.  It illustrates usage of operator
;; precedence and contextual precedence.  The grammar is generated
;; from the WY file semantic/wisent/calc.wy.
;;
;; To run the calculator use M-x wisent-calc and at "calc:" prompt
;; enter expressions separated by semicolons.  Here is a sample run of
;; `wisent-calc':
;;
;;   calc: 4 + 4.5 - (34.0/(8*3+-3));
;;   -> 6.880952380952381;
;;   calc: -56 + 2;
;;   -> -54;
;;   calc: 3 ^ 2;
;;   -> 9;
;;   calc: 2*2*2 = 2^3;
;;   -> t;
;;   calc: 2*2*2; 2^3;
;;   -> 8; 8;

;;; Code:
(require 'cedet-compat)
(require 'semantic/wisent)
(require 'semantic/wisent/calc-wy)

(define-lex-simple-regex-analyzer wisent-calc-lex-number
  "Detect and create number tokens."
  semantic-lex-number-expression 'NUM)

(define-lex-simple-regex-analyzer wisent-calc-lex-punctuation
  "Detect and create punctuation tokens."
  "\\(\\s.\\|\\s$\\|\\s'\\)" (char-after))

(define-lex wisent-calc-lexer
  "Calc lexical analyzer."
  semantic-lex-ignore-whitespace
  semantic-lex-ignore-newline
  wisent-calc-lex-number
  wisent-calc-lex-punctuation
  semantic-lex-default-action)

;;;###autoload
(defun wisent-calc (input)
  "Infix desktop calculator.
Parse INPUT string and output the result of computation."
  (interactive "scalc: ")
  (or (string-match ";\\s-*$" input)
      (setq input (concat input ";")))
  (with-temp-buffer
    (wisent-calc-setup-parser)
    (semantic-lex-init)
    (insert input)
    (let* ((wisent-lex-istream (semantic-lex-buffer))
	   (answer (wisent-parse semantic--parse-table 'wisent-lex)))
      (if (called-interactively-p 'interactive)
	  (message "%s -> %s" input answer))
      answer)))

;; Misc handy fcns
(defun wisent-calc-factorial (integer)
  "Compute factorial of an INTEGER.
Borrowed from the Emacs manual."
  (if (= 1 integer) 1
    (* integer (wisent-calc-factorial (1- integer)))))

(defun wisent-calc-not (num)
  "Compute a NOT operation of an NUMber.
If NUM is 0, return 1. If NUM is not 0, return 0."
  (if (= 0 num) 1 0))

(defun wisent-calc-= (num1 num2)
  "Compute a if NUM1 equal NUM2.
Return 1 if equal, 0 if not equal."
  (if (= num1 num2) 1 0))

;;; TEST SUITE:
(defvar wisent-calc-test-expressions
  '(
    ;; Basic
    ("1+1" . 2) ("2*2" . 4) ("12/3" . 4) ("3-2" . 1)
    ("2^3" . 8) ("!4" . 24) ("2=3" . 0)  ("2=2" . 1)
    ("~0" . 1)  ("~1" . 0)
    ;; Precidence
    ("1+2*3" . 7) ("1+2-1" . 2) ("6/2+1" . 4) ("2^2+1" . 5)
    ("-3+2" . -1) ("-3*2" . -6) ("!3*2" . 12) ("2+2=4" . 1)
    ("~2=0" . 1)
    ;; grouping
    ("(2+3)*2" . 10) ("2*(4-3)" . 2) ("1+2^(2+1)" . 9) ("~(2=0)" . 1)
    ;; Misc goofy
    ("1+2*4-7/4^3" . 9)
    )
  "List of expressions and answers to test all the features.")

;;;###autoload
(defun wisent-calc-utest ()
  "Test the wisent calculator."
  (interactive)
  (dolist (X wisent-calc-test-expressions)
    (let* ((exp (car X))
	   (ans (cdr X))
	   (act  (string-to-number (wisent-calc exp))))
      (when (not (eq act ans))
	(error "Failed: %S == %d, but should be %d"
	       exp act ans))))
  )

(provide 'semantic/wisent/calc)

;; Local variables:
;; generated-autoload-file: "../loaddefs.el"
;; generated-autoload-load-name: "semantic/wisent/calc"
;; End:

;;; semantic/wisent/calc.el ends here
