;;; cogre/wisent-dot.el --- GraphViz DOT parser

;; Copyright (C) 2003, 2004, 2009 Eric M. Ludlam

;; Author: Eric Ludlam <zappo@gnu.org>
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
;; Parser for GraphViz DOT language.
;; The language is declaritive and the whole thing is parsed.
;; The result could be used as a data structure representing a graph.

;; This depends on A dot mode 
;;
;; It will work with either cogre-dot-mode, or if available, the much
;; nicer graphviz-dot-mode by
;;   Pieter E.J. Pareit <pieter.pareit@planetinternet.be>
;;   http://users.skynet.be/ppareit/graphviz-dot-mode.el
;;   


;;; Code:
(require 'semantic/wisent)
(require 'semantic)
(require 'semantic/ctxt)
(require 'cogre/wisent-dot-wy)

(define-mode-local-override semantic-tag-components
  graphviz-dot-mode (tag)
  "Return the children of tag TAG."
  (cond
   ((memq (semantic-tag-class tag)
         '(generic-node graph-attributes node link))
    (semantic-tag-get-attribute tag :attributes)
    )
   ((memq (semantic-tag-class tag)
         '(digraph graph))
    (semantic-tag-get-attribute tag :members)
    )))

;;;###autoload
(defun wisent-dot-setup-parser ()
  "Setup buffer for parse."
  (wisent-dot-wy--install-parser)

  (setq
   ;; Lexical Analysis
   semantic-lex-analyzer 'wisent-dot-lexer
   semantic-lex-syntax-modifications
   '(
     (?- ".")
     (?= ".")
     (?, ".")
     (?> ".")
     (?< ".")
     )
   ;; Parsing
   ;; Environment
   semantic-imenu-summary-function 'semantic-format-tag-name
   imenu-create-index-function 'semantic-create-imenu-index
   semantic-command-separation-character ";"
   ;; Speedbar
   semantic-symbol->name-assoc-list
   '((graph . "Graph")
     (digraph . "Directed Graph")
     (node . "Node")
     )
   ;; Navigation
   senator-step-at-tag-classes '(graph digraph)
   ))

;;;###autoload
(add-hook 'graphviz-dot-mode-hook 'wisent-dot-setup-parser)
;;;###autoload
(add-hook 'cogre-dot-mode-hook 'wisent-dot-setup-parser)

(provide 'cogre/wisent-dot)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "cogre/wisent-dot"
;; End:

;;; cogre/wisent-dot.el ends here
