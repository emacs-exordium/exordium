;;; semantic-ectag-scala.el --- Scala support for ctags
;;
;; Copyright (C) 2009 Raymond Paul Racine
;
;;This program is free software; you can redistribute it and/or
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
;; The file contains Exuberent CTags support for the Scala language.

;;; Code:

;;; Scala Mode
;;

(defvar-mode-local scala-mode semantic-ectag-lang "scala"
  "Language name for Exuberent CTags.")

(defvar-mode-local scala-mode semantic-ectag-lang-kind
  "cotmnafVvTip"
  "Kinds of Exuberent CTags available.")

;; FIXME - Need to skip commented code.
;; i.e., --regex-Scala=/^[^\*\/]*class[ \t]*([a-zA-Z0-9_]+)/\1/c,classes/
(defvar-mode-local scala-mode semantic-ectag-lang-extra-flags
  '("--langdef=scala"
    "--langmap=scala:.scala"
    "--regex-Scala=/^[ \t]*class[ \t]*([a-zA-Z0-9_]+)/\\1/c,class/"
    "--regex-Scala=/^[ \t]*object[ \t]*([a-zA-Z0-9_]+)/\\1/o,class/"
    "--regex-Scala=/^[ \t]*class[ \t]*([a-zA-Z0-9_]+)/\\1/c,class/"
    "--regex-Scala=/^[ \t]*object[ \t]*([a-zA-Z0-9_]+)/\\1/o,class/"
    "--regex-scala=/^[ \t]*trait[ \t]*([a-zA-Z0-9_]+)/\\1/t,class/"
    "--regex-Scala=/^[ \t]*case[ \t]*class[ \t]*([a-zA-Z0-9_]+)/\\1/m,class/"
    "--regex-Scala=/^[ \t]*case[ \t]*object[ \t]*([a-zA-Z0-9_]+)/\\1/n,class/"
    "--regex-Scala=/^[ \t]*abstract[ \t]*class[ \t]*([a-zA-Z0-9_]+)/\\1/a,class/"
    "--regex-Scala=/^[ \t]*def[ \t]*([a-zA-Z0-9_]+)[ \t]*.*[:=]/\\1/f,function/"
    "--regex-Scala=/[ \t]*val[ \t]*([a-zA-Z0-9_]+)[ \t]*[:=]/\\1/V,value/"
    "--regex-Scala=/[ \t]*var[ \t]*([a-zA-Z0-9_]+)[ \t]*[:=]/\\1/v,variable/"
    "--regex-Scala=/^[ \t]*type[ \t]*([a-zA-Z0-9_]+)[ \t]*[\[<>=]/\\1/T,classs/"
    "--regex-Scala=/^[ \t]*import[ \t]*([a-zA-Z0-9_{}., \t=>]+$)/\\1/i,include/"
    "--regex-Scala=/^[ \t]*package[ \t]*([a-zA-Z0-9_.]+$)/\\1/p,package/")
  "Regex for Scala symbols from syntax.")

(defvar-mode-local scala-mode semantic-symbol->name-assoc-list
  '((type . "Types")
    (variable . "Variables")
    (value . "Values")
    (function . "Functions")
    (include . "Dependencies")
    (package . "Providers"))
  "List of tag classes and describing strings.")

;; CEDET has lots of ways of getting you to various levels of semantic parsing support.
;; The following incantation works.
(defun semantic-scala-cedet-support ()
  "Enable CEDET for Scala based upon exuberant ctags support as the primary parser."
  (interactive)
  (semantic-load-enable-code-helpers)
  (global-srecode-minor-mode 1)
  (semantic-load-enable-primary-exuberent-ctags-support)
  (add-hook 'scala-mode-hook 'semantic-ectag-simple-setup))

(semantic-scala-cedet-support)

;;(add-hook 'scala-mode-hook 'semantic-scala-cedet-support)

(provide 'semantic-ectag-scala)

;;; semantic-ectag-scala.el ends here
