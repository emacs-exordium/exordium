;;; semantic/ectags/lang.el --- Exuberant Ctags per-language support

;; Copyright (C) 2008, 2009, 2010 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>

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
;; Support various languages via Exuberant CTags.
;;
;; Support requires:
;;  * Specification of tag 'kind' to get
;;  * Signature parsing.

(require 'semantic/fw)
(require 'semantic/ectags/parse)

;;; Code:
(defun semantic-ectags-simple-setup ()
  "Default way to add exuberant ctags support in a language hook.
Any mode that has `semantic-ectags-lang' and `semantic-ectags-lang-kind'
can be support with this simple setup."
  (semantic-ectags-setup-parse-table)
  (setq imenu-create-index-function 'semantic-create-imenu-index))

(defmacro semantic-ectags-add-language-support (mode name kinds)
  "Add simple language support via exuberant ctags.
MODE is the mode to support.
NAME is the exuberant ctags language name.
KINDS are the kinds of tags to generate from exuberant ctags."
  `(progn
       (defvar-mode-local ,mode semantic-ectags-lang ,name
	 "Language name for Exuberant CTags.")
       (defvar-mode-local ,mode semantic-ectags-lang-kind ,kinds
	 "Kinds of Exuberant CTags available.")))

;;; MODE SUPPORT
;;
(semantic-ectags-add-language-support sh-mode "sh" "f")
(semantic-ectags-add-language-support asm-mode "asm" "dlmt")
;(semantic-ectags-add-language-support asp-mode "asp" "cfsv")
;(semantic-ectags-add-language-support awk-mode "awk" "f")
(semantic-ectags-add-language-support basic-mode "basic" "cfltvg")
;(semantic-ectags-add-language-support cobol-mode "cobol" "dfgpPs")
;(semantic-ectags-add-language-support eiffel-mode "eiffel" "cfl")
(semantic-ectags-add-language-support fortran-mode "fortran" "fikpstv") ; L for local variable info.
;(semantic-ectags-add-language-support lua-mode "lua" "f")
(semantic-ectags-add-language-support pascal-mode "pascal" "fp")
(semantic-ectags-add-language-support perl-mode "perl" "cflpsd")
(semantic-ectags-add-language-support python-mode "python" "cfmvi")
;(semantic-ectags-add-language-support rexx-mode "rexx" "s")
;(semantic-ectags-add-language-support sql-mode "sql" "s")
(semantic-ectags-add-language-support tcl-mode "tcl" "cmp")
;(semantic-ectags-add-language-support vera-mode "vera" "cdfgmPTv")
;(semantic-ectags-add-language-support verilog-mode "verilog" "cfm")

;;; BUFFER PARSING HOOKS
;;
;; We cannot blindly enable the buffer support for languages that
;; can only get tags from ectags.  The user must enable them via this
;; fcn instead.

;;;###autoload
(defun semantic-load-enable-primary-ectags-support ()
  "Enable all ectags supported parsers for new languages.
This is support for any language that does not have a regular
semantic parser."
  (interactive)

  ;; Make sure that the version of ectags installed will work.
  (semantic-ectags-test-version)

  ;; Mode Hooks for enabling parsing with ectags as the main parser.
  (add-hook 'sh-mode-hook 'semantic-ectags-simple-setup)

  ;; Support for the following is untested.  Once tested, move up
  ;; to the tested section.
  (add-hook 'asm-mode-hook 'semantic-ectags-simple-setup)
  ;;(add-hook 'basic-mode-hook 'semantic-ectags-simple-setup)
  (add-hook 'fortran-mode-hook 'semantic-ectags-simple-setup)
  ;;(add-hook 'lua-mode-hook 'semantic-ectags-simple-setup)
  (add-hook 'pascal-mode-hook 'semantic-ectags-simple-setup)
  (add-hook 'perl-mode-hook 'semantic-ectags-simple-setup)
  (add-hook 'python-mode-hook 'semantic-ectags-simple-setup)
  ;;(add-hook 'rexx-mode-hook 'semantic-ectags-simple-setup)
  (add-hook 'tcl-mode-hook 'semantic-ectags-simple-setup)
  ;;(add-hook 'vera-mode-hook 'semantic-ectags-simple-setup)
  ;;(add-hook 'verilog-mode-hook 'semantic-ectags-simple-setup)
  )

(semantic-alias-obsolete
 'semantic-load-enable-primary-exuberent-ctags-support
 'semantic-load-enable-primary-ectags-support
 "CEDET 1.2")

(provide 'semantic/ectags/lang)

;; Local variables:
;; generated-autoload-file: "../loaddefs.el"
;; generated-autoload-load-name: "semantic/ectags/lang"
;; End:

;;; semantic/ectags/lang.el ends here
