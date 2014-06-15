;;; srecode/m3.el --- Template insertion options for CEDET M3
;;
;; Copyright (C) 2011 Eric M. Ludlam
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
;; Context sensitive template insertion suggestions for cedet-m3.

(require 'cedet-m3)
(require 'srecode)
(require 'srecode/ctxt)
(require 'srecode/semantic)
(require 'srecode/document)

;;; Code:

;;;###autoload
(defun srecode-m3-items ()
  "Return a list of menu items based on SRecode features."
  (save-excursion
    (let ((sr-ctxt (srecode-calculate-context))
	  (sym (semantic-current-tag))
	  (items nil)
	  )
      (when (and sym (semantic-tag-of-class-p sym 'function))
	(push (cedet-m3-menu-item
	       (concat "Comment " (semantic-tag-name sym))
	       'srecode-document-insert-function-comment
	       :active t
	       :help "Write/replace a comment for this tag.")
	 items)
	)
      
      )))


(provide 'srecode/m3)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "srecode/m3"
;; End:

;;; srecode-m3.el ends here
