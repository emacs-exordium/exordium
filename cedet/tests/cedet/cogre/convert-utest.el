;;; convert.el --- 
;;
;; Copyright (C) 2011 Eric M. Ludlam
;;
;; Author: Eric M. Ludlam <eric@siege-engine.com>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see http://www.gnu.org/licenses/.

;;; Commentary:
;;
;; 

;;; Code:

;;; TESTS
;;
(require 'cogre/periodic)
(require 'cogre/convert)

;;;###autoload
(defun cogre-export-utest ()
  "Run all the COGRE structured export/convert test."
  (interactive)
  (cogre-export-dot-utest)
  ;;(cogre-export-typed-lang-utest)
  )

(defun cogre-export-dot-utest ()
  "Run the COGRE structured dot output converter test.
Basic DOT doesn't require much, so we'll use the periodic
table as an example."
  (interactive)

  ;; Step one, create the graph.
  (if (get-buffer "*Graph Periodic*")
      (switch-to-buffer "*Graph Periodic*")
    (cogre-periodic))
  ;; Step 2, convert.
  (message "Converting graph %s to DOT structure." (oref cogre-graph name))
  (let* ((graphtag (cogre-export-dot-method cogre-graph))
	 (members (semantic-tag-get-attribute graphtag :members))
	 )
    
    (when (not graphtag)
      (error "Conversions failed to make anything"))

    (when (not (string= (semantic-tag-name graphtag) "Periodic"))
      (error "Converted graph has wrong name: %S" (semantic-tag-name graphtag)))
    (when (not (semantic-tag-of-class-p graphtag 'digraph))
      (error "Converted graph is not a digraph"))

    (let ((N cogre-periodic-node-name-list)
	  (L cogre-periodic-link-connectivity-list)
	  )
      (while members

	(let* ((M (car members))
	       (n (semantic-tag-name M)))

	  (cond ((semantic-tag-of-class-p M 'node)
		 (if (string= n (car (car N)))
		     (setq N (cdr N))
		   (error "Unexpected node %S in conversion" n))
		 )
		((semantic-tag-of-class-p M 'link)
		 ;; Links go backward from COGRE to dot.
		 (if (string= (semantic-tag-get-attribute M :to)
			      (car (car L)))
		     (setq L (cdr L))
		   (message "Expected link %S to %S"
			    (car (car L)) (car (cdr (car L))))
		   (error "Unexpected link from %S to %S in conversion"
			  n (semantic-tag-get-attribute M :to)))
		 )
		(t
		 (error "Unknown dot tag %S" M)))

	  )
	(setq members (cdr members)))
      )

    (message "Graph Conversion to DOT success.")))


(provide 'cedet/cogre/convert-utest)

;;; convert.el ends here
