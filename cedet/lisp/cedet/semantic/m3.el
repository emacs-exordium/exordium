;;; semantic/m3.el --- Utilities for CEDET/M3
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
;; CEDET/M3 is the mouse-3 menu.
;;
;; These functions allow semantic to "add" interesting items to the m3
;; menu.

(require 'cedet-m3)
(require 'semantic/analyze)
(require 'semantic/analyze/refs)
(require 'semantic/idle)

;;; Code:

;;
;; @TODO - FOR FILE RENAME - The below hooks should be added/removed in
;;         semantic-mode on buffer init, but make the hooks buffer-local.
;;
;;;###autoload
(defun semantic-m3-install ()
  "Install ourselves into the `cedet-m3' system."
  (interactive)
  (add-hook 'cedet-m3-menu-query-hooks 'semantic-m3-add-whatisit nil nil)
  (add-hook 'cedet-m3-menu-visit-hooks 'semantic-m3-ref-items nil nil)
  (add-hook 'cedet-m3-menu-completions-hooks 'semantic-m3-context-items nil nil)
  )

;;; WHATISIT?
;;
;; Look at the local syntax.  What is it?
;;;###autoload
(defun semantic-m3-add-whatisit ()
  "Return a menu item for the 'whatisit' function."
  (list (cedet-m3-menu-item "What is this?"
			    'semantic-m3-whatisit
			    :active t)))

(defun semantic-m3-print-tagstack (tagstack)
  "Print out the current tag stack.
Argument TAGSTACK is the list of tags under point."
  (let* ((prevtag nil))
    (if (not tagstack)
	(princ "\nThere are no tags under point.\n")
      
      (princ "\nPoint is in the following tag")
      (when (> (length tagstack) 1) (princ "s"))
      (princ "\n")

      (while tagstack
	(princ "\n")
	(princ (semantic-format-tag-summarize  (car tagstack) prevtag t))
	(princ "\nRaw: ")
	(prin1 (car tagstack))
	(princ "\n")

	;; Increment.
	(setq prevtag (car tagstack)
	      tagstack (cdr tagstack)
	      )))))

(defun semantic-m3-whatisit ()
  "Try and explain what this is.
The what is under the cursor."
  (interactive)
  (semanticdb-without-unloaded-file-searches
      (let* ((tagstack (semantic-find-tag-by-overlay))
	     (ctxt (semantic-analyze-current-context))
	     (pf (when ctxt (oref ctxt prefix)))
	     (rpf (reverse pf))
	     )
	(with-output-to-temp-buffer (help-buffer)
	  (with-current-buffer standard-output
	    (if (not ctxt)
		(progn
		  ;; @TODO - what might we say about that location?
		  (princ "You have selected some uninteresting location:\n")
		  (princ "   whitespace, punctuation, comment, or string content.\n")
		  (semantic-m3-print-tagstack tagstack)
		  )

	      ;; Found something
	      (princ "You have found ")
	      (cond

	       ;; RAW String - unknown symbol
	       ((stringp (car rpf))
		(let ((comp (save-current-buffer
			      (set-buffer (oref ctxt :buffer))
			      (condition-case nil
				  (save-excursion
				    (semantic-analyze-possible-completions ctxt))
				(error nil)))))
		  (princ "the text ")
		  (princ (car rpf))
		  (princ "\n\n")
		  (if (not comp)
		      (princ "There are no known completions.")
		    (if (cdr comp)
			(progn
			  (princ "There are ")
			  (prin1 (length comp))
			  (princ " possible completions:\n"))
		      (princ "There is one possible completion:\n"))

		    (dolist (C comp)
		      (princ "   ")
		      (princ (semantic-format-tag-summarize C))
		      (princ "\n"))
		    )))

	       ;; A Semantic Tag
	       ((semantic-tag-p (car rpf))
		(princ "the symbol:\n  ")
		(princ (semantic-format-tag-summarize (car rpf)
						      (car (cdr rpf))
						      t))
		(princ "\n\n")

		 
		;; Filename
		(when (semantic-tag-file-name (car rpf))
		  (princ "This tag is found in:\n  ")
		  (princ (semantic-tag-file-name (car rpf)))
		  (let ((line (semantic-tag-get-attribute (car rpf) :line))
			(start (when (semantic-tag-with-position-p (car rpf))
				 (semantic-tag-start (car rpf)))))
		    (cond (line
			   (princ "\n  on Line: ")
			   (princ line))
			  (start
			   (princ "\n  at character: ")
			   (princ start))
			  ))
		  (princ "\n\n"))
	    
		;; Raw Tag Data
		(princ "The Raw Tag Data Structure is:\n\n")
		(prin1 (car rpf))
		(princ "\n")
		(semantic-m3-print-tagstack tagstack)
		)

	       ;; Something else?
	       (t

		(princ "absolutely nothing...")

		))))))))

;;;###autoload
(defun semantic-m3-context-items ()
  "Return a list of menu items if the cursor is on some useful code constrct."
  (semanticdb-without-unloaded-file-searches
      (save-excursion
	(let* ((ctxt (semantic-analyze-current-context))
	       (prefix (when ctxt (oref ctxt :prefix)))
	       (sym (car (reverse prefix)))
	       (completions (when (and ctxt (semantic-idle-summary-useful-context-p))
			      (condition-case nil
				  (semantic-analyze-possible-completions ctxt)
				(error nil))))
	       (tag (semantic-current-tag))
	       (items nil)
	       )
	  ;; If there is a context and bounds, then pulse the symbol
	  (when (and ctxt (oref ctxt :bounds))
	    (let ((pulse-flag nil))
	      (pulse-momentary-highlight-region (car (oref ctxt :bounds))
						(cdr (oref ctxt :bounds))))

	    ;; If there are completions, then add some in.
	    ;; Don't use completions if there is only one, and SYM
	    ;; is a tag.
	    (when (and completions (or (> (length completions) 1)
				       (stringp sym)))
	      (dolist (T (reverse completions))
		(push (cedet-m3-menu-item
		       (concat "==> " (semantic-format-tag-name T))
		       `(lambda () (interactive)
			  (semantic-m3-complete-from-menu (quote ,T)))
		       :active t)
		      items)))
	    ;; If there were no completions, do non-completion like things
	    (when (or (not completions) (= (length completions) 1))

	      ;; If this symbol is purely local, we can do a mini refactor.
	      ;; with semantic-symref-rename-local-variable
	      (when (and (semantic-tag-p sym)
			 (semantic-tag-of-class-p sym 'variable)
			 (semantic-tag-with-position-p sym)
			 ;; within this tag
			 (or (> (semantic-tag-start sym) (semantic-tag-start tag))
			     (< (semantic-tag-end sym) (semantic-tag-end tag)))
			 ;; within this buffer
			 (or (not (semantic-tag-buffer sym))
			     (eq (semantic-tag-buffer sym) (current-buffer)))
			 )
		(push (cedet-m3-menu-item
		       (concat "Rename local variable: " (semantic-format-tag-name sym))
		       'semantic-symref-rename-local-variable
		       :active t
		       :help "Rename the local value using field edits.")
		      items))
	  
	      ;; Symref lookups
	      (let ((str (semantic-format-tag-name-from-anything sym)))
		(push (cedet-m3-menu-item
		       (concat "Symref Lookup: " str)
		       `(lambda () (interactive)
			  (semantic-symref-symbol ,str))
		       :active t)
		      items))

	      ;; Offer the JUMP TO option iff there is a thing to jump to
	      (when (semantic-tag-with-position-p sym)
		(let ((fn (semantic-tag-file-name sym)))
		  (push (cedet-m3-menu-item
			 (concat "Jump to symbol: " (semantic-format-tag-name sym))
			 'semantic-ia-fast-jump
			 :active t
			 :help "Jump to the current symbol.")
			items)))

	      ))
	  items))))

;;;###autoload
(defun semantic-m3-ref-items ()
  "Return a list of menu items for dealing with analyzer refs."
  (semanticdb-without-unloaded-file-searches
      (save-excursion
	(let* ((sym (semantic-current-tag))
	       (sar (if sym
			(semantic-analyze-tag-references sym)
		      nil))
	       (target nil)
	       (items nil))
	  (when sar
	    (setq target
		  (if (semantic-tag-prototype-p sym)
		      (car (semantic-analyze-refs-impl sar t))
		    (car (semantic-analyze-refs-proto sar t))))
	    (when target
	      ;; We have something, so offer to go there.
	      (push (cedet-m3-menu-item
		     (concat "Jump to "
			     (if (semantic-tag-prototype-p target)
				 "prototype" "impl")
			     ": " (semantic-format-tag-name sym))
		     'semantic-analyze-proto-impl-toggle
		     :active t
		     :help "Jump to the current symbol.")
		    items)
	      ))))))

(defun semantic-m3-complete-from-menu (tag)
  "Complete the item under point with TAG."
  ;; getting the context here may seem like extra work, but buffer
  ;; caching prevents this from being a big problem. easy
  (semanticdb-without-unloaded-file-searches
      (let* ((ctxt (semantic-analyze-current-context))
	     (bounds (when ctxt (oref ctxt :bounds)))
	     )
	(when (and ctxt bounds)
	  (delete-region (car bounds) (cdr bounds))
	  (goto-char (car bounds))
	  (insert (semantic-format-tag-name tag)))
	)))


(provide 'semantic/m3)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "semantic/m3"
;; End:

;;; semantic-m3.el ends here
