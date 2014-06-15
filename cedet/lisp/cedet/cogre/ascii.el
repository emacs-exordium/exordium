;;; cogre/ascii.el --- Export a cogre diagram to ASCII.
;;
;; Copyright (C) 2009 Eric M. Ludlam
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
;; Export a cogre diagram into all ASCII art.

;;; Code:

(require 'cogre/uml)
(require 'picture)

;;;###autoload
(defun cogre-export-ascii ()
  "Export the current diagram into an ASCII buffer."
  (interactive)
  (when (not (eieio-object-p cogre-graph)) (error "No graph to export"))

  (let* ((g cogre-graph)
	 (name (oref g name))
	 )

    (switch-to-buffer (get-buffer-create (concat "*ASCII Graph " name "*")))
    (erase-buffer)
    (kill-all-local-variables)
    
    ;; Reset the drawing routines.
    (let ((cogre-node-rebuild-method 'cogre-node-rebuild-ascii)
	  ;; Need this for rendering.
	  (cogre-graph g)
	  )

      ;; Force the redraw
      (cogre-render-buffer g t))
    ))

(defun cogre-horizontal-box-line (width)
  "Return a string that can be the top or bottom of a box with a line.
The line will be WIDTH chars long."
  (concat "+"
	  (make-string (- width 2) picture-rectangle-h)
	  "+"))

(defun cogre-string-with-edges (str width align)
  "Return a string based on STR that is WIDTH chars wide.
The string will be justified based on ALIGN.
The string will have a box chars, such as | on either side."
  (when (> (length str) (- width 2))
    ;; If the string is too short, trim it.
    (setq str (substring 0 (- width 2))))
  (when (< (length str) (- width 2))
    ;; String is too short
    (let ((buff (make-string (- width 2 (length str)) ? )))
      (cond ((eq align 'right)
	     (setq str (concat buff str)))
	    (t
	     (setq str (concat str buff))))) )
  (concat (make-string 1 picture-rectangle-v)
	  str
	  (make-string 1 picture-rectangle-v)))

(defmethod cogre-node-rebuild-ascii ((node cogre-node))
  "Create a new value for `:rectangle' in NODE.
The `:rectangle' slot is inserted with rectangle commands.
A Rectangle is basically a list of equal length strings.
Those strings must have the proper face values on them.
Always make the width 2 greater than the widest string."
  (let* ((width (+ (cogre-node-widest-string node) 2))
	 (top-lines (oref node blank-lines-top))
	 (bottom-lines (oref node blank-lines-bottom))
	 (title (cogre-node-title node))
	 (slots (cogre-node-slots node))
	 (align (oref node alignment))
	 (first t)
	 (rect nil))

    (setq rect (cons (cogre-horizontal-box-line width)
		     rect))

    (while (> top-lines 0)
      (setq rect (cons (cogre-string-with-edges "" width align)
		       rect)
	    top-lines (1- top-lines)))

    (setq title (nreverse title))
    (while title
      (setq rect (cons (cogre-string-with-edges (car title) width align)
		       rect)
	    title (cdr title)))
    (while slots
      (let ((sl (car slots)))
	(setq rect (cons (cogre-horizontal-box-line width)
			 rect))
	
	(while sl
	  (setq rect (cons (cogre-string-with-edges (car sl) width align)
			   rect)
		sl (cdr sl))))
      (setq slots (cdr slots)))

    (while (> bottom-lines 0)
      (setq rect (cons (cogre-string-with-edges "" width align)
		       rect)
	    bottom-lines (1- bottom-lines)))

    ;; Bottom of the box.
    (setq rect (cons (cogre-horizontal-box-line width)
		     rect))

    ;; Set the string into our graph node.
    (oset node rectangle (nreverse rect))))


(defmethod cogre-node-rebuild-ascii ((node cogre-package))
  "Create the text rectangle for the COGRE package.
Calls the base method, and takes the return argument and
tweaks the faces."
  (let* ((rect (call-next-method))
	 (first (car rect))
	 )
    ;; Tweak the first and second string if it is long enough.
    (when (> (length first) 7)
      (let* ((backlen (- (length first) 4))
	     (newfirst (concat (cogre-horizontal-box-line 5)
			       (make-string (- backlen 1) ? )))
	     (newsecond (concat (make-string 1 picture-rectangle-v)
				(make-string 3 ? )
				(cogre-horizontal-box-line backlen))))
	(setcar rect newfirst)
	(setcar (cdr rect) newsecond)
	))
    ;; Return it.
    rect))
    
(defmethod cogre-node-rebuild-ascii ((node cogre-note))
  "Create the text rectangle for the COGRE package.
Calls the base method, and takes the return argument and
tweaks the faces."
  (let* ((rect (call-next-method))
	 (first (car rect))
	 (second (car (cdr rect))))
    ;; Tweak the first and second string iff it is long enough.
    (aset first 0 ? )
    (aset first 1 ?,)
    (aset first 2 ?+)
    (setcar rect first)
    (aset second 0 ?+)
    (aset second 1 ?-)
    (aset second 2 ?+)
    (setcar (cdr rect) second)
    ;; Return it.
    rect))

(provide 'cogre/ascii)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "cogre/ascii"
;; End:

;;; cogre/ascii.el ends here
