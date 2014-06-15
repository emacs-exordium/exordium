;;; cogre/convert.el --- Conversion for cogre charts into other formats
;;
;; Copyright (C) 2009, 2010, 2011 Eric M. Ludlam
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
;; Conversions for COGRE charts.
;;
;; A cogre chart is a collection of nodes and lines.  Some charts,
;; such as UML class diagrams, actually represent other kinds of structure.
;;
;; The COGRE-CONVERT utilities is a framework for transforming a graph
;; into some other LISP structure.  Some conversions, such as to ASCII
;; override drawing code.  Conversions transform the data instead.
;;
;; @TODO - re-write below as things are implemented
;; Ideas for conversions:
;;   graph -> semantic tags
;;   graph -> dot tags
;;   graph -> xml for dia, or other case tool
;;
;; Ideas for reverse conversion?
;;   semantic tags   -> graph
;;   xml fir dia     -> graph
;;   parsed dot file -> graph
;;
;;
;; Implementation strategy
;;  My thoughts on getting to the above.
;;  1) Write a straight-up converter to dot.
;;  2) Guess what an abstract converter looks like based on dot
;;  3) Promote the generic API, and write semantic-tag converter.

(require 'cogre)
(require 'cogre/srecode)
(require 'cogre/uml)
(require 'cogre/dot-mode)
(require 'srecode/semantic)
(require 'cedet-graphviz)

(eval-when-compile (require 'ps-print))
;;; Code:
(defvar cogre-export-max-y nil
  "Max y value in the current chart.")

;;;###autoload
(defun cogre-export-dot ()
  "Export the current COGRE graph to DOT notation.
DOT is a part of GraphViz."
  (interactive)
  (when (not (eieio-object-p cogre-graph)) (error "No graph to export"))

  (let* ((g cogre-graph)
	 (name (oref g name))
	 (fname (concat name ".dot"))
	 (ede-auto-add-method 'never)
	 )
    ;; Load in the file
    (switch-to-buffer (find-file fname))
    (erase-buffer)

    ;; Get a parsing mode running here.
    (cogre-dot-mode)
    
    ;; Convert G into this buffer.
    (let* ((graphtag (cogre-export-dot-method g))
	   (members (semantic-tag-get-attribute graphtag :members))
	   (cogre-srecode-current-graph g)
	   )
      ;; Load our tables.
      (cogre-srecode-load-tables)

      ;; Start it out.
      (srecode-insert "file:cogre")

      ;; Insert all the tags.
      (srecode-semantic-insert-tag graphtag)

      (dolist (M members)
	(srecode-semantic-insert-tag M))

      (save-buffer)
      (message "Converted graph into %d dot nodes."
	       (length (semantic-tag-get-attribute graphtag :members)))
      )))

;;;###autoload
(defun cogre-export-dot-png ()
  "Export the current COGRE graph to DOT, then convert that to PNG.
The png file is then displayed in an Emacs buffer.
DOT is a part of GraphVis."
  (interactive)
  ;; Make sure things are installed ok.
  (cedet-graphviz-dot-version-check)
  ;; Run dot to create the file.  The graph was already
  ;; verified.
  (let* ((def (file-name-nondirectory (concat (oref cogre-graph :name)
					      ".png")))
	 (fname (read-file-name "Write to: "
				default-directory nil nil def))
	 (keeplayout (y-or-n-p "Keep current Layout? "))
	 )
    ;; Convert to dot
    (save-window-excursion
      (cogre-export-dot)
      ;; Convert from dot to png
      (if keeplayout
	  (cedet-graphviz-translate-file (current-buffer)
					 (expand-file-name fname)
					 "png"
					 "-n")
	(cedet-graphviz-translate-file (current-buffer)
				       (expand-file-name fname)
				       "png")))

    (let ((ede-auto-add-method 'never))
      (find-file fname))
    ))

(declare-function ps-do-despool "ps-print")

;;;###autoload
(defun cogre-export-dot-postscript-print ()
  "Print the current graph.
This is done by exporting the current COGRE graph to DOT, then
convert that to Postscript before printing.
DOT is a part of GraphVis."
  (interactive)
  (require 'ps-print)
  ;; Make sure things are installed ok.
  (cedet-graphviz-dot-version-check)
  ;; Run dot to create the file.  The graph was already
  ;; verified.
  (let ((keeplayout (y-or-n-p "Keep current Layout? ")))

    ;; Convert to dot
    (cogre-export-dot)

    ;; Convert from dot to postscript
    (with-current-buffer
       (if keeplayout
	   (cedet-graphviz-translate-file (current-buffer)
					  nil
					  "ps"
					  "-n")
	 (cedet-graphviz-translate-file (current-buffer)
					nil
					"ps"))

      (require 'ps-print)
      (let ((ps-spool-buffer (current-buffer)))
	(ps-do-despool nil))
      )))

(defmethod cogre-export-dot-method ((g cogre-base-graph))
  "Convert G into DOT syntax of semantic tags."
  (with-current-buffer (oref g buffer)
    (let ((cogre-export-max-y (count-lines (point-min) (point-max))))
      (semantic-tag (oref g :name)
		    'digraph
		    :members 
		    (cogre-map-elements 'cogre-export-dot-method g)
		    )
      )))

(defun cogre-tag-put-dot-attribute (tag attribute value)
  "Get the attributes in TAG, and set ATTRIBUTE to VALUE.
This works similarly to `semantic-tag-put-attribute'."
  (let* ((lst (semantic-tag-get-attribute tag :attributes))
	 (atag (semantic-find-first-tag-by-name attribute lst)))
    (cond
     ;; If there is one, just change the value.
     (atag
      (semantic-tag-put-attribute atag :value value))
     ;; No list at all.  Make one.
     ((null lst)
      (semantic-tag-put-attribute
       tag :attributes
       (list (semantic-tag attribute 'attribute :value value))))
     ;; Add to the existing list.
     (t
      (add-to-list 'lst
		   (semantic-tag attribute 'attribute :value value)
		   t)))
    tag))

;;; NODES
(defmethod cogre-export-dot-method ((node cogre-node))
  "Convert NODE into DOT syntax of semantic tags."
  (semantic-tag
   (oref node :object-name)
   'node
   :attributes
   (list
    (semantic-tag "shape" 'attribute :value (cogre-export-dot-shape node))
    (semantic-tag "label" 'attribute :value (cogre-export-dot-label node))
    ;; Position in points.
    (semantic-tag "pos" 'attriute :value (cogre-export-dot-pos node))
    )
   )
  )

(defun cogre-calculate-node-position-scale ()
  "Calculate the node-position scale from the default face."
  ;; The height is reported in units of 1/10th of a point.
  (let ((height (face-attribute 'default :height)))
    ;; There is probaly somoething better to do here, like find out if
    ;; the :width is 'normal, or 'condensed, or whatever.
    (if (and height (numberp height))
	(cons (/ height 20) (/ height 10))
      (cons 6 12))))

(defcustom cogre-dot-node-position-scale
  (cogre-calculate-node-position-scale)
  "The scale to use when converting between COGRE and DOT position values.
This is of the format ( XSCALE . YSCALE ).
DOT uses points, where as COGRE uses characters."
  :group 'cogre
  :type 'cons)

(defmethod cogre-export-dot-pos ((node cogre-node))
  "Return a DOT compatible position."
  (let* ((pos (oref node position))
	 (scalex (car cogre-dot-node-position-scale))
	 (scaley (cdr cogre-dot-node-position-scale)))
    (format "%d,%d" (* scalex (aref pos 0))
	    ;; Dot does stuff upside-down, so we need to invert Y
	    (* scaley (- cogre-export-max-y (aref pos 1))))))

(defmethod cogre-export-dot-shape ((node cogre-node))
  "Convert NODE into DOT syntax of semantic tags."
  "box")

(defmethod cogre-export-dot-shape ((node cogre-class))
  "Convert NODE into DOT syntax of semantic tags."
  "record")

(defmethod cogre-export-dot-shape ((node cogre-package))
  "Convert NODE into DOT syntax of semantic tags."
  "tab")

(defmethod cogre-export-dot-shape ((node cogre-note))
  "Convert NODE into DOT syntax of semantic tags."
  "note")

(defmethod cogre-export-dot-label ((node cogre-node))
  "Convert NODE into DOT syntax of semantic tags."
  (mapconcat 'identity (cogre-node-title node) "\\n"))

(defmethod cogre-export-dot-label ((node cogre-scoped-node))
  "Convert NODE into DOT syntax of semantic tags."
  (let ((name (oref node :object-name))
	(pack (oref node :package-name)))
    (if (<= (length pack) 0)
	name
      (setq pack (concat "\\<\\<" pack "\\>\\>"))
      (concat pack "\\n" name))))

(defmethod cogre-export-dot-label ((node cogre-class))
  "Convert NODE into DOT syntax of semantic tags."
  (concat "{" (call-next-method) "|"
	  (cogre-export-dot-fieldslist node) "|"
	  (cogre-export-dot-methodlist node) "}"))

(defmethod cogre-export-dot-methodlist ((node cogre-class))
  "Get a list of methods on NODE.  Return as \n separated list."
  (mapconcat (lambda (s) (cogre-uml-stoken->uml node s)) (oref node methods) "\\l"))

(defmethod cogre-export-dot-fieldslist ((node cogre-class))
  "Get a list of fields on NODE.  Return as \n separated list."
  (mapconcat (lambda (s) (cogre-uml-stoken->uml node s)) (oref node attributes) "\\l"))

(defmethod cogre-export-dot-label ((node cogre-instance))
  "Convert NODE into DOT syntax of semantic tags."
  (let ((title (call-next-method)))
    (if (string-match "\\\\n" title)
	(replace-match "n:" t t title)
      (concat ":" title))))

;;; LINKS		  
(defmethod cogre-export-dot-method ((link cogre-link))
  "Convert LINK into DOT syntax of semantic tags."
  (let ((start (oref link start))
	(end (oref link end)))
    (semantic-tag (oref end :object-name)
		  'link
		  :to (oref start :object-name)
		  :attributes
		  ( list
		    (semantic-tag "arrowhead" 'attribute :value "none")
		    (semantic-tag "arrowtail" 'attribute :value "none")
		    )
		  )))
		
(defmethod cogre-export-dot-method ((link cogre-inherit))
  "Convert LINK into DOT syntax of semantic tags."
  (let ((tag (call-next-method))
	(end (oref link end)))
    (cogre-tag-put-dot-attribute tag "arrowtail" "empty")
    (cogre-tag-put-dot-attribute tag "arrowsize" "2")
    ;(cogre-tag-put-dot-attribute tag :sametail (oref end :object-name))
    tag))
		
(defmethod cogre-export-dot-method ((link cogre-aggregate))
  "Convert LINK into DOT syntax of semantic tags."
  (let ((tag (call-next-method)))
    (cogre-tag-put-dot-attribute tag "arrowhead" "diamond")
    tag))
		
(defmethod cogre-export-dot-method ((link cogre-arrow))
  "Convert LINK into DOT syntax of semantic tags."
  (let ((tag (call-next-method)))
    (cogre-tag-put-dot-attribute tag "arrowhead" "open")
    tag))

(provide 'cogre/convert)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "cogre/convert"
;; End:

;;; cogre/convert.el ends here
