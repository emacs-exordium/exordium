;;; cogre/mode.el --- Graph editing mode

;;; Copyright (C) 2001, 2002, 2003, 2007, 2009, 2010, 2012 Eric M. Ludlam

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; COGRE mode is based on a generic editor that can render arbitrary
;; graphs as specified by the COGRE core classes.
;; This depends on EIEIO for graph management.  COGRE mode depends on
;; `picture-mode' for drawing.
;;
;; Because COGRE graphs are meant to be edited in some fashion, COGRE
;; graphs depend on the custom widget library to provide text
;; controls, or toggle buttons for editing state in a graph.

(require 'cogre/picture-hack)
(require 'eieio)
(require 'eieio-opt)
(require 'eieio-base)
(require 'eieio-custom)
(require 'cogre)
(require 'cogre/convert)
(require 'cogre/semantic)
(require 'semantic/senator)

(eval-when-compile
  (require 'cl))

;;; Code:
(defface cogre-box-face  '((((class color) (background dark))
			    (:background "gray30" :foreground "white"))
			   (((class color) (background light))
			    (:background "gray" :foreground "black")))
  "Face used for rectangles of boxes displaying data."
  :group 'cogre)

(defface cogre-box-first-face  '((((class color) (background dark))
				  (:background "gray30" :foreground "white" :overline "white"))
				 (((class color) (background light))
				  (:background "gray" :foreground "black" :overline "black")))
  "Face used for the first data item in rectangles of boxes displaying data.
This has the `overline' property set to display borders between sections
within a box."
  :group 'cogre)

(defface cogre-box-last-face  '((((class color) (background dark))
				  (:background "gray30" :foreground "white" :underline "white"))
				 (((class color) (background light))
				  (:background "gray" :foreground "black" :underline "black")))
  "Face used for the first data item in rectangles of boxes displaying data.
This has the `overline' property set to display borders between sections
within a box."
  :group 'cogre)

(defun cogre-substitute (km oldfun newfun)
  "Substitue in KM, a key binding in ghe `cogre-mode-map'.
Argument OLDFUN is removed NEWFUN is substituted in."
  (substitute-key-definition oldfun newfun km global-map))

(defvar cogre-mode-map
  (let ((km (make-keymap)))
    (suppress-keymap km)
    ;; Structure Information
    (define-key km "\C-m" 'cogre-activate-element)
    ;; Structure changes
    (define-key km "R" 'cogre-refresh)
    (define-key km "N" 'cogre-new-node)
    (define-key km "L" 'cogre-new-link)
    (define-key km "D" 'cogre-delete)
    (define-key km "U" 'cogre-update-node-from-source)
    (define-key km "J" 'cogre-jump-from-node-to-peer)
    ;; Changing and Setting Defaults
    (define-key km "\C-c\C-n" 'cogre-default-node)
    (define-key km "\C-c\C-l" 'cogre-default-link)
    ;; Kill/Yank operations
    (define-key km "\C-k" 'cogre-kill-element)
    (define-key km "\C-w" 'cogre-kill-element)
    (define-key km "\M-w" 'cogre-copy-element)
    (define-key km "\C-y" 'cogre-yank-element)
    ;; Modifications
    (define-key km "n" 'cogre-set-element-name)
    ;; Move nodes around
    (define-key km [(meta left)] 'cogre-move-node-left)
    (define-key km [(meta right)] 'cogre-move-node-right)
    (define-key km [(meta down)] 'cogre-move-node-down)
    (define-key km [(meta up)] 'cogre-move-node-up)
    (define-key km "\M-b" 'cogre-move-node-left)
    (define-key km "\M-f" 'cogre-move-node-right)
    (define-key km "\M-n" 'cogre-move-node-down)
    (define-key km "\M-p" 'cogre-move-node-up)
    ;; Cursor Movement
    (define-key km "\C-i" 'cogre-next-node)
    (define-key km "\M-\C-i" 'cogre-prev-node)
    (cogre-substitute km 'forward-char  'picture-forward-column)
    (cogre-substitute km 'backward-char 'picture-backward-column)
    (cogre-substitute km 'next-line     'picture-move-down)
    (cogre-substitute km 'previous-line 'picture-move-up)
    ;; Mouse Manipulations
    (define-key km [down-mouse-1] 'cogre-down-mouse-1)
    (define-key km [drag-mouse-1] 'ignore)
    (define-key km [down-mouse-2] 'cogre-down-mouse-2)
    (define-key km [drag-mouse-2] 'ignore)
    (define-key km [down-mouse-3] 'cogre-down-mouse-3)
    (define-key km [drag-mouse-3] 'ignore)
    km)
  "Keymap used for COGRE mode.")

(easy-menu-define
  cogre-mode-menu cogre-mode-map "Connected Graph Menu"
  '("Graph"
    ["Refresh" cogre-refresh t ]
    ["Layout (dot)" cogre-layout t ]
    ("Insert" :filter cogre-insert-forms-menu)
    ("Navigate"
     ["Next Element" cogre-next-node t ]
     ["Prev Element" cogre-prev-node t ]
     ["Move Node Up"    cogre-move-node-up    (cogre-node-child-p (cogre-current-element)) ]
     ["Move Node Down"  cogre-move-node-down  (cogre-node-child-p (cogre-current-element)) ]
     ["Move Node Left"  cogre-move-node-left  (cogre-node-child-p (cogre-current-element)) ]
     ["Move Node right" cogre-move-node-right (cogre-node-child-p (cogre-current-element)) ]
     )
    ("Change" :filter cogre-change-forms-menu)
    "--"
    ( "Edit..."
      [ "Kill Node" cogre-kill-element (cogre-current-element) ]
      [ "Copy Node" cogre-copy-element (cogre-current-element) ]
      [ "Yank Node or Tag" cogre-yank-element (cogre-killring-active) ]
      [ "Delete" cogre-delete (cogre-current-element) ])
    ( "Update..."
      [ "Update Graph from Peers" cogre-update-graph-from-source t ]
      [ "Update Element from Peer" cogre-update-node-from-source (cogre-node-with-peer) ])
    [ "Customize Graph" cogre-customize-graph t]
    [ "PS Print" cogre-export-dot-postscript-print t ]
    ("Export to..."
     [ "ASCII Art" cogre-export-ascii t ]
     [ "Graphviz DOT" cogre-export-dot t ]
     [ ".png Image" cogre-export-dot-png t ]
     )
    ))

(defvar cogre-popup-map (make-sparse-keymap)
  "Map for popup menus.")

(easy-menu-define
  cogre-mode-create-popup-menu cogre-popup-map "Connected Graph Insert Menu"
  '("Insert"
    [ "Node" cogre-new-node t]
    "---"
    [ "Class" cogre-new-node t]
    [ "Package" cogre-new-node t]
    [ "Instance" cogre-new-node t]
    [ "Note" cogre-new-node t]
    "---"
    [ "Yank Node or Tag" cogre-yank-element (not (ring-empty-p senator-tag-ring)) ]
    [ "Customize Graph" cogre-customize-graph t]
    ))

(easy-menu-define
  cogre-mode-new-link-popup-menu cogre-popup-map "New Link Menu"
  '("New Link Type"
    [ "Link" cogre-select-a-link t]
    [ "Update Link from Peer" cogre-update-node-from-source (cogre-node-with-peer) ]
    "---"
    [ "Arrow" cogre-select-a-link t]
    [ "Inherit" cogre-select-a-link t]
    [ "Aggregate" cogre-select-a-link t]
    ))

(defvar cogre-node-base-menu
  '("Update Node"
    [ "Rename" cogre-set-element-name t ]
    [ "View/Edit" cogre-activate-element t ]
    [ "Update Node from Peer" cogre-update-node-from-source (cogre-node-with-peer) ]
    [ "Jump to Peer" cogre-jump-from-node-to-peer (cogre-node-with-peer) ]
    "---"
    [ "Kill Node" cogre-kill-element (cogre-current-element) ]
    [ "Copy Node" cogre-copy-element (cogre-current-element) ]
    [ "Delete" cogre-delete (cogre-current-element) ]
    )
    "List of basic items for a Menu on a Node.")

(easy-menu-define
  cogre-node-update-popup-menu cogre-popup-map "Connected Graph Update Menu"
  cogre-node-base-menu)

(defvar cogre-link-base-menu
  '("Update Link"
    [ "View/Edit" cogre-activate-element t ]
    [ "Update Node from Peer" cogre-update-node-from-source (cogre-node-with-peer) ]
    "---"
    [ "Delete" cogre-delete (cogre-current-element) ]
    )
    "List of basic items for a Menu on a Link.")

(easy-menu-define
  cogre-link-update-popup-menu cogre-popup-map "Connected Graph Update Menu"
  cogre-link-base-menu)

(defvar cogre-tool-bar-map
  (let ((tool-bar-map (make-sparse-keymap)))
    (when (fboundp 'tool-bar-add-item)
      (tool-bar-add-item "cogre-node" 'cogre-new-node 'node)
      (tool-bar-add-item "cogre-class" 'cogre-new-node 'class)
      (tool-bar-add-item "cogre-package" 'cogre-new-node 'package)
      (tool-bar-add-item "cogre-instance" 'cogre-new-node 'instance)
      (tool-bar-add-item "cogre-note" 'cogre-new-node 'note)
      (tool-bar-add-item "cogre-link" 'cogre-new-link 'link)
      (tool-bar-add-item "cogre-arrow" 'cogre-new-link 'arrow)
      (tool-bar-add-item "cogre-isa" 'cogre-new-link 'inherit)
      (tool-bar-add-item "cogre-hasa" 'cogre-new-link 'aggregate)
      )
    tool-bar-map)
  "The tool-bar used for COGRE mode.")


(defmethod cogre-insert-class-list ((graph cogre-base-graph))
  "Return a list of classes GRAPH will accept."
  (eieio-build-class-alist 'cogre-graph-element))

(defun cogre-insert-forms-menu (menu-def)
  "Create a menu for cogre INSERT item.
Argument MENU-DEF is the easy-menu definition."
  (easy-menu-filter-return
   (easy-menu-create-menu
    "Insert Forms"
    (let ((obj (cogre-current-element))
	  (elements (cogre-insert-class-list cogre-graph))
	  (newmenu nil))
      (while elements
	;; Added (car elements) to the menu.
	(setq newmenu (cons
		       (vector (car (car elements))
			       `(progn
				 (cogre-new-node
				  (point)
				  (intern ,(car (car elements))))
				 (cogre-render-buffer cogre-graph)
				 )
			       t)
		       newmenu))
	(setq elements (cdr elements)))
      (append  (list [ "New Link" cogre-new-link t ]
		     [ "New Node" cogre-new-node t ]
		     )
	       (nreverse newmenu))
      ))))

(defun cogre-change-forms-menu (menu-def)
  "Create a menu for a cogre node item.
Argument MENU-DEF is the easy-menu definition."
  (easy-menu-filter-return
   (easy-menu-create-menu
    "Change Forms"
    (let* ((obj (cogre-current-element))
	   (newmenu (cogre-augment-element-menu obj nil)))
      (append  '( [ "Name" cogre-set-element-name (cogre-current-element) ]
		  [ "View/Edit" cogre-activate-element (cogre-current-element) ]
		  )
	       (nreverse newmenu))
      ))))

;;; Major Mode
;;
;;;###autoload
(defun cogre-mode ()
  "Connected Graph Editor Mode.
\\{cogre-mode-map}"
  (interactive)
  (setq major-mode 'cogre-mode
	mode-name "Cogre")
  (use-local-map cogre-mode-map)
  (when cogre-tool-bar-map
    (set (make-local-variable 'tool-bar-map) cogre-tool-bar-map))
  (setq truncate-lines t)
  (setq indent-tabs-mode nil)
  (buffer-disable-undo)
  (set (make-local-variable 'transient-mark-mode) nil)
  (setq write-contents-functions 'cogre-save-hook)
  ;; Convert contents from save file.
  (cogre-convert-buffer-contents-on-init)
  ;; Tail setup.
  (run-hooks 'cogre-mode-hook)
  ;; Misc issues
  (set (make-local-variable 'font-lock-global-modes) nil)
  (font-lock-mode -1)
  ;; Force the redraw AFTER disabling font lock
  (cogre-render-buffer cogre-graph t)
  ;; If someone changes the major mode, be sure to convert everything
  ;; back into plain-text save file.
  (add-hook 'change-major-mode-hook 'cogre-switch-to-save-text t t)
  )
(put 'cogre-mode 'semantic-match-any-mode t)

;;;###autoload
(add-to-list 'auto-mode-alist (cons "\\.cgr\\'" 'cogre-mode))

(defun cogre-convert-buffer-contents-on-init ()
  "Convert the buffer contents into a graph.
If it is already drawing a graph, then don't convert."
  (when (not (eieio-object-p cogre-graph))
    ;; Convert the contents
    (if (and (buffer-file-name) (file-exists-p (buffer-file-name)))
	(let ((cogre-loading-from-file t))
	  ;; Convert this file into a graph.
	  (condition-case nil
	      (setq cogre-graph (eieio-persistent-read (buffer-file-name)
						       cogre-base-graph t))
	    (error (fundamental-mode)
		   (error "Not a COGRE graph file")))
	  (oset cogre-graph file (buffer-file-name))
	  (cogre-map-elements 'cogre-element-post-serialize)
	  )
      ;; Else, just initialize into a graph.
      (let ((name (file-name-sans-extension (buffer-file-name))))
	(setq cogre-graph (cogre-base-graph name :name name))
	(oset cogre-graph file (buffer-file-name)))
      )
    (set-buffer-modified-p nil) ))

(defun cogre-save-hook ()
  "Hook called when writing out a cogre buffer to disk."
  (when (and (buffer-file-name (current-buffer))
	     (not (slot-boundp cogre-graph 'file)))
    (oset cogre-graph file (buffer-file-name (current-buffer))))
  (when (not (slot-boundp cogre-graph 'file))
    (error "Cannot save graph.  File not set in graph object.  (programmer error?)"))
  (cogre-save cogre-graph)
  (set-buffer-modified-p nil)
  (clear-visited-file-modtime)
  t)

(defun cogre-switch-to-save-text ()
  "Convert the current graph to the text we save."
  (if (not cogre-graph)
      (message "No graph to conver to text when switching modes")

    ;; Setup the objects to have a file name.
    (when (and (buffer-file-name (current-buffer))
	       (not (slot-boundp cogre-graph 'file)))
      (oset cogre-graph file (buffer-file-name (current-buffer))))
    ;; Clear out all the graph text
    (erase-buffer)
    ;; Write the text into this buffer.
    (let ((standard-output (current-buffer)))
      (cogre-write-save-text cogre-graph)
      )
    (goto-char (point-min))
    ))

;;; Customzize the graph
;;
(defun cogre-customize-graph ()
  "Customize the current graph."
  (interactive)
  (let ((b (current-buffer)))
    (require 'eieio-custom)
    (customize-object cogre-graph)
    (setq cogre-custom-originating-graph-buffer b)))

;;; Menu Helper
;;
(defun cogre-killring-active ()
  "Return non-nil if there is a tag in the kill ring."
  (not (ring-empty-p senator-tag-ring)))

(defun cogre-node-with-peer ()
  "Return non-nil if there is a node with a peer under the cursor."
  (let* ((node (cogre-current-element (point))))
    (and node (oref node peer))))

;;; Interactive utility functions
;;
(defun cogre-node-at-point-interactive (&optional pos)
  "Return the node under POS.
Throw an error if there is no node."
  (let ((e (cogre-current-element (or pos (point)))))
    (if (or (not e) (not (obj-of-class-p e cogre-node)))
	(error "No graph node under point")
      e)))

(defun cogre-link-at-point-interactive (&optional pos)
  "Return the node under POS.
Throw an error if there is no node."
  (let ((e (cogre-current-element (or pos (point)))))
    (if (or (not e) (not (obj-of-class-p e cogre-link)))
	(error "No graph node under point")
      e)))

(defun cogre-element-at-point-interactive (&optional pos)
  "Return the node under POS.
Throw an error if there is no node."
  (let ((e (cogre-current-element (or pos (point)))))
    (if (not e)
	(error "No graph node under point")
      e)))

;;; Edit/View elements
;;
(defun cogre-activate-element (element)
  "View/Edit the ELEMENT.
The default ELEMENT is the one found under the cursor."
  (interactive (list (cogre-current-element)))
  (if element
      (cogre-activate element)
    (error "The cursor is not on an object")))

;;; Insert/Delete
;;
(defun cogre-new-node (point nodetype &rest fields)
  "Insert a new node at the current point.
Argument POINT is a position to insert this node to.
NODETYPE is the eieio class name for the node to insert.
Optional FIELDS are additional constructor fields to pass
in to the created node."
  (interactive (list (point) (cogre-default-node nil current-prefix-arg)))
  (save-excursion
    (goto-char point)
    (let* ((x (current-column))
	   (y (cogre-current-line))
	   (n (apply 'make-instance nodetype (oref nodetype name-default)
		     :position (vector x y)
		     fields)))
      (when (called-interactively-p 'any)
	(cogre-render n)
	)
      ;; Return the node.
      n)))

(defun cogre-new-link (mark point &optional linktype)
  "Insert a new link from the node at MARK to POINT of LINKTYPE.
MARK is the node within which the current mark is set.
POINT is the node the cursor is in.
LINKTYPE is the eieio class name for the link to insert."
  (interactive (list (cogre-node-at-point-interactive (mark))
		     (cogre-node-at-point-interactive (point))
		     (cogre-default-link nil current-prefix-arg)))
  (if (not linktype) (setq linktype cogre-link))
  (let ((l (make-instance linktype "Link" :start mark :end point)))

    (when (called-interactively-p 'any)
      (cogre-render l))
    l))

(defvar cogre-delete-dont-ask nil
  "Track if we should ask about deleting an object from the graph.")

(defun cogre-delete (element)
  "Delete the graph ELEMENT under the cursor."
  (interactive (list (cogre-element-at-point-interactive (point))))
  (if (or cogre-delete-dont-ask
	  (y-or-n-p (format "Really delete %s? " (eieio-object-name element))))
      (let ((cogre-delete-dont-ask t))
	(if (obj-of-class-p element cogre-node)
	    (let ((el (oref cogre-graph elements))
		  (test nil))
	      (while el
		(setq test (car el)
		      el (cdr el))
		(if (and (obj-of-class-p test cogre-link)
			 (or (eq element (oref test start))
			     (eq element (oref test end))))
		    (cogre-delete test)))))
	(cogre-erase element)
	(cogre-delete-element cogre-graph element))
    ))

;;; Kill/Yank
;;
(defun cogre-kill-element (element)
  "Kill the ELEMENT under point.
Uses `cogre-copy-element' to push it into the kill ring."
  (interactive (list (cogre-element-at-point-interactive (point))))
  ;; Copy the element first.
  (cogre-copy-element element)
  ;; Now delete the node.
  (cogre-delete element)
  )

(defun cogre-copy-element (element)
  "Copy the ELEMENT under point.
Clones the object in question and places the clone in semantic-tag-format
into `senator-tag-ring'."
  (interactive (list (cogre-element-at-point-interactive (point))))
  (let* ((cogre-export-max-y (count-lines (point-min) (point-max)))
	 (tag (cogre-export-dot-method element))
	 )
    ;; While cloning the old element, disable the graph.
    (semantic--tag-put-property
     tag :cogre (clone element (oref element :object-name)))
      
    (ring-insert senator-tag-ring tag)
    ))

(defun cogre-yank-element ()
  "Yank an element into the current graph.
Uses `senator-tag-ring'.  Will yank semantic tags from code buffers
and translate into COGRE nodes."
  (interactive)
  (or (ring-empty-p senator-tag-ring)
      (let* ((tag (ring-ref senator-tag-ring 0))
	     (elt (semantic--tag-get-property tag :cogre))
	     (x (current-column))
	     (y (cogre-current-line))
	     )
	(if elt
	    ;; If this was previously a cogre node, then clone it
	    ;; and move it and insert.
	    (progn
	      (setq elt (clone elt (oref elt :object-name)
			       :position (vector x y)))
	      ;; We need to make the name unique, and add to the
	      ;; graph, as clone won't call initialize.
	      (let ((n (oref elt object-name)))
		;; make sure our name is unique.
		(oset elt object-name (cogre-unique-name cogre-graph n)))
	      (cogre-add-element cogre-graph elt)
	      )
	  ;; It is some misc Semantic Tag.  Convert into a node.
	  (setq elt (cogre-semantic-tag-to-node tag))
	  (when elt
	    (oset elt :position (vector x y)))
	  )
	(cogre-refresh))))

;;; Navigation
;;
(defun cogre-goto-element (elt)
  "Move the cursor onto the element ELT."
  (if (obj-of-class-p elt cogre-node)
      ;; We have a node
      (let ((p (oref elt position)))
	(picture-goto-coordinate (aref p 0) (aref p 1)))
    ;; Else, we have a link
    (with-slots (stop-position) elt
      (apply 'picture-goto-coordinate stop-position)
      )))

(defun cogre-next-node (&optional arg)
  "Move forward ARG nodes in the hierarchy.
If ARG is unspecified, assume 1."
  (interactive "p")
  (let ((n (cogre-current-element (point)))
	(e (oref cogre-graph elements))
	(next nil))
    (if (not n)
	;; Not on the node?  Tab around.
	(setq next (car e))
      (let* ((l (length e))
	     (i (- l (length (member n e))))
	     (ni (+ i arg)))
	(if (< ni 0) (setq ni (+ l ni))
	  (if (>= ni l) (setq ni (- ni l))))
	(setq next (nth ni e))))
    (cogre-goto-element next)))

(defun cogre-prev-node (&optional arg)
  "Move backward ARG nodes in the hierarchy.
If ARG is unspecified, assume 1."
  (interactive "p")
  (cogre-next-node (- arg)))

;;; Node Modification
;;
(defun cogre-render-node-after-erase (node)
  "Redraw the node NODE after it was erased.
It will redraw the links too."
  (let ((links (cogre-node-links node)))
    (cogre-render node)
    (mapc 'cogre-render links)))

(defun cogre-set-element-name (node name)
  "Set the name of the current NODE to NAME."
  (interactive (let ((e (cogre-node-at-point-interactive)))
		 (let ((name (oref e object-name)))
		 (list e  (read-string "New Name: " name)))))
  (cogre-erase node)
  (oset node object-name (cogre-unique-name cogre-graph name))
  (when (called-interactively-p 'any)
    (cogre-render-node-after-erase node)
    (cogre-goto-element node)
    ;; If the user changes the name, update the peer.
    (let ((peer (oref node peer)))
      (when peer (cogre-peer-update-from-element peer node)))
    )
  )

(defun cogre-set-scoped-node-package (node package)
  "Set the package name of the current NODE to PACKAGE."
  (interactive (let ((e (cogre-node-at-point-interactive)))
		 (let ((name (oref e package-name)))
		 (list e  (read-string "New Package Name: " name)))))
  (cogre-erase node)
  (oset node package-name package)
  (when (called-interactively-p 'any)
    (cogre-render-node-after-erase node)
    (cogre-goto-element node)
    ;; If the user changes the name, update the peer.
    (let ((peer (oref node peer)))
      (when peer (cogre-peer-update-from-element peer node)))
    )
  )

(defun cogre-update-node-from-source (node)
  "Update the current NODE from its source.
The source is defined by the peer belonging to NODE."
  (interactive (list (cogre-node-at-point-interactive)))
  (let ((peer (oref node peer)))
    (if (not peer)
	(message "No peer to update from.")
      (cogre-erase node)
      (cogre-peer-update-from-source peer node)
      (cogre-render-node-after-erase node)
      (cogre-goto-element node)
      )))

(defun cogre-jump-from-node-to-peer (node)
  "Jump to some representation of the peer from NODE.
The source is defined by the peer belonging to NODE."
  (interactive (list (cogre-node-at-point-interactive)))
  (let ((peer (oref node peer))
	(win (selected-window)))
    (if (not peer)
	(message "No peer to visit.")
      (cogre-peer-jump-to-source peer node)
      )))

(defun cogre-update-graph-from-source ()
  "Update the current graph from its sources."
  (interactive)
  ;; Do the update.
  (cogre-update-graph-from-peers cogre-graph)
  ;; Now refresh the graph.
  (cogre-refresh))

;;; Node Movement
;;
(defun cogre-move-node (x y &optional node)
  "Set a node to postion X, Y.
If NODE is not provided, then calculate from current position."
  (interactive "nX: \nnY: ")
  (let ((inhibit-point-motion-hooks t)
	(e (or node (cogre-current-element (point)))))
    (cogre-erase e)
    (if (<= x 0) (setq x 0))
    (if (<= y 0) (setq y 0))
    (cogre-move e x y)
    (let ((pos (oref e position)))
      (picture-goto-coordinate (aref pos 0) (aref pos 1)))
    ;; Do the service of redrawing the modified pieces.
    (cogre-render-node-after-erase e)
    (picture-goto-coordinate x y)))

(defun cogre-node-position (&optional noerror)
  "Get the position of the node at point.
Optional NOERROR means don't throw an error if there was no node."
  (let ((e (cogre-current-element (point)))
	)
    (if e (oref e position)
      (if noerror
	  nil
	(error "No node at point %d" (point))))))

(defun cogre-move-node-left (arg)
  "Move NODE left by ARG columns."
  (interactive "p")
  (let* ((p (cogre-node-position)))
    (cogre-move-node (- (aref p 0) arg) (aref p 1))
    ))

(defun cogre-move-node-right (arg)
  "Move NODE right by ARG columns."
  (interactive "p")
  (let* ((p (cogre-node-position)))
    (cogre-move-node (+ (aref p 0) arg) (aref p 1))
    ))

(defun cogre-move-node-up (arg)
  "Move NODE up by ARG columns."
  (interactive "p")
  (let* ((p (cogre-node-position)))
    (cogre-move-node (aref p 0) (- (aref p 1) arg))
    ))

(defun cogre-move-node-down (arg)
  "Move NODE down by ARG columns."
  (interactive "p")
  (let* ((p (cogre-node-position)))
    (cogre-move-node (aref p 0) (+ (aref p 1) arg))
    ))

;;; Mouse Handlers
;;
;; Cogre is mostly keyboard driven.  The mouse will make dragging
;; existing things around easier.

(defun cogre-down-mouse-1 (event)
  "Handle a mouse-down-1 EVENT in `cogre' mode.
Clicking and dragging on a node will move the node."
  (interactive "@e")
  (let* ((echo-keystrokes 10000)	; don't show pressed keys.
	 (start-pos (posn-col-row (event-end event)))
	 (x1        (car start-pos))
	 (y1        (cdr start-pos))
	 )

    ;; Make sure the text character exists for this point.
    (picture-mouse-set-point event)

    ;; Did we click on a node?
    (let* ((node (cogre-current-element (point)))
	   (auto-hscroll-mode nil))

      (cond
       ((not node)
	;; We didn't click on anything.  Drag the whole graph
	;; around.
	(track-mouse

	  (while (progn
		   (setq event (read-event))
		   (mouse-movement-p event))

	    (let* ((next-pos (posn-col-row (event-end event)))
		   (x2       (car next-pos))
		   (y2       (cdr  next-pos))
		   (dx (- x2 x1))
		   (dy (- y2 y1))
		   )

	      (condition-case nil
		  (scroll-down dy)
		(error nil))
	      (condition-case nil
		  (scroll-right dx)
		(error nil))

	      (setq x1 x2
		    y1 y2
		    start-pos next-pos)
	    )))
	nil)
       ((cogre-node-child-p node)
	;; We have a node.  Drag it.
	(track-mouse

	  (while (progn
		   (setq event (read-event))
		   (mouse-movement-p event))

	    (let* ((next-pos (posn-col-row (event-end event)))
		   (x2       (car next-pos))
		   (y2       (cdr  next-pos))
		   (dx (- x2 x1))
		   (dy (- y2 y1))
		   (p (oref node position))
		   )

	      ;; We have a node.  Start dragging.
	      (cogre-move-node (+ (aref p 0) dx) (+ (aref p 1) dy) node)

	      (setq x1 x2
		    y1 y2
		    start-pos next-pos)
	    )))
	;; Always redraw when we are done.
	(cogre-render-buffer cogre-graph t)
	)
       ((cogre-link-child-p node)
	;; Implement something good here someday.
	nil)
       (t
	nil)

       ))))

(defvar cogre-down-mouse-2-link-selector nil
  "The link type to use when using mouse 2.
Set by menu operations.")

(defun cogre-select-a-link ()
  "Select a link type from a popup menu."
  (interactive)
  (setq cogre-down-mouse-2-link-selector
	(cogre-default-link)))

(defun cogre-down-mouse-2 (event)
  "Handle a mouse-down-2 EVENT in `cogre' mode.
Clicking and dragging on a node will move the node."
  (interactive "@e")
  (let* ((echo-keystrokes 10000)	; don't show pressed keys.
	 (start-pos (posn-col-row (event-end event)))
	 (x1        (car start-pos))
	 (y1        (cdr start-pos))
	 )

    ;; Make sure the text character exists for this point.
    (picture-mouse-set-point event)

    ;; Did we click on a node?
    (let* ((node (cogre-current-element (point))))

      (if (and node (cogre-node-child-p node))
	  (progn
	    (message "Drag POINT to node to create a link.")
	    ;; Create a link by dragging from this node to another.
	    (track-mouse

	      (while (progn
		       (setq event (read-event))
		       (mouse-movement-p event))
		
		;; We need some way to indicate the drag.
		(picture-mouse-set-point event)

		(message "Drag POINT to node to create a link.")
		))
	    (let ((endnode (cogre-current-element (point)))
		  (cogre-down-mouse-2-link-selector nil))
	      (if endnode
		  (progn
		    (popup-menu cogre-mode-new-link-popup-menu)
		    (make-instance (or cogre-down-mouse-2-link-selector
				       'cogre-link)
				   :start node :end endnode)
		    (cogre-render-buffer cogre-graph)
		    )
		;; else, a bug
		(message "You must drop the link onto another node."))
	      ))
	(message "Click on a node and drag to create a link.")
	;; Eat the next event
	(read-event)
	;; Repeat the message
	(message "Click on a node and drag to create a link.")
	))))

(defun cogre-down-mouse-3 (event)
  "Handle a popup menu EVENT in `cogre' mode.
Pops up a context menu of various activities to perform."
  (interactive "@e")
  (let* ((startwin (selected-window))
	 ;; This line has an issue in XEmacs.
	 (win (semantic-event-window event))
	 )
    (select-window win t)
    (save-excursion
      (picture-mouse-set-point event)
      (sit-for 0)
      (let ((node (cogre-current-element (point))))
	(cond
	 ((cogre-node-child-p node)
	  (let ((extramenu (cogre-augment-element-menu node nil)))
	    (if (not extramenu)
		(popup-menu cogre-node-update-popup-menu)
	      ;; Merge menus, and pop up the new one.
	      (let ((newmenu nil))
		(dolist (M (cdr cogre-node-base-menu))
		  (push M newmenu)
		  (when (and (stringp M) (string= M "---"))
		    ;; Add new entries here.
		    (dolist (NM extramenu)
		      (push NM newmenu))
		    (push "---" newmenu)
		    ))
		;; No convert the fabricated menu into a temporary popup.
		(popup-menu
		 (easy-menu-create-menu (car cogre-node-base-menu) (nreverse newmenu)))
		))))
	 ((cogre-link-child-p node)
	  (popup-menu cogre-link-update-popup-menu))
	 (t
	  (popup-menu cogre-mode-create-popup-menu)))
	))
    ;; If the selected window didn't change, then go back to the old window.
    ;; If it did change, leave this state alone.
    (when (eq win (selected-window))
      (select-window startwin)) ))

(provide 'cogre/mode)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "cogre/mode"
;; End:

;;; cogre/mode.el ends here
