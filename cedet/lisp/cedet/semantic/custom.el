;;; semantic/custom.el --- custom support for semantic tags.
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
;; Provide CUSTOM support for Semantic tags.
;;
;; Defines new custom widget types of:
;;
;;  'tag-edit  - A semantic-tag edited in-place
;;  'tag       - A semantic-tag that is cloned before editing
;;
;; Use of the tag widget currently assumes the user is familiar with
;; the internal structure of a tag.  This program could use some help.

(require 'widget)
(require 'wid-edit)
(require 'semantic/tag)

;;; Code:

;;; TAG-EDIT
;;
;; The TAG-EDIT widget type will edit a single tag.  Since TAGS can
;; have a lot of arbitrary entries, helping the user through adding
;; such entries can be a challenge.  Provide some conveniences that
;; get flushed if left nil.

;;;###autoload
(define-widget 'tag-edit 'group
  "Abstractly modify a Semantic Tag."
  :tag "Tag"
  :format "%v"
  :convert-widget 'widget-types-convert-widget
  :value-create 'semantic-tag-widget-value-create
  :value-get 'semantic-tag-widget-value-get
  :value-delete 'widget-children-value-delete
  :validate 'widget-children-validate
  :match 'semantic-tag-widget-match
  :clone-object-children nil
  )

;;;###autoload
(defun semantic-tag-widget-match (widget value)
  "Match infor for WIDGET against VALUE."
  ;; Write me
  t)

;;;###autoload
(defun semantic-tag-widget-value-create (widget)
  "Create the value of WIDGET."
  (let* ((tag (widget-get widget :value))
	 (name (semantic-tag-name tag))
	 (class (semantic-tag-class tag))
	 (attr (semantic-tag-attributes tag))
	 (chil nil)
	 )

    (widget-insert "Summary:\n   ")
    (widget-insert (semantic-format-tag-prototype tag nil t))
    (widget-insert "\n\n")

    (push (widget-create-child-and-convert
	   widget 'string
	   :tag "Name"
	   :sample-face 'custom-face-tag
	   :value name)
	  chil)

    (push (widget-create-child-and-convert
	   widget '(choice
		    :button-face font-lock-variable-name-face
		    :sample-face 'custom-face-tag
		    (const function)
		    (const variable)
		    (const type)
		    (const include)
		    (const provide)
		    symbol)
	   :tag "Class"
	   :sample-face 'custom-face-tag
	   :value class)
	  chil)

    (let ((attrs '(:arguments
		   :type
		   :constant-flag
		   :prototype-flag
		   :constructor-flag
		   :destructor-flag
		   :operator-flag
		   :reentrant-flag
		   :methodconst-flag
		   :pure-virtual-flag
		   :typedef
		   :typemodifiers
		   :members
		   :template-specifier)))

      (push (widget-create-child-and-convert
	     widget `(plist :key-type
			    (choice
			     :sample-face 'custom-face-tag
			     :button-face font-lock-variable-name-face
			     ,@(mapcar
				(lambda (a)
				  (list 'const a
					;:sample-face 'font-lock-keyword-face
					))
				attrs)
			     symbol)
			    :value-type sexp
			    :sample-face 'custom-face-tag
			    :tag "Attribute")
	     :tag "Attributes"
	     :value attr)
	    chil))
    
    (widget-put widget :children (nreverse chil))
    ))

;;;###autoload
(defun semantic-tag-widget-value-get (widget)
  "Get the value out of WIDGET."
  (let* ((tag (widget-get widget :value))
	 (chil (widget-get widget :children))
	 (name (widget-apply (nth 0 chil) :value-inline))
	 (class (widget-apply (nth 1 chil) :value-inline))
	 (attr (widget-apply (nth 2 chil) :value-inline))
	 )
    ;; Extract the value from each child, and apply to
    ;; our tag.
    (semantic-tag-set-name tag (car name))

    (setcar (nthcdr 1 tag) (car class))

    (setcar (nthcdr 2 tag) (car attr))

    ;; Leave other bits alone.

    ))

;;; TAG
;;
;; The TAG widget type can take some tag variable from the environment
;; and make it editable.  Inheriting from tag-edit, which presumes that
;; the tag in question is ok.

;;;###autoload
(define-widget 'tag 'tag-edit
  "A Semantic Tag."
  :format "%{%t%}:\n%v"
  :value-to-internal 'semantic-tag-widget-to-internal
  :value-to-external 'semantic-tag-widget-to-external
  :close-object-children t
  )

;;;###autoload
(defun semantic-tag-widget-to-internal (widget value)
  "For WIDGET, convert VALUE to a safe representation."
  (cond ((semantic-tag-p value)
	 (semantic-tag-clone value))
	((null value)
	 value)
	(t nil))
  )

;;;###autoload
(defun semantic-tag-widget-to-external (widget value)
  "For WIDGET, convert VALUE from the abstract value."
  value)

;;; TAG CUSTOMIZE BUFFER
;;
;; A special function for customizing a single tag in a UI.
(defvar semantic-tag-wo nil
  "Buffer local variable in tag customize buffers for the current widget.")
(defvar semantic-tag-co nil
  "Buffer local variable in tag customize buffers for the current tag.")

;;;###autoload
(defun semantic-tag-customize (&optional tag)
  "Customize TAG.
When the user clicks 'ACCEPT', then the location where TAG
is stored is directly modified.
If TAG is not provided, then the tag under point is used."
  (interactive)
  ;; setup
  (when (not tag) (setq tag (semantic-current-tag)))
  ;; Create the buffer for editing.
  (switch-to-buffer (get-buffer-create "*CUSTOMIZE TAG*"))
  (setq buffer-read-only nil)
  (kill-all-local-variables)
  (erase-buffer)
  (let ((all (semantic-overlay-lists)))
    (mapc 'semantic-overlay-delete (car all))
    (mapc 'semantic-overlay-delete (cdr all)))
  ;; Add the apply/reset buttons.
  (semantic-tag-custom-apply-reset tag)
  (widget-insert "\n\n")
  (widget-insert "Edit tag:\n\n")
  ;; Create the widget editing the tag
  (make-local-variable 'semantic-tag-wo)
  (setq semantic-tag-wo (semantic-tag-custom-widget-insert tag))
  ;; Now create more apply reset buttons
  (widget-insert "\n")
  (semantic-tag-custom-apply-reset tag)
  ;; Initialize the buffer
  (use-local-map widget-keymap)
  (widget-setup)
  (goto-char (point-min))
  (widget-forward 3)
  (make-local-variable 'semantic-tag-co)
  (setq semantic-tag-co tag)
  )

(defun semantic-tag-custom-apply-reset (tag)
  "Insert an Apply and Reset button into the tag editor.
Argument TAG os the tag being customized."
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   (widget-apply semantic-tag-wo :value-get)
			   (bury-buffer))
		 :button-face 'custom-button
		 "Accept")
  (widget-insert "   ")
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   ;; I think the act of getting it sets
			   ;; it's value through the get function.
			   (message "Applying Changes...")
			   (widget-apply semantic-tag-wo :value-get)
			   (message "Applying Changes...Done."))
		 :button-face 'custom-button
		 "Apply")
  (widget-insert "   ")
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   (message "Resetting.")
			   (semantic-tag-customize semantic-tag-co))
		 :button-face 'custom-button
		 "Reset")
  (widget-insert "   ")
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   (bury-buffer))
		 :button-face 'custom-button
		 "Cancel"))

(defun semantic-tag-custom-widget-insert (tag &rest flags)
  "Insert the widget used for editing TAG in the current buffer.
Arguments FLAGS are widget compatible flags.
Must return the created widget."
  (apply 'widget-create 'tag-edit :value tag flags))


(provide 'semantic/custom)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "semantic/custom"
;; End:

;;; semantic/custom.el ends here
