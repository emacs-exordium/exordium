;;; cedet-m3.el --- A CEDET mode for binding mouse-3 convenience menu.
;;
;; Copyright (C) 2010, 2011 Eric M. Ludlam
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
;; CEDET M3 is a generic minor mode for CEDET, the collection that
;; puts useful information into a context menu.  This context menu
;; is designed to be bound to mouse-3, and maximize a user's efficiency
;; by figuring out what the "best thing to do" might be, and collecting
;; those concepts together in the menu.

(require 'semantic/util-modes)
(require 'semantic/senator)

;;; Code:
(eval-and-compile
  (if (featurep 'xemacs)
      ;; XEmacs support
      (defalias 'cedet-event-window 'event-window)
    ;; Emacs
    (defun cedet-event-window (event)
      "Extract the window from EVENT."
      (car (car (cdr event))))))

(defcustom global-cedet-m3-minor-mode nil
  "Non-nil in buffers with CEDET-Mouse3 menu enabled keybindings."
  :group 'cedet-m3
  :type 'boolean
  :require 'cedet-m3
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (global-cedet-m3-minor-mode (if val 1 -1))))

(defvar cedet-m3-minor-mode nil
  "Non-nil in buffers with CEDET Mouse3 menu keybinding.")
(make-variable-buffer-local 'cedet-m3-minor-mode)

(defcustom cedet-m3-minor-mode-hook nil
  "Hook run at the end of the function `cedet-m3-minor-mode'."
  :group 'cedet-m3
  :type 'hook)

(defvar cedet-m3-prefix-key (if (featurep 'xemacs) [ button3 ] [ mouse-3 ])
  "The common prefix key in cedet-m3 minor mode.")

(defvar cedet-m3-minor-menu nil
  "Menu keymap build from `cedet-m3-menu-bar'.")

(defvar cedet-m3-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km cedet-m3-prefix-key 'cedet-m3-menu)
    ;;(define-key km "\C-x," 'cedet-m3-menu-kbd)
    km)
  "Keymap for cedet-m3 minor mode.")

(defvar cedet-m3-hack-map (make-sparse-keymap)
  "Keymap where we hide our context menu.")

;;;###autoload
(define-minor-mode cedet-m3-minor-mode
  "Toggle cedet-m3 minor mode, a mouse 3 context menu.
With prefix argument ARG, turn on if positive, otherwise off.
Return non-nil if the minor mode is enabled.

\\{cedet-m3-mode-map}"
  :keymap cedet-m3-mode-map
  (when cedet-m3-minor-mode
      (progn
	(when (and (featurep 'semantic) (semantic-active-p))
	  (semantic-m3-install))
	(with-no-warnings
	  (when (and (featurep 'ede) ede-minor-mode)
	    (ede-m3-install)))
	(run-hooks 'cedet-m3-minor-mode-hook))))

(semantic-add-minor-mode 'cedet-m3-minor-mode "m3")

;;;###autoload
(define-minor-mode global-cedet-m3-minor-mode
  "Toggle global use of cedet-m3 minor mode.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle."
  :global t :group 'cedet :group 'cedet-modes
  (semantic-toggle-minor-mode-globally
   'cedet-m3-minor-mode
   (if global-cedet-m3-minor-mode 1 -1)))

;;; KEYBINDING COMMANDS
;;
(defun cedet-m3-menu (event)
  "Popup a menu that can help a user figure out what is under the mouse.
Argument EVENT describes the event that caused this function to be called."
  (interactive "e")
  (let* ((startwin (selected-window))
	 (win (cedet-event-window event))
	 (startloc (point-marker))
	 (winloc (window-start startwin))
	 (menuloc nil)
	 )
    (select-window win t)
    (mouse-set-point event)
    (setq menuloc (point-marker))
    (cedet-m3-create-menu)
    (sit-for 0)
    (semantic-popup-menu cedet-m3-minor-menu)
    ;; Once this is done, decide if the mouse should go back to where
    ;; it came from.
    (when (and (eq (point) menuloc)
	       (eq (window-start) winloc))
      ;; Go back
      (goto-char startloc)
      (select-window startwin))
    ))

(defun cedet-m3-menu-kbd ()
  "Popup a menu at the cursor to help a user figure out what is at that point."
  (interactive)
  (cedet-m3-create-menu)
  (sit-for 0)
  (semantic-popup-menu cedet-m3-minor-menu (senator-completion-menu-point-as-event))
  )

;;; UTILITIES
;;
;;; Menu Item compatibility
;;
(defvar cedet-m3-menu-query-hooks nil
  "List of hook functions that return menu items for M3 query actions.
Each function should return a list of items created by
`cedet-m3-menu-item'.")

(defvar cedet-m3-menu-visit-hooks nil
  "List of hook functions that return menu items for M3 that vist the context.
Each function should return a list of items created by
`cedet-m3-menu-item'.")

(defvar cedet-m3-menu-completions-hooks nil
  "List of hook functions that return menu items for M3 completion lists.
Each function should return a list of items created by
`cedet-m3-menu-item'.")

(defvar cedet-m3-menu-do-hooks nil
  "List of hook functions that return menu items for M3 that do things.
Each function should return a list of items created by
`cedet-m3-menu-item'.")

(defun cedet-m3-menu-item (itemname function &rest attributes)
  "Build an easymenu compatible menu item.
Provides a menu item compatible with Emacs or XEmacs.
XEmacs is different in that :help is removed.
The name is ITEMNAME.  It will call FUNCTION.
ATTRIBUTES are easymenu compatible attributes."
  (when (featurep 'xemacs)
    (attributes (plist-remprop attr :help)))
  (apply #'vector itemname function attributes))

(defun cedet-m3-create-menu ()
  "Create a menu custom to this location."
  (let ((menu nil)
	(easy nil)
	(hvars '(cedet-m3-menu-query-hooks
		 cedet-m3-menu-visit-hooks
		 cedet-m3-menu-completions-hooks
		 cedet-m3-menu-do-hooks))
	)
    (dolist (HV hvars)
      (let ((syms (symbol-value HV)))
	(while syms
	  (cond ((eq (car syms) t) ;; hook is buffer local, pull default value too.
		 (setq syms (default-value HV)))
		((fboundp (car syms))
		 (setq menu (append menu (funcall (car syms))))
		 (setq syms (cdr syms)))
		(t (error "Unknown M3 hook value: %S" (car syms))))
	  )))

    (setq easy (cons "CEDET" menu))

    (easy-menu-define cedet-m3-minor-menu
      cedet-m3-hack-map
      "Cedet-M3 Minor Mode Menu"
      easy)
    ))

(provide 'cedet-m3)

;;; cedet-m3.el ends here
