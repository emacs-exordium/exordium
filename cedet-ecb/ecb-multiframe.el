;;; ecb-multiframe.el --- 

;; $Id$

;; Copyright (C) 2000-2003 Free Software Foundation, Inc.
;; Copyright (C) 2000-2003 Kevin A. Burton (burton@openprivacy.org)

;; Author: Kevin A. Burton (burton@openprivacy.org)
;; Maintainer: Kevin A. Burton (burton@openprivacy.org)
;; Location: http://relativity.yi.org
;; Keywords: 
;; Version: 1.0.0

;; This file is [not yet] part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2 of the License, or any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program; if not, write to the Free Software Foundation, Inc., 59 Temple
;; Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:

;; This is am implementation of multiple frame support for the ECB.  The
;; original design was fairly easy to implement and since it will be a long time
;; until ECB 2.0 it seems obvious that this should be done as a temporary
;; solution.
;;
;; In order to use this just create a new frame and run 'ecb-activate.  You can
;; create as many frames and run ECB in any of them.
;;

;;; Notes:
;;
;; Because ECB is now global it is never really deactivated.  You can deactivate
;; ECB in a frame if you want but the advice will still be around.
;;
;; You have a separate ECB methods, directory, and source buffer for each ECB
;; frame.
;;

;;; Install:
;;
;; Place a (require 'ecb-multiframe) at the end of your normal ECB
;; initialization

;;; TODO:
;;
;; - Should we have a separeate speedbar buffer?  What about eshell?
;;
;; - I should allocate my own ecb-compile-window for each frame.
;;
;; - ECB deactivation isn't currently supported.
;;
;; - Make sure I don't have any hooks that might run on deleted buffers.
;;
;; - Make sure we clean up when a frame is deleted.
;;
;; - Is it possible to migrate some of this code into default-frame-alist
;; instead of using a hook?

;; NOTE: If you enjoy this software, please consider a donation to the EFF
;; (http://www.eff.org)

;;; Code:

;;make certain variables frame local

(defvar ecb-multiframe-variables (list 'ecb-last-edit-window-with-point
                                       'ecb-edit-window
                                       'ecb-compile-window
                                       'ecb-frame
                                       'ecb-windows-hidden
                                       'ecb-toggle-layout-state
                                       'ecb-minor-mode
                                       'ecb-activated-window-configuration)
  "List of ecb variables that are required to be nil in new frames and frame local.")

(defun ecb-multiframe-make-frame-hook(frame)
  "Create a hook so that we can enable the default variables within new frames."
  (interactive
   (list
    (selected-frame)))

  ;;make variables frame local in this frame.

  ;;reset everything to the default value?
  
  (dolist(variable ecb-multiframe-variables)
    (set-frame-parameter nil frame (list (cons variable nil)))
    (modify-frame-parameters frame (list (cons variable nil))))

  ;;ecb-eshell-buffer-name ?
  ;;ecb-speedbar-buffer-name ?

  ;;set ECB special buffer names

  (ecb-multiframe-setup-buffer-name 'ecb-methods-buffer-name " *ECB Methods <%s>*")
  (ecb-multiframe-setup-buffer-name 'ecb-history-buffer-name " *ECB History <%s>*")
  (ecb-multiframe-setup-buffer-name 'ecb-sources-buffer-name " *ECB Sources <%s>*")
  (ecb-multiframe-setup-buffer-name 'ecb-directories-buffer-name " *ECB Directories <%s>*")

  ;;eshell support
  (when (and (featurep 'eshell)
             (featurep 'ecb-eshell))
    
    (ecb-multiframe-setup-buffer-name 'ecb-eshell-buffer-name " *eshell <%s>*")
    (ecb-multiframe-setup-buffer-name 'eshell-buffer-name " *eshell <%s>*"))

  ;;speedbar support
  (when (and (featurep 'speedbar)
             (featurep 'ecb-speedbar))
    
    ;;fix speedbar by binding the given speedbar frame value with the current frame
      
    (mapc (lambda(sframe)
	    (when (boundp sframe)
                (set-frame-parameter nil frame (list (cons sframe frame))))
                (modify-frame-parameters frame (list (cons sframe frame))))
            '(speedbar-frame speedbar-attached-frame dframe-attached-frame))
      
    ;;setup speedbar with a new buffer

    (let((new-ecb-speedbar-buffer-name nil))
    
      (setq new-ecb-speedbar-buffer-name (ecb-multiframe-setup-buffer-name 'ecb-speedbar-buffer-name " SPEEDBAR <%s>"))

      (set-frame-parameter nil frame (list (cons 'speedbar-buffer
						 (get-buffer-create new-ecb-speedbar-buffer-name))))
      (modify-frame-parameters frame (list (cons 'speedbar-buffer
                                                 (get-buffer-create new-ecb-speedbar-buffer-name)))))))

(defun ecb-multiframe-setup-buffer-name(variable buffer-format-name)
  "Given a variable name such as 'ecb-methods-buffer-name and a format such as
'*ECB Methods <%s>*' we will register a new buffer mapping with the current
frame.  When complete return the new buffer name."

  (let((new-buffer-name (format buffer-format-name
                                (format-time-string "%s"))))
    (with-no-warnings
      (set-frame-parameter nil frame (list (cons variable new-buffer-name))))
  
    (with-no-warnings
      (modify-frame-parameters frame (list (cons variable new-buffer-name))))

    new-buffer-name))

(with-no-warnings
  (defun ecb-deactivate-internal ()
    "Deactivates the ECB and kills all ECB buffers and windows."
    (unless (not ecb-minor-mode)
      
      (setq ecb-minor-mode nil))
    (message "The ECB is now deactivated.")
    ecb-minor-mode))

(defun ecb-multiframe-activate-hook()
  "Hook to run to initialize multiframe support"

  ;;disable ECB frame management for this frame
  (ad-deactivate 'delete-frame)

  ;;now make sure that the buffer being displayed in the edit window isn't a
  ;;compilation buffer.  (NOTE: I actually think this should be a standard part
  ;;of the ECB)
  (ecb-multiframe-edit-window-non-compilation-buffer))

(defun ecb-multiframe-edit-window-non-compilation-buffer()
  "Go through the buffer list making the edit window a non compilation buffer."
  (interactive)
  
  (let((buffer-list (buffer-list))
       (index 0))

    (while (and (or (ecb-compilation-buffer-p (window-buffer ecb-edit-window))
                    (null (buffer-file-name (window-buffer ecb-edit-window))))
                (< index (length buffer-list)))

      (set-window-buffer ecb-edit-window (nth index buffer-list))
      
      (setq index (1+ index)))))

;;this needs to happen last and it should be the last hook
(add-hook 'ecb-activate-hook 'ecb-multiframe-activate-hook t)

;;we need to modify frame parameters for new frames
(add-hook 'after-make-frame-functions 'ecb-multiframe-make-frame-hook)

(provide 'ecb-multiframe)

;;; ecb-multiframe.el ends here
