;;; window.el --- 

;; Copyright (C) 2001 by Free Software Foundation, Inc.

;; Author:  <Sune Mangs@MAYHEM>
;; Keywords: 

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; 

;;; Code:

(require 'eieio)

;;====================================================
;; 
;;====================================================

(defclass jn-window ()
  ((parent :type (or null jn-window)
	   :initform nil)
   )
  )

(defmethod jn-set-parent ((window jn-window) (parent jn-window-container))
  (jn-set-window parent window))
    

;;====================================================
;; 
;;====================================================

(defclass jn-emacs-window (jn-window)
  ((emacs-window :initarg :emacs-window
		 :initform nil)
   )
  )

(defmethod jn-set-size ((window jn-emacs-window) size)
  (save-selected-window
    (select-window (oref window emacs-window))
    (when (car size)
      (enlarge-window (- (car size) (window-width)) t))
    (when (cdr size)
      (enlarge-window (- (cdr size) (window-height))))))

(defmethod jn-get-size ((window jn-emacs-window))
  (let ((w (oref window emacs-window)))
    (cons (window-width w) (window-height w))))

(defmethod jn-get-emacs-window ((window jn-emacs-window))
  (oref window emacs-window))


;;====================================================
;; 
;;====================================================

(defclass jn-view-window (jn-emacs-window)
  ((view :type (or null jn-view)
	 :initarg :view
	 :initform nil)
   )
  )

(defmethod jn-set-view ((window jn-view-window) (view jn-view))
  (oset window view view)
  (oset view window window))


;;====================================================
;; 
;;====================================================

(defclass jn-window-container ()
  ((window :type (or null jn-window)
	   :initform nil)
   )
  )

(defmethod jn-set-size ((window-container jn-window-container) size)
  (jn-set-size (oref window-container window) size))

(defmethod jn-get-size ((window-container jn-window-container))
  (jn-get-size (oref window-container window)))

(defmethod jn-set-window ((window-container jn-window-container) (window jn-window))
  (let ((old-window (oref window-container window)))
    (unless (eq old-window window)
      (when old-window
	(oset old-window parent nil))
      (oset window-container window window)
      (when window
	(oset window parent window-container)))))


;;====================================================
;; 
;;====================================================

(defclass jn-frame ()
  ((window-container :type (or null jn-window-container)
		     :initform nil)
   )
  )

(defmethod jn-set-window ((frame jn-frame) (window jn-window))
  (jn-set-window (oref frame window-container) window))


;;====================================================
;; 
;;====================================================

(defclass jn-split-window (jn-window)
  ((orientation :type symbol
		:initarg orientation
		:initform vertical)
   (amount :type (or integer float)
	   :initarg amount
	   :initform 0.50)
   (top-left-container :type (or null jn-window-container)
		       :initform nil)
   (bottom-right-container :type (or null jn-window-container)
			   :initform nil)
   )
  )

(defmethod jn-set-top-left-window ((window jn-split-window) (window window))
  (jn-set-window (oref window top-left-container) window))

(defmethod jn-set-bottom-right-window ((window jn-split-window) (window window))
  (jn-set-window (oref window bottom-right-container) window))

(defmethod jn-set-amount ((window jn-split-window) amount)
  (oset window amount amount)
  (let ((abs-amount (if (floatp amount)
			(floor (* amount
				  (if (eq (oref window orientation) 'vertical)
				      (car (jn-get-size window))
				    (cdr (jn-get-size window)))))
		      amount)))
    (jn-set-size (oref window top-left-container)
		 (if (eq (oref window orientation) 'vertical)
		     (cons nil abs-amount)
		 (cons abs-amount nil)))))


;;====================================================
;; 
;;====================================================

(defclass jn-view ()
  ((window :type (or null jn-view-window)
	   :initarg :window
	   :initform nil)
   (emacs-buffer :initform nil)
   )
  )

(defmethod jn-init-1 ((view jn-view) buffer)
  (set-buffer buffer)
  (make-local-variable 'jn-view)
  (setq jn-view view)
  (oset view emacs-buffer buffer))

(defmethod jn-get-emacs-buffer ((view jn-view))
  (oref view emacs-buffer))

(defmethod jn-get-window ((view jn-view))
  (oref view window))

(defmethod jn-set-window ((view jn-view) (window jn-view-window))
  (oset view window window)
  (set-window-buffer (jn-get-emacs-window window) (jn-get-emacs-buffer view)))

(defvar jn-general-view-key-map nil)


;;====================================================
;; 
;;====================================================

(defclass jn-general-view (jn-view)
  ()
  )

(defmethod jn-init-1 ((view jn-general-view) name)
  (let* ((buffer (get-buffer-create name))
	 (nop (function (lambda () (interactive)))))
    (save-current-buffer
      (call-next-method view buffer)
      (set-buffer buffer)
      (erase-buffer)
      
      (make-local-variable 'jn-general-view-key-map)
      (setq jn-general-view-key-map (make-sparse-keymap))

      ;; Undefine all unwanted mouse events
      (dolist (key (list
		    [drag-mouse-1] [mouse-1] [double-mouse-1] [triple-mouse-1]
		    [mouse-2] [double-mouse-2] [triple-mouse-2]
		    [mouse-3] [double-mouse-3] [triple-mouse-3]))
	(define-key jn-general-view-key-map key nop))
      
      (define-key jn-general-view-key-map
	[down-mouse-1]
	(function (lambda (e)
		    (interactive "e")
		    (mouse-set-point e)
		    (jn-selected jn-view))))

      (use-local-map jn-general-view-key-map)
      )
    view))

(provide 'jn-window)

;;; window.el ends here