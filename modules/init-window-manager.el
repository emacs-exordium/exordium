;;;; A simple window manager.
;;;
;;; ----------------- ---------------------------------------------------------
;;; Key               Definition
;;; ----------------- ---------------------------------------------------------
;;; C-c <arrow>       Move cursor between windows (similar to C-x o).
;;; C-c shift-<arrow> Move the windows themselves, e.g. swap them.
;;; M-p <number>      Ace-window: M-p displays a number in each window, and you
;;;                   just have to type the number you want. If there are only 2
;;;                   windows, it cycles between them.
;;;
;;; Simple test:
;;; - C-x 2           split the screen between 2 windows one on top of the other
;;; - M-C-l           open a different buffer in the current window
;;; - C-c up/down     jump between windows
;;; - C-c S-up/down   swap windows
;;;
;;; Functions:
;;;
;;; `toggle-window-dedicated' makes a window dedicated or not (it prevents
;;; Emacs from reusing the window to display another buffer).


;;; C-c arrow = move the focus between visible buffers

(require 'windmove)
(exordium-define-key global-map 'windmove-left)
(exordium-define-key global-map 'windmove-right)
(exordium-define-key global-map 'windmove-up)
(exordium-define-key global-map 'windmove-down)


;;; C-c shift-arrow = move buffers themselves (e.g. swap windows)

(defun move-buffer-up ()
  "Move the current window up, if possible."
  (interactive)
  (let ((other-win (windmove-find-other-window 'up))
        (buffer    (window-buffer (selected-window))))
    (cond (other-win
           (set-window-buffer (selected-window) (window-buffer other-win))
           (set-window-buffer other-win buffer)
           (select-window other-win))
          (t
           (message "No window up from selected window")))))

(defun move-buffer-down ()
  "Move the current window down, if possible."
  (interactive)
  (let ((other-win (windmove-find-other-window 'down))
        (buffer    (window-buffer (selected-window))))
    (cond ((and other-win
                (not (window-minibuffer-p other-win)))
           (set-window-buffer (selected-window) (window-buffer other-win))
           (set-window-buffer other-win buffer)
           (select-window other-win))
          (t
           (message "No window down from selected window")))))

(defun move-buffer-left ()
  "Move the current window to the left, if possible."
  (interactive)
  (let ((other-win (windmove-find-other-window 'left))
        (buffer    (window-buffer (selected-window))))
    (cond (other-win
           (set-window-buffer (selected-window) (window-buffer other-win))
           (set-window-buffer other-win buffer)
           (select-window other-win))
          (t
           (message "No window left from selected window")))))

(defun move-buffer-right ()
  "Move the current window to the right, if possible."
  (interactive)
  (let ((other-win (windmove-find-other-window 'right))
        (buffer    (window-buffer (selected-window))))
    (cond (other-win
           (set-window-buffer (selected-window) (window-buffer other-win))
           (set-window-buffer other-win buffer)
           (select-window other-win))
          (t
           (message "No window right from selected window")))))

(exordium-define-key global-map 'move-buffer-up)
(exordium-define-key global-map 'move-buffer-down)
(exordium-define-key global-map 'move-buffer-left)
(exordium-define-key global-map 'move-buffer-right)



(defun toggle-window-dedicated ()
  "Toggles whether the current active window is dedicated or not"
  (interactive)
  (let ((window (get-buffer-window (current-buffer))))
    (message (if (set-window-dedicated-p window
                                         (not (window-dedicated-p window)))
                 "Window '%s' is dedicated"
               "Window '%s' is normal")
             (current-buffer))))

;;; Note: apparently there is no Pause key on an Apple keyboard...
(exordium-define-key global-map 'toggle-window-dedicated)


;;; Ace-window

(require 'ace-window)
(exordium-define-key global-map 'ace-window)

(provide 'init-window-manager)
