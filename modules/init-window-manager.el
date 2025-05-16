;;; init-window-manager.el --- A simple window manager -*- lexical-binding: t -*-

;;; Commentary:
;;
;; ----------------- ---------------------------------------------------------
;; Key               Definition
;; ----------------- ---------------------------------------------------------
;; C-c <arrow>       Move cursor between windows (similar to C-x o).
;; C-c shift-<arrow> Move the windows themselves, e.g. swap them.
;; M-o <letter>      Ace-window: M-o displays a letter in each window, and you
;;                   just have to type the letter you want.  If there are only
;;                   2 windows, it cycles between them.  When called with
;;                   prefix arg, swap windows.
;;
;; Simple test:
;; - C-x 2           split the screen between 2 windows one on top of the other
;; - M-C-l           open a different buffer in the current window
;; - C-c up/down     jump between windows
;; - C-c S-up/down   swap windows
;;
;; Functions:
;;
;; `exordium-toggle-window-dedicated' makes a window dedicated or not (it prevents
;; Emacs from reusing the window to display another buffer).



;;; Code:
(eval-when-compile
  (unless (featurep 'init-require)
    (load (file-name-concat (locate-user-emacs-file "modules") "init-require"))))
(exordium-require 'init-prefs)

(use-package window
  :ensure nil
  :custom
  (split-window-preferred-direction exordium-split-window-preferred-direction)
  :functions (exordium--window-try-vertical-split
              exordium--window-try-horizontal-split
              exordium-split-window-sensibly)
  :init
  (unless (boundp 'split-window-preferred-direction) ;; Until Emacs-31
    (defun exordium--window-try-vertical-split (window)
      "Try split WINDOW vertically."
      (when (window-splittable-p window)
        (with-selected-window window
          (split-window-below))))

    (defun exordium--window-try-horizontal-split (window)
      "Try split WINDOW horizontally."
      (when (window-splittable-p window t)
        (with-selected-window window
          (split-window-right))))

    (defun exordium-split-window-sensibly (&optional window)
      "Split WINDOW in a way suitable for `display-buffer'."
      (let ((window (or window (selected-window))))
        (or (if (or
                 (eq exordium-split-window-preferred-direction 'horizontal)
                 (and (eq exordium-split-window-preferred-direction 'longest)
                      (> (frame-width) (frame-height))))
                (or (exordium--window-try-horizontal-split window)
                    (exordium--window-try-vertical-split window))
              (or (exordium--window-try-vertical-split window)
                  (exordium--window-try-horizontal-split window)))
	        (and
             ;; If WINDOW is the only usable window on its frame (it is
             ;; the only one or, not being the only one, all the other
             ;; ones are dedicated) and is not the minibuffer window, try
             ;; to split it vertically disregarding the value of
             ;; `split-height-threshold'.
             (let ((frame (window-frame window)))
               (or
                (eq window (frame-root-window frame))
                (catch 'done
                  (walk-window-tree (lambda (w)
                                      (unless (or (eq w window)
                                                  (window-dedicated-p w))
                                        (throw 'done nil)))
                                    frame nil 'nomini)
                  t)))
	         (not (window-minibuffer-p window))
             (let ((split-height-threshold 0))
               (exordium--window-try-vertical-split window))))))

    ;; Helm directly calls `split-window-sensibly'
    (advice-add 'split-window-sensibly :override
                #'exordium-split-window-sensibly)))

(use-package windmove
  :ensure nil
  :autoload (windmove-find-other-window)
  :functions (move-buffer-left
              move-buffer-right
              move-buffer-up
              move-buffer-down)

  :init
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

  :bind
  (;; C-c arrow = move the focus between visible buffers
   ("C-c <left>"    . #'windmove-left)
   ("C-c <right>"   . #'windmove-right)
   ("C-c <up>"      . #'windmove-up)
   ("C-c <down>"    . #'windmove-down)
   ;; C-c shift-arrow = move buffers themselves (e.g. swap windows)
   ("C-c S-<left>"  . #'move-buffer-left)
   ("C-c S-<right>" . #'move-buffer-right)
   ("C-c S-<up>"    . #'move-buffer-up)
   ("C-c S-<down>"  . #'move-buffer-down)))



(defun exordium-toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not."
  (interactive)
  (let ((window (get-buffer-window (current-buffer))))
    (message (if (set-window-dedicated-p window
                                         (not (window-dedicated-p window)))
                 "Window '%s' is dedicated"
               "Window '%s' is normal")
             (current-buffer))))

;;; Note: apparently there is no Pause key on an Apple keyboard...
(bind-key "<pause>" #'exordium-toggle-window-dedicated)


;;; Ace-window
(use-package posframe)

(use-package ace-window
  :diminish
  :init
  (use-package diff-mode
    :ensure nil
    :bind
    (:map diff-mode-map
     ("M-o" . nil)))
  :custom
  (aw-scope 'frame)
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-translate-char-function #'(lambda (c)
                                  (if (eq ?\M-o c) ?n c)))
  :bind ("M-o" . #'ace-window)
  :config
  (when (and (require 'posframe nil t)
             (posframe-workable-p))
    (ace-window-posframe-mode)))

(provide 'init-window-manager)

;;; init-window-manager.el ends here
