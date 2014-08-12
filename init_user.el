;;;; User/keyboard preferences
;;;
;;; -------------- -------------------------------------------------------
;;; Key            Definition
;;; -------------- -------------------------------------------------------
;;; Meta-g         Goto line
;;; Ctrl-z         Undo
;;; Meta-backspace Delete word
;;; Ctrl-esc       Delete other windows
;;; Ctrl-`         Kill current buffer (= Ctrl-x k)
;;; Shift-enter    Return + tab
;;; F10            Speedbar
;;; Super-arrow    Move between windows (= Ctrl-x o)
;;; Meta-ctrl-l    Switch to last buffer
;;; Ctrl +/-       Zoom
;;; -------------- -------------------------------------------------------


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keyboard

;; Use ESC as Control-G (default requires ESC ESC ESC)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;;; Use "y or n" answers instead of full words "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;;; Delete selection when typing
(delete-selection-mode t)

;;; Indent with spaces, not tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;;; Autofill at 79 characters
(setq-default fill-column 79)
;;;(global-visual-line-mode 1) ; Wordwrap at word boundaries

;;; Shortcut keys
(global-set-key [(meta g)] 'goto-line)
(define-key global-map [(control z)] 'advertised-undo)
(define-key global-map [(meta backspace)] 'backward-kill-word)
(global-set-key [(control escape)] 'delete-other-windows)
(global-set-key [(control ?`)] 'kill-this-buffer)
(global-set-key [(shift return)] 'newline-and-indent)
(global-set-key [f10] 'speedbar)

;;; Meta-Super-arrow = move the focus between visible buffers
(require 'windmove)
(windmove-default-keybindings 'meta) ;; will be overridden
(global-set-key (kbd "<M-s-left>")  'windmove-left)
(global-set-key (kbd "<M-s-right>") 'windmove-right)
(global-set-key (kbd "<M-s-up>")    'windmove-up)
(global-set-key (kbd "<M-s-down>")  'windmove-down)

;;; Meta-Control-L = switch to last buffer
(defun switch-to-other-buffer ()
  "Alternates between the two most recent buffers"
  (interactive)
  (switch-to-buffer (other-buffer)))
(define-key global-map [(meta control l)] 'switch-to-other-buffer)

;;; Ctrl +/- to zoom in/out
(define-key global-map [(control +)] 'text-scale-increase)
(define-key global-map [(control -)] 'text-scale-decrease)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Files

(setq large-file-warning-threshold nil)

;; Remove trailing blanks on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; Disable backup files (e.g. file~)
(defun no-backup-files ()
  "Disable creation of backup files"
  (interactive)
  (setq make-backup-files nil))
(no-backup-files)
