;;;; Key bindings
;;;
;;;; Keyboard preferences
;;;
;;; ----------------- ---------------------------------------------------------
;;; Key               Definition
;;; ----------------- ---------------------------------------------------------
;;; ESC               Quit (= Ctrl-G)
;;; Meta-g            Goto line
;;; Ctrl-z            Undo
;;; Meta-backspace    Delete word
;;; Ctrl-esc          Delete other windows
;;; Ctrl-`            Kill current buffer (= Ctrl-x k)
;;;
;;; Return/shift      Return and Return + indent, depending on init-prefs
;;;
;;; Meta-Shift-arrow  Move between windows (= Ctrl-x o)
;;; Meta-ctrl-l       Switch to last buffer
;;; Ctrl +/-          Zoom
;;;
;;; Ctrl-x g          Magit status
;;; F10               Speedbar
;;; ----------------- ---------------------------------------------------------

;; Use ESC as Control-G (default requires ESC ESC ESC)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;;; Use "y or n" answers instead of full words "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;;; Delete selection when typing
(delete-selection-mode t)

;;; Shortcut keys
(global-set-key [(meta g)] 'goto-line)
(define-key global-map [(control z)] 'advertised-undo)
(define-key global-map [(meta backspace)] 'backward-kill-word)
(global-set-key [(control escape)] 'delete-other-windows)
(global-set-key [(control ?`)] 'kill-this-buffer)

;;; The return key
(cond (*init-enable-newline-and-indent*
       (global-set-key "\C-m" 'newline-and-indent)
       (global-set-key [(shift return)] 'newline))
      (t
       (global-set-key [(shift return)] 'newline-and-indent)))

;;; Meta-Shif-arrow = move the focus between visible buffers
(require 'windmove)
(global-set-key [(meta shift left)] 'windmove-left)
(global-set-key [(meta shift right)] 'windmove-right)
(global-set-key [(meta shift up)] 'windmove-up)
(global-set-key [(meta shift down)] 'windmove-down)

;;; Meta-Control-L = switch to last buffer
(defun switch-to-other-buffer ()
  "Alternates between the two most recent buffers"
  (interactive)
  (switch-to-buffer (other-buffer)))
(define-key global-map [(meta control l)] 'switch-to-other-buffer)

;;; Ctrl +/- to zoom in/out
(define-key global-map [(control +)] 'text-scale-increase)
(define-key global-map [(control -)] 'text-scale-decrease)

;;; Ctrl-x g = magit-status
(define-key global-map [(control x)(g)] 'magit-status)

;;; Speed bar
(global-set-key [f10] 'speedbar)

(provide 'init-keyboard)
