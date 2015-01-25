;;;; Keyboard preferences: remaps existing functions to new keys
;;;
;;; ----------------- ---------------------------------------------------------
;;; Key               Definition
;;; ----------------- ---------------------------------------------------------
;;; ESC               Quit (= Ctrl-G)
;;; M-g               Goto line
;;; C-z               Undo
;;; C-ESC             Delete other windows
;;; C-`               Kill current buffer (= C-x k)
;;;
;;; RETURN            Return or Return + indent, depending on init-prefs
;;; S-RETURN          The opposite
;;;
;;; M-S-ARROW         Move between windows (= Ctrl-x o)
;;; M-C-l             Switch to last buffer
;;; C-x C-b           Buffer menu with `ibuffer', replacing `list-buffers'
;;; C- +/-            Zoom
;;;
;;; F10               Speedbar
;;; ----------------- ---------------------------------------------------------

;; Use ESC as Control-G (default requires ESC ESC ESC)
(when *init-keyboard-escape*
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit))

;;; Use "y or n" answers instead of full words "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;;; Delete selection when typing
(delete-selection-mode t)

;;; Shortcut keys
(global-set-key [(meta g)] 'goto-line)
(define-key global-map [(control z)] 'advertised-undo)
(global-set-key [(control escape)] 'delete-other-windows)
(global-set-key [(control ?`)] 'kill-this-buffer)

;;; The return key
(cond (*init-enable-newline-and-indent*
       (global-set-key "\C-m" 'newline-and-indent)
       (global-set-key [(shift return)] 'newline))
      (t
       (global-set-key [(shift return)] 'newline-and-indent)))


;;; Electric pair

(when *init-enable-electric-pair-mode*
  (electric-pair-mode))


;;; Winmove

;;; Meta-Shift-arrow = move the focus between visible buffers
(require 'windmove)
(global-set-key [(meta shift left)] 'windmove-left)
(global-set-key [(meta shift right)] 'windmove-right)
(global-set-key [(meta shift up)] 'windmove-up)
(global-set-key [(meta shift down)] 'windmove-down)


;;; Buffers

;;; Meta-Control-L = switch to last buffer
(defun switch-to-other-buffer ()
  "Alternates between the two most recent buffers"
  (interactive)
  (switch-to-buffer (other-buffer)))
(define-key global-map [(meta control l)] 'switch-to-other-buffer)

;;; C-x C-b = ibuffer (better than list-buffers)
(define-key global-map [(control x)(control b)] 'ibuffer)


;;; Zoom

(define-key global-map [(control +)] 'text-scale-increase)
(define-key global-map [(control -)] 'text-scale-decrease)


;;; Project explorer

(define-key global-map [(control c)(e)] 'project-explorer-open)


;;; CUA
;;; CUA makes C-x, C-c and C-v cut/copy/paste when a region is selected.
;;; Adding shift or doubling the Ctrl-* makes it switch back to Emacs keys.
;;; It also has a nice feature: C-RET for selecting rectangular regions.
;;; If *init-enable-cua-mode* is nil, only the rectangular regions are enabled.

(cond ((eq *init-enable-cua-mode* :region)
       (cua-selection-mode t))
      (*init-enable-cua-mode*
       (cua-mode t)))


;;; Expand region

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)


(provide 'init-keyboard)
