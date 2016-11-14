;;;; Look and feel
;;;
;;; Keyboard preferences: remaps existing functions to new keys
;;;
;;; ----------------- ---------------------------------------------------------
;;; Key               Definition
;;; ----------------- ---------------------------------------------------------
;;; ESC               Quit (= Ctrl-G)
;;; M-g               Goto line
;;; C-z               Undo
;;; C-`               Kill current buffer (= C-x k)
;;;
;;; RETURN            Return or Return + indent, depending on init-prefs
;;; S-RETURN          The opposite
;;;
;;; M-C-l             Switch to last buffer
;;; C-x C-b           Buffer menu with `ibuffer', replacing `list-buffers'
;;; C- +/-            Zoom

;;; C-=               Expand region by semantic units
;;; M-C-=             Contract region by semantic units
;;;
;;; F10               Speedbar
;;; ----------------- ---------------------------------------------------------

(with-no-warnings (require 'cl))
(require 'init-prefs)


;;; Font

(defvar exordium-available-preferred-fonts
  (remove-if-not (lambda (font-and-size)
                   (member (car font-and-size) (font-family-list)))
                 exordium-preferred-fonts))

(defvar exordium-font-size
  (when exordium-available-preferred-fonts
    (cdar exordium-available-preferred-fonts)))

(defvar exordium-font-name
  (when exordium-available-preferred-fonts
    (caar exordium-available-preferred-fonts)))

(defun exordium-set-font (&optional font size)
  "Find the preferred fonts that are available and choose the first one."
  (interactive
   (list (completing-read (format "Font (default %s): " exordium-font-name)
                          exordium-available-preferred-fonts nil nil nil nil exordium-font-name)
         (read-number "Size: " exordium-font-size)))
  (let ((font (or font exordium-font-name))
        (size (or size exordium-font-size)))
    (when (and font size)
      (message "Setting font family: %s, height: %s" font size)
      (set-face-attribute 'default nil
                          :family font
                          :height size
                          :weight 'normal))))

(when exordium-preferred-fonts
  (exordium-set-font))


;;; User interface

;;; Default frame size
(when (and exordium-preferred-frame-width
           exordium-preferred-frame-height)
  (setq default-frame-alist `((width  . ,exordium-preferred-frame-width)
                              (height . ,exordium-preferred-frame-height))))

;;; Remove the toolbar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;;; Only show the menu bar in a graphical window
;;; (we don't want to loose that top line in a tty)
(menu-bar-mode (if (null (window-system)) -1 1))

;;; Remove welcome message
(setq inhibit-startup-message t)

;;; Disable blinking cursor
(when (fboundp 'blink-cursor-mode)
  (blink-cursor-mode -1))

;;; Display column number in the modebar
(column-number-mode 1)

;;; Smooth scrolling
(setq scroll-step 1)
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      scroll-preserve-screen-position t)

;;; Scrollbar
(if exordium-scroll-bar
    (set-scroll-bar-mode `right)
  (set-scroll-bar-mode nil))

;;; Better frame title with buffer name
(setq frame-title-format (concat "%b - emacs@" system-name))

;;; Disable beep
;;(setq visual-bell t)

;;; Colorize selection
(transient-mark-mode 'on)

;;; Show matching parentheses
(show-paren-mode t)

;;; Mouse selection
(setq x-select-enable-clipboard t)

;;; http://www.reddit.com/r/emacs/comments/30g5wo/the_kill_ring_and_the_clipboard/
(setq save-interprogram-paste-before-kill t)

;;; Electric pair: automatically close parenthesis, curly brace etc.
;;; `electric-pair-open-newline-between-pairs'.
(when exordium-enable-electric-pair-mode
  (setq electric-pair-open-newline-between-pairs t)
  (electric-pair-mode))

;;; Indent with spaces, not tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;;; Autofill at 79 characters
(setq-default fill-column 79)

;;; Wordwrap at word boundadies
;;;(global-visual-line-mode 1)

;; Show only 1 window on startup (useful if you open multiple files)
(add-hook 'emacs-startup-hook (lambda () (delete-other-windows)) t)


;;; Keyboard preferences

;; Use ESC as Control-G
(when exordium-keyboard-escape
  (global-set-key (kbd "<escape>") 'keyboard-quit))

;;; Use "y or n" answers instead of full words "yes or no"
(when exordium-enable-y-or-n
  (fset 'yes-or-no-p 'y-or-n-p))

;;; Delete selection when typing
(delete-selection-mode t)

;;; Let me scroll the buffer while searching, without exiting the search.
;;; This allows for using C-l during isearch.
(when (boundp 'isearch-allow-scroll)
  (setq isearch-allow-scroll t))

;;; Evil-mode
(require 'evil)
(if (and exordium-enable-evil-mode (fboundp 'evil-mode))
    (evil-mode t)
  ;; Evil mode depends in undo-tree, which thinks it should work by default
  (when (boundp 'global-undo-tree-mode)
    (global-undo-tree-mode -1)))


;;; Shortcut keys

(global-set-key [(meta g)] (function goto-line))
(when exordium-keyboard-ctrl-z-undo
  (define-key global-map [(control z)] (function undo)))
(global-set-key [(control ?`)] (function kill-this-buffer))


;;; Meta-Control-L = switch to last buffer
(defun switch-to-other-buffer ()
  "Alternates between the two most recent buffers"
  (interactive)
  (switch-to-buffer (other-buffer)))

(define-key global-map [(meta control l)] (function switch-to-other-buffer))

;;; C-x C-b = ibuffer (better than list-buffers)
(define-key global-map [(control x)(control b)] (function ibuffer))

;;; Zoom
(require 'default-text-scale)
(define-key global-map [(control +)] (function default-text-scale-increase))
(define-key global-map [(control -)] (function default-text-scale-decrease))
(define-key global-map [(control mouse-4)] (function default-text-scale-increase))
(define-key global-map [(control mouse-5)] (function default-text-scale-decrease))

;;; CUA.
;;; CUA makes C-x, C-c and C-v cut/copy/paste when a region is selected.
;;; Adding shift or doubling the Ctrl-* makes it switch back to Emacs keys.
;;; It also has a nice feature: C-RET for selecting rectangular regions.
;;; If exordium-enable-cua-mode is nil, only the rectangular regions are enabled.
(cond ((eq exordium-enable-cua-mode :region)
       (cua-selection-mode t))
      (exordium-enable-cua-mode
       (cua-mode t)))


;;; Cool extensions

;;; Expand region
(require 'expand-region)
(global-set-key (kbd "C-=") (function er/expand-region))
(global-set-key (kbd "M-C-=") (function er/contract-region))


;;; File saving and opening

;; Warn when opening files bigger than 100MB (use nil to disable it entirely)
(setq large-file-warning-threshold 100000000)

;; Propose vlf (Very Large File) as a choice when opening large files
;; (otherwise one can open a file using M-x vlf):
(require 'vlf-setup)

;; Remove trailing blanks on save
(define-minor-mode delete-trailing-whitespace-mode
  "Remove trailing whitespace upon saving a buffer"
  :lighter nil
  (if delete-trailing-whitespace-mode
      (add-hook 'before-save-hook #'delete-trailing-whitespace nil t)
    (remove-hook 'before-save-hook #'delete-trailing-whitespace t)))

(define-globalized-minor-mode global-delete-trailing-whitespace-mode
  delete-trailing-whitespace-mode
  (lambda ()
    (delete-trailing-whitespace-mode t)))

(when exordium-delete-trailing-whitespace
  (global-delete-trailing-whitespace-mode t))

;;; Disable backup files (e.g. file~)
(defun no-backup-files ()
  "Disable creation of backup files"
  (interactive)
  (setq make-backup-files nil))

(unless exordium-backup-files
  (no-backup-files))



(provide 'init-look-and-feel)
