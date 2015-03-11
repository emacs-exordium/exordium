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
;;; C-ESC             Delete other windows
;;; C-`               Kill current buffer (= C-x k)
;;;
;;; RETURN            Return or Return + indent, depending on init-prefs
;;; S-RETURN          The opposite
;;;
;;; M-ARROW           Move between windows (= Ctrl-x o)
;;; M-C-l             Switch to last buffer
;;; C-x C-b           Buffer menu with `ibuffer', replacing `list-buffers'
;;; C- +/-            Zoom
;;;
;;; F10               Speedbar
;;; ----------------- ---------------------------------------------------------

(with-no-warnings (require 'cl))


;;; Font

(defun init-set-font ()
  "Find the preferred fonts that are available and choose the first one."
  (let* ((available-fonts (font-family-list))
         (available-preferred-fonts
          (remove-if-not (lambda (font-and-size)
                           (member (car font-and-size) available-fonts))
                         *init-preferred-fonts*)))
    (when available-preferred-fonts
      (let ((preferred-font (caar available-preferred-fonts))
            (preferred-size (cdar available-preferred-fonts)))
        (message "Setting font: %s %d" preferred-font preferred-size)
        (set-face-attribute 'default nil
                            :family preferred-font
                            :height preferred-size
                            :weight 'normal)))))

(when *init-preferred-fonts*
  (init-set-font))


;;; User interface

;;; Default frame size
(when (and *init-preferred-frame-width*
           *init-preferred-frame-height*)
  (setq default-frame-alist `((width  . ,*init-preferred-frame-width*)
                              (height . ,*init-preferred-frame-height*))))

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

;;; Highlight the line where the cursor is
(when *init-line-mode*
  (global-hl-line-mode 1))

;;; Smooth scrolling
(setq scroll-step 1)
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      scroll-preserve-screen-position t)

;;; Scrollbar on the right
;;(setq scroll-bar-mode-explicit t)
;;(set-scroll-bar-mode `right)

;;; Syntax highlighing
(global-font-lock-mode 1)
(setq font-lock-maximum-decoration
      '((emacs-lisp-mode . t)
        (c-mode . t)
        (C++-mode . 1) ;; t or 1 or 2
        (t . t)))

;; Lazy font-lock to avoid the bug in Emacs 24
(cond ((fboundp 'jit-lock-mode)
       (setq jit-lock-chunk-size 5000
             jit-lock-context-time 0.2
             jit-lock-defer-time .1
             jit-lock-stealth-nice 0.2
             jit-lock-stealth-time 5
             jit-lock-stealth-verbose nil)
       (jit-lock-mode t))
      ((fboundp 'turn-on-lazy-shot)
       (add-hook 'font-lock-mode-hook 'turn-on-lazy-shot))
      ((fboundp 'turn-on-lazy-lock)
       (add-hook 'font-lock-mode-hook 'turn-on-lazy-lock)
       (setq lazy-lock-stealth-time 10)
       (setq lazy-lock-minimum-size 10000)))
;;(fci-always-use-textual-rule t)

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

;;; Electric pair: automatically close parenthesis, curly brace etc.
(when *init-enable-electric-pair-mode*
  (electric-pair-mode))

;;; Indent with spaces, not tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;;; Autofill at 79 characters
(setq-default fill-column 79)

;;; Wordwrap at word boundadies
;;;(global-visual-line-mode 1)

;;; Display page breaks with an horizontal line instead of ^L.
;;; Note: To insert a page break: C-q C-l
;;;       To jump to the previous/next page break: C-x [ and C-x ]
(require 'page-break-lines)
(global-page-break-lines-mode 1)
(diminish 'page-break-lines-mode)

;; Show only 1 window on startup (useful if you open multiple files)
(add-hook 'emacs-startup-hook (lambda () (delete-other-windows)) t)


;;; Keyboard preferences

;; Use ESC as Control-G (default requires ESC ESC ESC)
;;(when *init-keyboard-escape*
;;  (global-set-key (kbd "<escape>") 'keyboard-escape-quit))

;;; Use "y or n" answers instead of full words "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;;; Delete selection when typing
(delete-selection-mode t)

;;; Shortcut keys
(global-set-key [(meta g)] 'goto-line)
(define-key global-map [(control z)] 'undo)
(global-set-key [(control escape)] 'delete-other-windows)
(global-set-key [(control ?`)] 'kill-this-buffer)

;;; The return key
(cond (*init-enable-newline-and-indent*
       (global-set-key "\C-m" 'newline-and-indent)
       (global-set-key [(shift return)] 'newline))
      (t
       (global-set-key [(shift return)] 'newline-and-indent)))

;;; Winmove:  Meta-Shift-arrow = move the focus between visible buffers
(require 'windmove)
(global-set-key [(meta left)] 'windmove-left)
(global-set-key [(meta right)] 'windmove-right)
(global-set-key [(meta up)] 'windmove-up)
(global-set-key [(meta down)] 'windmove-down)

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


;;; File saving and opening

;; Warn when opening files bigger than 100MB (use nil to disable it entirely)
(setq large-file-warning-threshold 100000000)

;; Remove trailing blanks on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; Disable backup files (e.g. file~)
(defun no-backup-files ()
  "Disable creation of backup files"
  (interactive)
  (setq make-backup-files nil))
(no-backup-files)

;; Reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
;;(setq gc-cons-threshold 50000000)


(provide 'init-look-and-feel)
