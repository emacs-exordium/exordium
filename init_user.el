;;;; User preferences

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Visual

;;; Font
(when (emacs-osx-p)
 (set-face-attribute 'default nil
                     :family "Consolas" :height 120 :weight 'normal)
 (setq default-frame-alist '((width . 100)
                             (height . 65))))

(when (and (emacs-x-p) (getenv "MBIG_NUMBER"))
  (setq default-frame-alist
        (append `(;;(font . ,(choose-frame-font))
                  (font . "Monospace 12")
                  ;;(font . "-*-verdana-medium-r-*-*-12-*-*-*-*-*-*-*")
                  ;;(font . "-*-consolas-medium-r-*-*-*-*-*-*-*-*-*-*")
                  ;;(font . "-*-courier-*-r-*-*-14-*-*-*-*-*-*-*")
                  (width . 120)
                  (height . 65)
                  (vertical-scroll-bars . right)
                  (internal-border-width . 0)
                  ;;(border-width . 0)
                  (horizontal-scroll-bars . t))
                default-frame-alist)))

;;; Remove the toolbar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;;; Remove the menu bar
;;(menu-bar-mode -1)

;;; Remove welcome message
(setq inhibit-startup-message t)

;;; Disable blinking cursor
(when (fboundp 'blink-cursor-mode)
  (blink-cursor-mode -1))

;;; Display column number and line numbers
(column-number-mode 1)
(if (boundp 'global-linum-mode)
    (global-linum-mode t))

;;; Highlight cursor
(global-hl-line-mode 1)

;;; Smooth scrolling
(setq scroll-step 1)
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-up-aggressively 0
      scroll-down-aggressively 0
      scroll-preserve-screen-position t)

;;; Scrollbar on the right
;;(setq scroll-bar-mode-explicit t)
;;(set-scroll-bar-mode `right)

;;; Syntax highlighing
(global-font-lock-mode 1)
(setq font-lock-maximum-decoration t)  ; Maximum colors
(when (emacs-24-p)
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
  ;;(setq fci-always-use-textual-rule t)
  )

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keyboard

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

;;; Meta-G         = goto
;;; Ctrl-Z         = undo
;;; Meta-Backspace = delete word
;;; Ctrl-Esc       = delete other windows
;;; Ctrl-`         = kill current buffer (Ctrl-X K)
;;; Meta-Enter     = return + tab
;;; f10            = speedbar
(global-set-key [(meta g)] 'goto-line)
(define-key global-map [(control z)] 'advertised-undo)
(define-key global-map [(meta backspace)] 'backward-kill-word)
(global-set-key [(control escape)] 'delete-other-windows)
(global-set-key [(control ?`)] 'kill-this-buffer)
(global-set-key [(meta m)] 'newline-and-indent)
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
