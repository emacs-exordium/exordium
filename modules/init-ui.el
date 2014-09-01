;;;; Emacs looks and feel
;;; - Fonts
;;; - Toolbar
;;; - Menubar
;;; - Splash screen
;;; - Blinking cursor
;;; - Scrolling
;;; - Copy and paste
;;; - Font lock

(require 'cl)

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

(init-set-font)

;;; Frame size
(when (and *init-preferred-frame-width*
           *init-preferred-frame-height*)
  (setq default-frame-alist `((width  . ,*init-preferred-frame-width*)
                              (height . ,*init-preferred-frame-height*))))

;; (when *environment-osx*
;;  (set-face-attribute 'default nil
;;                      :family "Consolas" :height 120 :weight 'normal)
;;  (setq default-frame-alist '((width . 100)
;;                              (height . 65))))

;; (when *environment-bloomberg*
;;   (setq default-frame-alist
;;         (append `(;;(font . ,(choose-frame-font))
;;                   (font . "Monospace 13")
;;                   ;;(font . "-*-verdana-medium-r-*-*-12-*-*-*-*-*-*-*")
;;                   ;;(font . "-*-consolas-medium-r-*-*-*-*-*-*-*-*-*-*")
;;                   ;;(font . "-*-courier-*-r-*-*-14-*-*-*-*-*-*-*")
;;                   (width . 120)
;;                   (height . 65)
;;                   (vertical-scroll-bars . right)
;;                   (internal-border-width . 0)
;;                   ;;(border-width . 0)
;;                   (horizontal-scroll-bars . t))
;;                 default-frame-alist)))

;; (when (and *environment-linux* (not *environment-bloomberg*))
;;   (set-face-attribute 'default nil
;;                       :family "Mono" :height 120 :weight 'normal)
;;   (setq default-frame-alist '((width . 110)
;;                               (height . 65))))

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

(provide 'init-ui)
