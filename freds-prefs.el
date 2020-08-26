
;;
;; Make emacs work nicely with eXceed and others (emacs 23 and above)
(setq x-select-enable-primary t)

;;;
;;; Setup path to VM

;;(setq load-path '( "~/Elisp" ))
(add-to-list `load-path "~/Elisp/")
(add-to-list `load-path "~/Elisp/confluence-el/")
(when (string-match "Aquamacs" (emacs-version))
    (load-file "~/Elisp/aquacompat.el")
    (tool-bar-mode 0))
;;
;(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
; '(custom-enabled-themes (quote (wheatgrass)))
; '(display-time-mode t))

;;(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
;; '(default ((t (:family "Bloomberg Fixed Unicode B" :foundry "outline" :slant normal :weight normal :height 143 :width normal)))))
;;(load-file "~/Elisp/edit-server.el")
;;(unless (server-running-p) (server-start))

;; (locate-library "edit-server")
;;(require 'edit-server)
;;(edit-server-start)

(if (not (string-match "XEmacs" (emacs-version)))
       (load-library "advice")
       t)

;;
;; preserve ownership of edited files.
(setq backup-by-copying-when-mismatch t)

;;;
;;; Setup default mode, and create function to handle setting auto-fill-mode
;;; and setting the right margin.

(defun my-text-mode ()
  "Function called when entering text-mode.  Turn off auto-fill and set
the column to wrap at to 72."
  (interactive)
  (set-fill-column 72)
  (auto-fill-mode 0)
)

(defun my-python-mode ()
  "Function called when entering python-mode."
  (interactive)
 (local-set-key (kbd "C-j") 'previous-line)
)

(defun my-enh-ruby-mode ()
  "Function called when entering ruby-mode."
  (interactive)
 (local-set-key (kbd "C-j") 'previous-line)
)

(add-hook 'text-mode-hook 'my-text-mode)
(setq default-major-mode 'text-mode)

(add-hook 'python-mode-hook 'my-python-mode)
(add-hook 'enh-ruby-mode-hook 'my-python-mode)

(global-set-key "\C-x\C-b" 'electric-buffer-list)
;; (define-key ctl-x-map "=" 'goto-line)
(define-key ctl-x-map "=" 'what-cursor-position)

(global-set-key "\C-x " 'set-mark-command)
(global-set-key "\C-x\C-m" 'compile)
(global-set-key "\C-x\C-n" 'next-error)
(global-set-key "i" 'insert-prefix-region)


(defun insert-prefix-region ()
"Insert a prefix at the beginning of each line of a region"
  (interactive)
    (save-restriction
      (setq prefix (read-string "Enter prefix:"))
      (narrow-to-region (mark) (point))
      (goto-char (point-min))
      (while (not (eobp))
	(beginning-of-line)
	(insert prefix)
	(forward-line 1))))


(defun my-indent-region ()
"Indent a region by 4 spaces"
  (interactive)
  (indent-rigidly (mark) (point) 2))

(defun my-outdent-region ()
"remove an indent region by 4 spaces"
  (interactive)
  (indent-rigidly (mark) (point) -2))


;;(setq debug-on-error t)          ;; turn off after libraries are loaded
(display-time)
(global-set-key "\C-x\C-b" 'electric-buffer-list)
;;

(put 'narrow-to-region 'disabled nil)
(put 'eval-expression 'disabled nil)
(fset 'yes-or-no-p 'y-or-n-p)

(defun fredc-function-template ()
  "Insert a function template at current point"
;;
;;
  (interactive)
  (setq fname (read-input "Function Name? "))
  (insert-file-contents "~fred/.Emacs/function.template")
  (search-forward "FUNCTION-NAME" nil t)
  (replace-match fname t 'literal)
  (setq returnpoint (point))
  (search-forward "FUNCTION-NAME" nil t)
  (replace-match fname t 'literal)
  (goto-char returnpoint)

 )

(defun goto-matching-fence ()
  "Goto the matching parentheses of the character at point."
  (interactive)
  (cond
   ((looking-at "\\s(") (goto-char (1- (scan-sexps (point) 1))))
   ((looking-at "\\s)") (goto-char (scan-sexps (1+ (point)) -1)))
   (t (message "Not a valid fence character."))))


;; (if (not (string-match "XEmacs" emacs-version))
 (defadvice scroll-down (around move-point-near-top activate)
  "When called interactively, if the top of the buffer is visible,
just move point to the top of the buffer instead."
  (if (and (interactive-p)
          (pos-visible-in-window-p (point-min)))
      (goto-char (point-min))
    ad-do-it))

 (defadvice scroll-up (around move-point-near-bottom activate)
  "When called interactively, if the bottom of the buffer is visible,
just move point to the bottom of the buffer instead."
  (if (and (interactive-p)
          (pos-visible-in-window-p (point-max)))
      (goto-char (point-max))
    ad-do-it))
;; )

;; Mac specific stuff
;; (setq mac-command-modifier 'control)
;; (setq mac-control-modifier 'meta)

 (global-set-key "\^x!" 'shell-command)
 (global-set-key "\^xs" 'save-buffer)
 (global-set-key "\^O" 'overwrite-mode)
 (global-set-key "\e|" 'execute-macro-14)	;;; execute-macro-14\e|
 (global-set-key "\eN" 'open-line)		;;;  open-line\eN
 (global-set-key "\e\^i" 'execute-macro-15)	;;;  execute-macro-15\e^I
 (global-set-key "\^V" 'recenter)		;;;  clear-and-redraw ^V
 (global-set-key "\e?" 'describe-bindings)	;;;  describe-bindings\e?
;; (global-set-key "\e\^f" 'end-of-line)		;;;  end-of-line\e^F
 (global-set-key "\e\^[" 'execute-command-line)	;;;  execute-command-line\e^[
 (global-set-key "\^f" 'forward-char)		;;;  forward-character ^F
 (global-set-key "\ec" 'forward-char)		;;;  forward-char\eC
 (global-set-key "\eD" 'backward-char)		;;;  backward-character\eD
 (global-set-key "\^x\^n" 'move-window-down)	;;;  move-window-down\^x^N
 (global-set-key "\^x\^U" 'move-window-up)	;;;  move-window-up\^x^U
 (global-set-key "\^N" 'next-line)		;;;  next-line ^N
 (global-set-key "\eB" 'next-line)		;;;  next-line\eB
 (global-set-key "\^K" 'scroll-up)		;;;  next-page ^K
 (global-set-key "\eI" 'next-page)		;;;  next-page\eI
 (global-set-key "\^xO" 'next-window)		;;;  next-window\^xO
 (global-set-key "\eF" 'next-word)		;;;  next-word\eF
 (global-set-key "\^Q" 'delete-previous-word)	;;;  delete-previous-word ^Q
 (global-set-key "\en" 'open-line)		;;;  open-line\eN
 (global-set-key "\eA" 'previous-line)		;;;  previous-line\eA
 (global-set-key "\^J" 'previous-line)		;;;  previous-line ^J
 (global-set-key "\eP" 'backward-page)		;;;  previous-page\eP
 (global-set-key "\^P" 'scroll-down)		;;;  previous-page ^P
 (global-set-key "\^x\^p" 'previous-paragraph)	;;;  previous-paragraph\^x^P
 (global-set-key "\e\^V" 'redraw-display)	;;;  redraw-display\e^V
 (global-set-key "\^xW" 'resize-window)		;;;  resize-window\^xW
 (global-set-key "\^xS" 'save-file)		;;;  save-file\^xS
 (global-set-key "\^W" 'isearch-forward)		;;;  search-forward ^W
 (global-set-key "\^\\" 'goto-matching-fence )
 (global-set-key "\^Y" 'set-mark-command)	;;;  set-mark ^Y
 (global-set-key "\^cc" 'yank)			;;;  yank ^Xc
 (global-set-key "\^x\^u" 'case-region-upper )	;;;  case-region-upper\^x^U
 (global-set-key "\eA" 'previous-line)		;;;  previous-line\eA
 (global-set-key "\^J" 'previous-line)		;;;  previous-line ^J
 (global-set-key "\eP" 'scroll-up)		;;;  previous-page\eP
 (global-set-key "\^xk" 'kill-this-buffer)
 (global-set-key "\^xn" 'rename-buffer)
 (global-set-key "\^h" 'backward-delete-char-untabify)
 (global-set-key "\^x\[" 'my-indent-region )	;;;  indent one step
 (global-set-key "\^x\]" 'my-outdent-region )	;;;  outdent one step
 (global-set-key "\^P" 'scroll-down)		;;;  previous-page ^P
 (global-set-key "\^k" 'scroll-up)
 (global-set-key "\^z" 'kill-line)
 (global-set-key "\^x\^p" 'previous-paragraph)	;;;  previous-paragraph\^x^P
 (global-set-key "\e\^V" 'redraw-display)	;;;  redraw-display\e^V
 (global-set-key "\^xS" 'save-file)		;;;  save-file\^xS
 (global-set-key "\^L" 'kill-region)		;;; bin-to-key kill-region
 (global-set-key "\^x\^u" 'case-region-upper )	;;;  case-region-upper\^x^U
 (global-set-key "\e\^r" 'query-replace )	;;;  case-region-upper\^x^U
 (global-set-key "\^xx" 'bury-buffer)

;;;;; remap ruby keystrokes
 (defun my-ruby-mode ()
     (local-set-key "\C-j" 'previous-line)
     (setq ruby-indent-level 4)
)
;;;;; Make c-mode work nicely.
 (defun dg-c-mode ()
   (setq c-tab-always-indent 'nil)
   (setq c-auto-'newline)
   (setq c-indent-level '0)
   (setq c-continued-statement-offset '4)
   (setq c-basic-offset '4)
   (setq c-brace-imaginary-offset '0)
   (setq c-argdecl-indent '32)
   (setq c-label-offset '0)
   (setq c-tab-always-indent 't)
   (setq indent-tabs-mode nil)
   (local-set-key "\^J" 'previous-line)		;;;  previous-line ^J
   (local-set-key "\^P" 'scroll-down)		;;;  previous-page ^P
   (local-set-key "\^k" 'scroll-up)
   (local-set-key "\^h" 'backward-delete-char-untabify)
   (modify-syntax-entry ?_ "w" c-mode-syntax-table)

   (local-set-key "\e^f" 'fredc-function-template)
 )

(defconst my-c-style
  '((c-tab-always-indent        . t)
    (c-comment-only-line-offset . 4)
    (c-hanging-braces-alist     . ((substatement-open after)
                                   (brace-list-open)))
    (c-hanging-colons-alist     . ((member-init-intro before)
                                   (inher-intro)
                                   (case-label after)
                                   (label after)
                                   (access-label after)))
    (c-cleanup-list             . (scope-operator
                                   empty-defun-braces
                                   defun-close-semi))
    (c-offsets-alist            . ((arglist-close . c-lineup-arglist)
                                   (substatement . 4)
                                   (substatement-open . +)
                                   (substatement-block-intro . 0)
                                   (case-label        . 4)
                                   (block-open        . 0)
                                   (knr-argdecl-intro . -)))
    (c-echo-syntactic-information-p . t)
    )
  "My C Programming Style")

;; Customizations for all modes in CC Mode.
(defun my-c-mode-common-hook ()
  ;; add my personal style and set it for the current buffer
  (c-add-style "PERSONAL" my-c-style t)
  ;; offset customizations not in my-c-style
  (c-set-offset 'member-init-intro '++)
  ;; other customizations
  (setq tab-width 8
        ;; this will make sure spaces are used instead of tabs
        indent-tabs-mode nil)
  ;; we like auto-newline and hungry-delete
  (c-toggle-auto-hungry-state 1)
  ;; keybindings for all supported languages.  We can put these in
  ;; c-mode-base-map because c-mode-map, c++-mode-map, objc-mode-map,
  ;; java-mode-map, idl-mode-map, and pike-mode-map inherit from it.
  (define-key c-mode-base-map "\C-m" 'newline-and-indent)
  )
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)


;; (setq c-mode-hook 'dg-c-mode)
 (add-hook 'c-mode-hook 'dg-c-mode)
 (add-hook 'cperl-mode-hook 'dg-c-mode)
 (add-hook 'ruby-mode-hook 'my-ruby-mode)
 (add-to-list 'auto-mode-alist '("\\.ino\\'" . c++-mode))
 (line-number-mode 1)

  (auto-fill-mode 0)
 (setq scroll-step 1)    ;;; only scroll by 1 when we go off screen

 (setq completion-ignored-extensions
	 '(".o" "~"))

 (setq inhibit-startup-message 't)  ;;; no stupid startup messages
;; (set-face-foreground 'modeline  "red")
(custom-set-variables)
;;(custom-set-faces
;; '(modeline-buffer-id ((t (:foreground "orange"))) t)
;; '(modeline-mousable ((t (:foreground "red"))) t))


(defun dos-unix ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))
(defun unix-dos ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\n" nil t) (replace-match "\r\n")))

;;
;; Sort ^X^b buffer list ...
;;

;; after list-buffers is called, switch to it
(defadvice list-buffers (after jc-switch-to-other-win)
  (if (not (equalp (buffer-name (current-buffer))
                   "*Buffer List*"))
      (other-window 1))
  (goto-char (+ 4 (point))))

;; emacs24 doesn't recognize Buffer-menu-sort-column so we do this
;; nonsense: after list-buffers is called and we've switched to it,
;; check whether the buffer matches what's stored in
;; jc-buffer-menu. If it doesn't match, it means it's new, so call
;; Buffer-menu-sort and update jc-buffer-menu so we don't sort again
;; on subsequent calls.
(when (>= emacs-major-version 24)
  (setq jc-buffer-menu nil)
  (defadvice list-buffers (after jc-buffer-menu-sort last)
    (when (not (equal jc-buffer-menu (current-buffer)))
      (setq jc-buffer-menu (current-buffer))
      ;; for debugging:
      ;;(message "sorting!")
      (Buffer-menu-sort 6))))
(ad-activate 'list-buffers)


;;
;;  Bloomberg specific setup
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Packages from Melpa and Marmelade
;; Use M-x `package-refresh-contents' to update the cache.
;; Use M-x `package-list-package' to load and display the list of packges,
;; then press I to mark for installation and X to execute (it's like dired).

;;(require 'package)
;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/"))
;;(add-to-list 'package-archives
;;             '("melpa" . "http://melpa.org/packages/") t)

;; (when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  ;; (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;;(add-to-list 'package-archives
;;             '("melpa" . "http://melpa.org/packages/") t)
;;(package-initialize)

(defadvice package-compute-transaction
  (before package-compute-transaction-reverse (package-list requirements) activate compile)
    "reverse the requirements"
    (setq requirements (reverse requirements))
    (print requirements))
;;(set-face-background 'hl-line "gray15")
