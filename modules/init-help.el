;;;; Help extensions
;;;
;;; ----------------- ---------------------------------------------------------
;;; Key               Definition
;;; ----------------- ---------------------------------------------------------
;;; C-c C-o           Open URL at point (in `help-mode' and `helpful-mode')
;;; C-h f             Show help for function, macro or special form
;;; C-h F             Show help for function
;;; C-h v             Show help for variable
;;; C-h k             Show help for interactive command bound to key sequence
;;; C-h C             Show help for interactive command
;;; C-c C-d           Show help for thing at point (in `emacs-lisp-mode')


;;; Which Key - display available keybindings in popup.
(use-package which-key
  :diminish
  :config
  (which-key-mode))


;; Tune keys in `help-mode' - i.e., works when reading package information in
;; `package-list-packages'.

(use-package help-mode
  :ensure nil
  :bind
  (:map help-mode-map
        ("C-c C-o" . #'exordium-browse-url-at-point)))


(use-package page-break-lines
  :diminish
  :hook
  (help-mode . page-break-lines-mode))

(use-package helpful
  :bind
  (;; Note that the built-in `describe-function' includes both functions
   ;; and macros. `helpful-function' is functions only, so we provide
   ;; `helpful-callable' as a drop-in replacement.
   ("C-h f" . #'helpful-callable)
   ;; Look up *F*unctions (excludes macros).
   ;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
   ;; already links to the manual, if a function is referenced there.
   ("C-h F" . #'helpful-function)
   ("C-h v" . #'helpful-variable)
   ("C-h k" . #'helpful-key)
   ;; Look up *C*ommands.
   ;; By default, C-h C is bound to describe `describe-coding-system'.
   ;; Apparently it's frequently useful to only look at interactive functions.
   ("C-h C" . #'helpful-command)
   ;; Lookup the current symbol at point. C-c C-d is a common keybinding
   ;; for this in lisp modes.
   :map emacs-lisp-mode-map
        ("C-c C-d" . #'helpful-at-point)
   :map helpful-mode-map
        ("C-c C-d" . #'helpful-at-point)
        ("C-c C-o" . #'exordium-browse-url-at-point)))

(use-package helm
  :diminish
  :custom
  (helm-describe-variable-function #'helpful-variable)
  (helm-describe-function-function #'helpful-function))


(provide 'init-help)
