;;; init-help.el --- Help extensions                 -*- lexical-binding: t -*-

;;; Commentary:
;;
;; ----------------- ---------------------------------------------------------
;; Key               Definition
;; ----------------- ---------------------------------------------------------
;; C-c C-o           Open URL at point (in `help-mode' and `helpful-mode')
;; C-h f             Show help for function, macro or special form
;; C-h F             Show help for function
;; C-h v             Show help for variable
;; C-h k             Show help for interactive command bound to key sequence
;; C-h C             Show help for interactive command
;; C-c C-d           Show help for thing at point (in `emacs-lisp-mode')



;;; Code:
(eval-when-compile
  (unless (featurep 'init-require)
    (load (file-name-concat (locate-user-emacs-file "modules") "init-require"))))
(exordium-require 'init-lib)
(exordium-require 'init-helm)
(exordium-require 'init-highlight)

;;; Which Key - display available keybindings in popup.
(when exordium-enable-which-key
  (use-package which-key
    :pin gnu
    :diminish
    :config
    (which-key-mode)))


;; Tune keys in `help-mode' - i.e., works when reading package information in
;; `package-list-packages'.

(use-package help-mode
  :ensure nil
  :bind
  (:map help-mode-map
   ("C-c C-o" . #'exordium-browse-url-at-point)))


(use-package helpful
  :init
  (use-package helm
    :defer t
    :custom
    (helm-describe-variable-function #'helpful-variable)
    (helm-describe-function-function #'helpful-function))

  ;; TODO: seems like `font-lock-add-keywords' destroys all formating in
  ;; `helpful-mode'.  The former is used by `highlight-symbol' so will
  ;; not enable it now.
  ;; (when exordium-highlight-symbol
  ;;   (use-package highlight-symbol
  ;;     :defer t
  ;;     :hook ((helpful-mode . highlight-symbol-mode)
  ;;            (helpful-mode . highlight-symbol-nav-mode))))

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
   :map helpful-mode-map
   ("C-c C-d" . #'helpful-at-point)
   ("C-c C-o" . #'exordium-browse-url-at-point)))



(provide 'init-help)

;;; init-help.el ends here
