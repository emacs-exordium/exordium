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
;; C-j               Show help for currentlyselected candidate, when completing
;;                   read for `helpful' commands
;; C-c C-d           Show help for thing at point (in `emacs-lisp-mode')
;; C-o               Show a `casual' transient, which one depends on mode.
;; C-c =             Run `difftastic-dired-diff' in `dired-mode'.


;;; Code:
(eval-when-compile
  (unless (featurep 'init-require)
    (load (file-name-concat (locate-user-emacs-file "modules") "init-require"))))
(exordium-require 'init-lib)
(exordium-require 'init-helm)
(exordium-require 'init-git)

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
  :functions (exordium--helpful-persistent-action
              exordium--helm-helpful-completing-read)
  :init
  (use-package helm
    :defer t
    :custom
    (helm-describe-variable-function #'helpful-variable)
    (helm-describe-function-function #'helpful-function))
  (use-package helm-mode
    :ensure helm
    :defer t
    :autoload (helm-completing-read-default-handler))

  (defun exordium--helpful-pop-to-buffer (buffer)
    "Pop to BUFFER in the same window if it is a Helpful window.
Otherwise pop to buffer (presumably in a new window)."
    (if (derived-mode-p 'helpful-mode)
        (pop-to-buffer-same-window buffer)
      (pop-to-buffer buffer)))

  (defun exordium--helpful-persistent-action (type)
    "Generate a function that adds `:persistent-action' TYPE to args."
    (lambda (&rest args)
      (plist-put (plist-put (car args)
                            :persistent-help
                            (format "Describe %s" type))
                 :persistent-action
                 (lambda (candidate)
                   (pcase (intern-soft candidate)
                     ((and (pred fboundp) (pred boundp) sym)
                      (if (eq type 'variable)
                          (helm-describe-variable sym)
                        (helm-describe-function sym)))
                     ((and (pred boundp) sym)
                      (helm-describe-variable sym))
                     ((and (pred fboundp) sym)
                      (helm-describe-function sym))
                     ((and (pred facep) sym)
                      (helm-describe-face sym)))))))

  (defun exordium--helm-helpful-completing-read (&rest args)
                                        ; checkdoc-params: (args)
    "Ensure affixation and persistent actions are used."
    (let* ((current-command (or (helm-this-command) this-command))
           (str-command (if current-command
                            (helm-symbol-name current-command)
                          "completing-read"))
           (buf-name (format "*%s*" str-command))
           (type (cond
                  ((eq current-command 'helpful-variable) 'variable)
                  ((eq current-command 'helpful-symbol) 'symbol)
                  (t 'function)))
           (persistent-action (exordium--helpful-persistent-action type)))
      (unwind-protect
          (let ((completion-extra-properties
                 '(:affixation-function
                   helm-symbol-completion-table-affixation)))
            (advice-add 'helm-comp-read
                        :filter-args persistent-action)
            (apply #'helm-completing-read-default-handler
                   (append args
                           (list str-command buf-name))))
        (advice-remove 'helm-comp-read persistent-action))))

  :custom
  ;; By default `show-paren-mode' is disabled in modes deriving from
  ;; `special-mode'.  Enable it for `helpful' if it doesn't match
  ;; the `show-paren-predicate'
  (show-paren-predicate (if (with-temp-buffer
                              (require 'helpful nil t)
                              (helpful-mode)
                              (buffer-match-p show-paren-predicate
                                              (current-buffer)))
                            show-paren-predicate
                          (list 'or '(derived-mode . helpful-mode)
                                show-paren-predicate)))
  (helpful-switch-buffer-function #'exordium--helpful-pop-to-buffer)
  (completions-detailed t)

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
   ("C-c C-o" . #'exordium-browse-url-at-point))
  :config
  (require 'helm-mode)
  (dolist (fun '(helpful-callable
                 helpful-command
                 helpful-function
                 helpful-macro
                 helpful-variable
                 helpful-symbol))
    (add-to-list 'helm-completing-read-handlers-alist
                 (cons fun #'exordium--helm-helpful-completing-read))))


(when (version< "29" emacs-version) ;; Since Emacs-29

(use-package casual
  :defer t
  :bind ("C-o" . #'casual-editkit-main-tmenu)
  :init
  (use-package org-agenda
    :ensure nil
    :defer t
    :commands (org-agenda-clock-goto)
    :bind (:map org-agenda-mode-map
           ("C-o" . #'casual-agenda-tmenu)
           ("M-j" . #'org-agenda-clock-goto)
           ("J" . #'bookmark-jump)))
  (use-package bookmark
    :ensure nil
    :defer t
    :bind (:map bookmark-bmenu-mode-map
           ("C-o" .  #'casual-bookmarks-tmenu)
           ("J" . #'bookmark-jump)))
  (use-package calc
    :ensure nil
    :defer t
    :bind (:map calc-mode-map
           ("C-o" . #'casual-calc-tmenu)))
  (use-package calc-ext
    :ensure nil
    :defer t
    :bind (:map calc-alg-map
           ("C-o" . #'casual-calc-tmenu)))
  (use-package calendar
    :ensure nil
    :defer t
    :bind (:map calendar-mode-map
           ("C-o" . #'casual-calendar)))
  (use-package dired
    :ensure nil
    :defer t
    :bind (:map dired-mode-map
           ("C-o" . #'casual-dired-tmenu)
           ("s" . #'casual-dired-sort-by-tmenu)
           ("/" . #'casual-dired-search-replace-tmenu)
           ("C-c =" . #'difftastic-dired-diff)))
  (use-package ibuffer
    :init
    (use-package ibuf-ext
      :ensure nil
      :commands (ibuffer-backwards-next-marked
                 ibuffer-forward-next-marked
                 ibuffer-backward-filter-group
                 ibuffer-forward-filter-group
                 ibuffer-toggle-filter-group))
    :ensure nil
    :defer t
    :bind (:map ibuffer-mode-map
           ("C-o" . #'casual-ibuffer-tmenu)
           ("F" . #'casual-ibuffer-filter-tmenu)
           ("s".  #'casual-ibuffer-sortby-tmenu)
           ("{" . #'ibuffer-backwards-next-marked)
           ("}" . #'ibuffer-forward-next-marked)
           ("[" . #'ibuffer-backward-filter-group)
           ("]" . #'ibuffer-forward-filter-group)
           ("$" . #'ibuffer-toggle-filter-group)))
  (use-package info
    :ensure nil
    :defer t
    :bind (:map Info-mode-map
           ("C-o" . #'casual-info-tmenu)
           ("M-[" . #'Info-history-back)
           ("M-]" . #'Info-history-forward)
           ("/" . #'Info-search)
           ("B" . #'bookmark-set)))
  (use-package isearch
    :ensure nil
    :defer t
    :bind (:map isearch-mode-map
           ("C-o" . #'casual-isearch-tmenu)))
  (use-package re-builder
    :ensure nil
    :defer t
    :bind (:map reb-mode-map
           ("C-o". #'casual-re-builder-tmenu)
           :map reb-lisp-mode-map
           ("C-o" . #'casual-re-builder-tmenu)))
  :config
  (with-eval-after-load 'casual-dired
    (let ((loc '(1 -1))
          (suffix ["Compare"
                   ("=" "Diff" dired-diff)
                   ("C-c =" "Difftastic" difftastic-dired-diff)]))
      (unless (equal (transient-parse-suffix 'casual-transient-tmenu suffix)
                     (transient-get-suffix 'casual-dired-tmenu loc))
        (transient-append-suffix 'casual-dired-tmenu loc suffix)))))
) ;; (when (version< "29" emacs-version) ;; Since Emacs-29


(provide 'init-help)

;;; init-help.el ends here
