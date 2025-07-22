;;; init-helm.el --- Helm - see http://tuhdo.github.io/helm-intro.html -*- lexical-binding: t -*-

;;; Commentary:
;;
;; ----------------- -------------------------------------------------------
;; Key               Definition
;; ----------------- -------------------------------------------------------
;; M-x               Remap standard: Execute command with helm.
;; M-y               Remap standard: Yank with helm.
;; C-x b             Remap standard: Switch buffer with helm.
;; C-x C-f           Remap standard: Find file with helm.
;; C-x C-r           Open recent file with Helm (see also `init-ido.el').
;; C-h b             Describe keybindings using Helm.
;; C-S-r             Search with ripgrep: in current project root.
;;                   See also`init-helm-porojectile.el'
;; C-S-d             Search with Ag: ask for directory first.
;;                   See also`init-helm-porojectile.el'.
;; C-S-s             Helm occur
;; C-x c g           Helm Google suggest.
;; C-c C-p           Edit helm ag/grep/occur etc. search results (after exporting/saving them)

;;; Code:

(eval-when-compile
  (unless (featurep 'init-require)
    (load (file-name-concat (locate-user-emacs-file "modules") "init-require"))))
(exordium-require 'init-prefs)

(require 'cl-lib)

(use-package helm
  :diminish
  :bind
  (:map helm-command-map
   ("g" . #'helm-google-suggest))
  :custom
  (helm-split-window-default-side 'other)

  (helm-split-window-other-side-when-one-window
   (if (eq exordium-split-window-preferred-direction 'horizontal)
       'right
     'below))

  (helm-split-window-preferred-function
   (if (eq exordium-split-window-preferred-direction 'longest)
       #'split-window-sensibly
     #'helm-split-window-default-fn))

  (helm-buffer-max-length nil)
  (helm-completion-style (cond
                          ((and exordium-helm-fuzzy-match
                                (eq exordium-helm-completion-style 'helm))
                           ;; backward compatibility
                           'helm-fuzzy)
                          ((eq exordium-helm-completion-style 'orderless)
                           'emacs)
                          (t exordium-helm-completion-style))))

(when (eq exordium-helm-completion-style 'orderless)
  (use-package orderless
    :custom
    (completion-styles '(orderless basic))
    (completion-category-overrides '((file (styles basic partial-completion))))))

(use-package helm
  :diminish
  :when exordium-helm-everywhere
  :functions (exordium--helm-swith-to-buffer-update-sources
              exordium--helm-switch-to-buffer-completing-read
              exordium--helm-occur-ensure-input
              exordium-helm-do-grep-ag-in-directory)
  :init
  (use-package helm-lib
    :ensure helm
    :defer t
    :autoload (helm-this-command
               helm-symbol-name
               helm-get-attr
               helm-set-attr))
  (use-package helm-mode
    :ensure helm
    :defer t
    :autoload (helm-completing-read-default-handler))
  (use-package helm-source
    :ensure helm
    :defer t
    :autoload (helm-make-source))
  (use-package helm-core
    :ensure helm
    :defer t
    :autoload (helm-normalize-sources
               helm-set-local-variable))
  (use-package helm-grep
    :ensure helm
    :defer t
    :autoload (helm-grep-ag))

  (use-package helm-buffers
    :ensure helm
    :defer t
    :autoload (helm-buffer--format-mode-name))

  (defun exordium--helm-swith-to-buffer-update-sources (&rest args)
    "Copy relevant attributes from a `helm-source-buffers' to `:sources' in ARGS."
    (if-let* ((args (car args))
              ((plistp args))
              (sources (cl-remove-if (lambda (source)
                                       (equal "Unknown candidate"
                                              (helm-get-attr 'name source)))
                                     (helm-normalize-sources
                                      (plist-get args :sources))))
              (source-buffers (helm-make-source "Buffers" 'helm-source-buffers)))
        (progn
          (dolist (source sources)
            (helm-set-attr 'filtered-candidate-transformer
                           (append '(helm-skip-boring-buffers
                                     helm-buffers-sort-transformer)
                                   (helm-get-attr 'filtered-candidate-transformer
                                                  source))
                           source)
            (helm-set-attr 'action
                           (append (let ((action (helm-get-attr 'action source)))
                                     (if (listp action)
                                         (list (car action))
                                       (list action)))
                                   (cdr helm-type-buffer-actions))
                           source)
            (dolist (attr '(keymap
                            persistent-action
                            persistent-help
                            help-message
                            match
                            mode-line
                            heder-line
                            find-file-target))
              (helm-set-attr attr
                             (helm-get-attr attr source-buffers)
                             source)))
          (plist-put args
                     :sources (append sources
                                      (list helm-source-buffer-not-found))))
      args))

  (defun exordium--helm-switch-to-buffer-completing-read (&rest args)
                                        ; checkdoc-params: (args)
  "Ensure `helm-source-buffers' attributes are used."
  (let* ((current-command (or (helm-this-command) this-command))
         (str-command (if current-command
                          (helm-symbol-name current-command)
                        "completing-read"))
         (buf-name (format "*%s*" str-command)))
    (unwind-protect
        (progn
          (advice-add
           'helm
           :filter-args #'exordium--helm-swith-to-buffer-update-sources)
          (when-let* ((buffers (all-completions "" (cadr args)))
                      (result (cl-loop for b in buffers
                                       maximize (length b)
                                       into len-buf
                                       maximize (length
                                                 (helm-buffer--format-mode-name b))
                                       into len-mode
                                       finally return (cons len-buf len-mode))))
            (unless (default-value 'helm-buffer-max-length)
              (helm-set-local-variable 'helm-buffer-max-length (car result)))
            (unless (default-value 'helm-buffer-max-len-mode)
              (helm-set-local-variable 'helm-buffer-max-len-mode (cdr result))))

          (apply #'helm-completing-read-default-handler
                 (append args
                         (list str-command buf-name))))
      (advice-remove
       'helm #'exordium--helm-swith-to-buffer-update-sources))))

  (defun exordium--helm-occur-ensure-input (args)
    "Add `:input' equal to `:default' to ARGS plist if it's not already there."
    (if (or (not (eq 'helm-occur this-command))
            (not (plistp args))
            (plist-get args :input))
        args
      (plist-put args :input (plist-get args :default))))

  (defun exordium-helm-do-grep-ag-in-directory (&optional arg)
    "Like `helm-do-grep-ag', but ask for a directory first."
    (interactive "P")
    (let ((dir (helm-read-file-name
                "Search in directory: "
                :test #'file-directory-p
                :default default-directory
                :must-match t)))
      (helm-grep-ag (expand-file-name dir) arg)))

  :custom
  (history-delete-duplicates t)
  (helm-M-x-always-save-history t)
  (helm-M-x-show-short-doc t)
  (completions-detailed exordium-help-extensions)
  (helm-grep-ag-command exordium-helm-grep-ag-command)
  ;; Extract value of --color-match argument (if present) when
  ;; `exordium-helm-grep-ag-command' uses ag.
  (helm-grep-ag-pipe-cmd-switches
   (when (and
          (member
           (when-let* ((cmd (car
                             (cl-remove-if (lambda (str)
                                             (string-match
                                              (rx string-start
                                                  alpha
                                                  (zero-or-more alnum)
                                                  "=")
                                              str))
                                           (split-string
                                            exordium-helm-grep-ag-command)))))
             (file-name-nondirectory cmd))
           '("ag" "pt"))
          (string-match (rx (group
                             "--color-match"
                             (or "=" (one-or-more space))
                             (+? (not space)))
                            (or "\\" space string-end))
                        exordium-helm-grep-ag-command))
     (list (match-string 1 exordium-helm-grep-ag-command))))

  :bind
  (([remap execute-extended-command] . #'helm-M-x) ; M-x
   ([remap yank-pop] . #'helm-show-kill-ring) ; M-y
   ([remap find-file] . #'helm-find-files) ; C-x C-f
   ([remap find-file-read-only] . #'helm-recentf) ; C-x C-r
   ("C-S-d" . #'exordium-helm-do-grep-ag-in-directory)
   ("C-S-s" . #'helm-occur))

  :config
  ;; Do not show these files in helm buffer
  (dolist (pat (list (rx ".tsk" string-end)
                     (rx ".log.")))
    (add-to-list 'helm-boring-file-regexp-list pat))

  (require 'helm-mode)
  (dolist (fun '(switch-to-buffer
                 switch-to-buffer-other-frame
                 switch-to-buffer-other-tab
                 switch-to-buffer-other-window))
    (add-to-list 'helm-completing-read-handlers-alist
                 (cons fun #'exordium--helm-switch-to-buffer-completing-read)))

  (advice-add #'helm :filter-args #'exordium--helm-occur-ensure-input)

  (helm-mode))

(use-package wgrep-helm
  :defer t)

(use-package helm-descbinds
  :defer t
  :bind
  ("C-h b" . #'helm-descbinds))

(use-package helm-rg
  :defer t)

(use-package helm-rg
  :unless exordium-helm-projectile
  :defer t
  :bind
  ("C-S-r" . #'helm-rg))

(when exordium-helm-everywhere
  (use-package helm-xref
    :defer t))



(provide 'init-helm)

;;; init-helm.el ends here
