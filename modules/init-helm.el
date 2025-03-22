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
;; C-S-f             Search with Ag: this file (like Swoop).
;; C-S-a             Search with Ag: in current project root.
;;                   See also`init-helm-porojectile.el'.
;; C-S-s             Helm Swoop
;; C-x c g           Helm Google suggest.

;;; Code:

(eval-when-compile
  (unless (featurep 'init-require)
    (load (file-name-concat (locate-user-emacs-file "modules") "init-require"))))
(exordium-require 'init-prefs)

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
              exordium--helm-switch-to-buffer-completing-read)
  :init
  (use-package helm-lib
    :ensure helm
    :defer t
    :autoload (helm-this-command
               helm-symbol-name
               helm-get-attr
               helm-set-attr
               helm-mklist))
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
    :autoload (helm-normalize-sources))

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
                           (append '(helm-skip-boring-buffers)
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

          (apply #'helm-completing-read-default-handler
                 (append args
                         (list str-command buf-name))))
      (advice-remove
       'helm #'exordium--helm-swith-to-buffer-update-sources))))

  :custom
  (history-delete-duplicates t)
  (helm-M-x-always-save-history t)
  (helm-M-x-show-short-doc t)
  (completions-detailed exordium-help-extensions)

  :bind
  (([remap execute-extended-command] . #'helm-M-x) ; M-x
   ([remap yank-pop] . #'helm-show-kill-ring) ; M-y
   ([remap find-file] . #'helm-find-files) ; C-x C-f
   ([remap find-file-read-only] . #'helm-recentf)) ; C-x C-r

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
  (helm-mode))

(use-package helm-descbinds
  :defer t
  :bind
  ("C-h b" . #'helm-descbinds))

(use-package helm-ag
  :defer t
  :custom
  (helm-ag-insert-at-point 'symbol)
  :bind
  (("C-S-d" . #'helm-do-ag)
   ("C-S-f" . #'helm-do-ag-this-file)))

(use-package helm-ag
  :defer t
  :unless exordium-helm-projectile
  :bind
  ("C-S-a" . #'helm-ag-project-root))

(use-package helm-rg
  :defer t)

(use-package helm-rg
  :unless exordium-helm-projectile
  :defer t
  :bind
  ("C-S-r" . #'helm-rg))

(use-package helm-swoop
  :defer t
  :init
  (use-package isearch
    :ensure nil
    :defer t
    :bind
    (:map isearch-mode-map
     ("C-S-s" . #'helm-swoop-from-isearch)))

  :commands (helm-swoop--edit-complete
             helm-swoop--edit-cancel
             helm-swoop--edit-delete-all-lines)
  :custom
  (helm-swoop-split-direction (pcase exordium-split-window-preferred-direction
                                ('longest #'split-window-sensibly)
                                ('horizontal #'split-window-horizontally)
                                (_ #'split-window-vertically)))
  :bind
  (("C-S-s" . #'helm-swoop)
   ;; Use similar bindings to `helm-ag-edit'
   :map helm-swoop-edit-map
   ("C-c C-c" . #'helm-swoop--edit-complete)
   ("C-c C-k" . #'helm-swoop--edit-cancel)
   ("C-c C-q C-k" . #'helm-swoop--edit-delete-all-lines)))

(when exordium-helm-everywhere
  (use-package helm-xref
    :defer t))



;; TODO: work in progress
;; The intent is to improve the readability of the helm swoop selection line
;; (in the helm buffer).

;; (defun fix-helm-swoop-colors (orig-fun &rest args)
;;   "Advice around `helm-swoop' to change the background of the
;; selected line in the hem buffer, for better readability"
;;   (let ((bg       (face-attribute 'helm-selection :background))
;;         (swoop-bg (face-attribute 'helm-swoop-target-line-face :background)))
;;     (set-face-attribute 'helm-selection nil :background swoop-bg)
;;     (let ((res (apply orig-fun args)))
;;       (set-face-attribute 'helm-selection nil :background bg)
;;       res)))

;; (advice-add 'helm-swoop :around #'fix-helm-swoop-colors)



(provide 'init-helm)

;;; init-helm.el ends here
