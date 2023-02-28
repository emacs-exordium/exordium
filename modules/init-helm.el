;;;; Helm - see http://tuhdo.github.io/helm-intro.html
;;;
;;; ----------------- -------------------------------------------------------
;;; Key               Definition
;;; ----------------- -------------------------------------------------------
;;; M-x               Remap standard: Execute command with helm.
;;; M-y               Remap standard: Yank with helm.
;;; C-x b             Remap standard: Switch buffer with helm.
;;; C-x C-f           Remap standard: Find file with helm.
;;; C-x C-r           Open recent file with Helm (see also `init-ido.el').
;;; C-h b             Describe keybindings using Helm.
;;; C-S-r             Search with ripgrep: in current project root. (see also`init-helm-porojectile.el')
;;; C-S-d             Search with Ag: ask for directory first.
;;; C-S-f             Search with Ag: this file (like Swoop).
;;; C-S-a             Search with Ag: in current project root. (see also`init-helm-porojectile.el')
;;; C-S-s             Helm Swoop

(require 'init-prefs)

(use-package helm
  :diminish
  :custom
  (helm-split-window-default-side 'other)
  (helm-buffer-details-flag nil)

  :config
  (when exordium-helm-fuzzy-match
    ;; following advice from `helm-completion-style' doc
    (let ((style (or
                  (car (assq 'flex completion-styles-alist))
                  (car (assq 'helm-flex completion-styles-alist)))))
      (if style
          (add-to-list 'completion-styles style)
        (customize-set-variable 'helm-completion-style 'helm-fuzzy)))))

(use-package helm
  :diminish
  :when exordium-helm-everywhere
  :custom
  (history-delete-duplicates t)
  (helm-M-x-always-save-history t)
  :bind
  (([remap execute-extended-command] . #'helm-M-x) ; M-x
   ([remap yank-pop] . #'helm-show-kill-ring) ; M-y
   ([remap find-file] . #'helm-find-files) ; C-x C-f
   ([remap find-file-read-only] . #'helm-recentf)) ; C-x C-r
  :config
  ;; Do not show these files in helm buffer
  (add-to-list 'helm-boring-file-regexp-list "\\.tsk$")
  (add-to-list 'helm-boring-file-regexp-list "\\.log\\.")
  (helm-mode))

(use-package helm-descbinds
  :bind
  (("C-h b" . #'helm-descbinds)))

(use-package helm-ag
  :custom
  (helm-ag-insert-at-point 'symbol)
  :bind
  (("C-S-d" . #'helm-do-ag)
   ("C-S-f" . #'helm-do-ag-this-file)))

(use-package helm-ag
  :unless exordium-helm-projectile
  :bind
  (("C-S-a" . #'helm-ag-project-root)))

(use-package helm-rg
  :unless exordium-helm-projectile
  :bind
  (("C-S-r" . #'helm-rg)))

(use-package helm-swoop
  :custom
  (helm-swoop-split-direction 'split-window-horizontally)
  :bind
  (("C-S-s" . #'helm-swoop)
   ;; Use similar bindings to `helm-ag-edit'
   :map helm-swoop-edit-map
        ("C-c C-c" . #'helm-swoop--edit-complete)
        ("C-c C-k" . #'helm-swoop--edit-cancel)
        ("C-c C-q C-k" . #'helm-swoop--edit-delete-all-lines)))




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
