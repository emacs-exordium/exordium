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

;;; Code:

(eval-when-compile
  (unless (featurep 'init-require)
    (load (file-name-concat (locate-user-emacs-file "modules") "init-require"))))
(exordium-require 'init-prefs)

(use-package helm
  :diminish
  :custom
  (helm-split-window-default-side 'other)
  (helm-split-window-other-side-when-one-window 'right)
  (helm-buffer-details-flag nil)
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
  :custom
  (history-delete-duplicates t)
  (helm-M-x-always-save-history t)
  (helm-M-x-show-short-doc t)
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
  :after (helm)
  :bind
  (("C-h b" . #'helm-descbinds)))

(use-package helm-ag
  :after (helm)
  :custom
  (helm-ag-insert-at-point 'symbol)
  :bind
  (("C-S-d" . #'helm-do-ag)
   ("C-S-f" . #'helm-do-ag-this-file)))

(use-package helm-ag
  :after (helm)
  :unless exordium-helm-projectile
  :bind
  (("C-S-a" . #'helm-ag-project-root)))

(use-package helm-rg
  :after (helm)
  :defer t)

(use-package helm-rg
  :after (helm)
  :unless exordium-helm-projectile
  :bind
  (("C-S-r" . #'helm-rg)))

(use-package helm-swoop
  :after (helm)
  :commands (helm-swoop--edit-complete
             helm-swoop--edit-cancel
             helm-swoop--edit-delete-all-lines)
  :custom
  (helm-swoop-split-direction 'split-window-horizontally)
  :bind
  (("C-S-s" . #'helm-swoop)
   ;; Use similar bindings to `helm-ag-edit'
   :map helm-swoop-edit-map
   ("C-c C-c" . #'helm-swoop--edit-complete)
   ("C-c C-k" . #'helm-swoop--edit-cancel)
   ("C-c C-q C-k" . #'helm-swoop--edit-delete-all-lines)))

(use-package helm-xref
  :after helm
  :if exordium-helm-everywhere
  :commands helm-xref
  :config
  (setq xref-show-xrefs-function 'helm-xref-show-xrefs))



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
