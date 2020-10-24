;;;; Helm - see http://tuhdo.github.io/helm-intro.html
;;;
;;; ----------------- -------------------------------------------------------
;;; Key               Definition
;;; ----------------- -------------------------------------------------------
;;; M-x               Remap standard: Execute command with helm.
;;; M-y               Remap standard: Yank with helm.
;;; C-x b             Remap standard: Switch buffer with helm.
;;; C-x C-f           Remap standard: Find file with helm.
;;; C-S-r             Search with ripgrep: in current projectile project.
;;; C-S-d             Search with Ag: ask for directory first.
;;; C-S-f             Search with Ag: this file (like Swoop).
;;; C-S-a             Search with Ag: in current projectile project.

(use-package helm
  :custom
  (helm-split-window-default-side 'other))
(use-package helm-projectile)
(use-package helm-ag)
(use-package helm-rg)


(global-set-key (kbd "C-S-r")   'helm-projectile-rg)
(global-set-key (kbd "C-S-d")   'helm-do-ag)
(global-set-key (kbd "C-S-f")   'helm-do-ag-this-file)
(global-set-key (kbd "C-S-a")   'helm-projectile-ag)

(when exordium-helm-everywhere
  (helm-mode)
  (diminish 'helm-mode)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring))

(when exordium-helm-fuzzy-match
  (setq helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t
        helm-ff-fuzzy-matching t
        helm-ag-fuzzy-match t
        helm-buffer-details-flag nil
        helm-ag-insert-at-point 'symbol))


(provide 'init-helm)
