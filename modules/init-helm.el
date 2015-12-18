;;;; Helm - see http://tuhdo.github.io/helm-intro.html
;;;
;;; ----------------- -------------------------------------------------------
;;; Key               Definition
;;; ----------------- -------------------------------------------------------
;;; M-x               Remap standard: Execute command with helm.
;;; M-y               Remap standard: Yank with helm.
;;; C-x b             Remap standard: Switch buffer with helm.
;;; C-x C-f           Remap standard: Find file with helm.
;;; C-S-r             Search with Ag: from project root.
;;; C-S-d             Search with Ag: ask for directory first.
;;; C-S-f             Search with Ag: this file (like Swoop).
;;; C-S-a             Search with Ag: in current projectile project.

(require 'helm)
(require 'helm-projectile)
(require 'helm-ag)


(global-set-key (kbd "C-S-r") 'helm-ag-project-root)
(global-set-key (kbd "C-S-d") 'helm-do-ag)
(global-set-key (kbd "C-S-f") 'helm-do-ag-this-file)
(global-set-key (kbd "C-S-a") 'helm-projectile-ag)

(when exordium-helm-everywhere
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "C-x b") 'helm-mini))

(when exordium-helm-fuzzy-match
  (setq helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t
        helm-ff-fuzzy-matching t
        helm-ag-fuzzy-match t
        helm-buffer-details-flag nil
        helm-ag-insert-at-point 'symbol))


(provide 'init-helm)
