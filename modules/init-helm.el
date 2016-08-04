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
(require 'init-lib)


(exordium-define-key global-map 'helm-ag-project-root)
(exordium-define-key global-map 'helm-do-ag)
(exordium-define-key global-map 'helm-do-ag-this-file)
(exordium-define-key global-map 'helm-projectile-ag)

(when exordium-helm-everywhere
  (exordium-define-key global-map 'helm-M-x)
  (exordium-define-key global-map 'helm-find-files)
  (exordium-define-key global-map 'helm-show-kill-ring)
  (exordium-define-key global-map 'helm-mini))

(when exordium-helm-fuzzy-match
  (setq helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t
        helm-ff-fuzzy-matching t
        helm-ag-fuzzy-match t
        helm-buffer-details-flag nil
        helm-ag-insert-at-point 'symbol))


(provide 'init-helm)
