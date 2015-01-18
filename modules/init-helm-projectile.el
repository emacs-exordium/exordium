;;;; Helm - see http://tuhdo.github.io/helm-intro.html
;;;; Projectile - see http://batsov.com/projectile/
;;;
;;; -------------- -------------------------------------------------------
;;; Key            Definition
;;; -------------- -------------------------------------------------------
;;; C-c h          Open file with helm/projectile (current project)
;;; C-c C-h        Same but first select the project
;;; C-c p s g      Grep in project

(require 'helm)
(require 'projectile)
(require 'helm-projectile)

(projectile-global-mode)
(global-set-key [(control c)(h)] 'helm-projectile)
(global-set-key [(control c)(control h)] 'helm-projectile-switch-project)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Do not show these files in helm buffer
(add-to-list 'helm-boring-file-regexp-list "\\.tsk$")
(add-to-list 'helm-boring-file-regexp-list "\\.log\\.")

;; Use `recentf-list' instead of `file-name-history' in `helm-find-files'.
;;(setq helm-ff-file-name-history-use-recentf t)

(provide 'init-helm-projectile)
