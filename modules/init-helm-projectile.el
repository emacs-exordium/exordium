;;;; Helm - see http://tuhdo.github.io/helm-intro.html
;;;; Projectile - see http://batsov.com/projectile/
;;;
;;; ----------------- -------------------------------------------------------
;;; Key               Definition
;;; ----------------- -------------------------------------------------------
;;; C-c h OR C-c C-f  Open file with helm/projectile (current project).
;;; C-c C-h           Same but first select the project.
;;; C-c p s g         Grep in project.
;;;
;;; C-h b             Describe keybindings using Helm.
;;; C-S-s             Helm Swoop, a way of searching with Helm (try it!).
;;; C-c e             Project Explorer: show the directory tree.

(require 'helm)
(require 'projectile)
(require 'helm-projectile)
(require 'helm-swoop)
(require 'project-explorer)

(projectile-global-mode)
(global-set-key [(control c)(h)] (function helm-projectile))
(global-set-key [(control c)(control f)] (function helm-projectile))
(global-set-key [(control c)(control h)] (function helm-projectile-switch-project))


;;; Do not show these files in helm buffer
(add-to-list 'helm-boring-file-regexp-list "\\.tsk$")
(add-to-list 'helm-boring-file-regexp-list "\\.log\\.")

;; Use `recentf-list' instead of `file-name-history' in `helm-find-files'.
;;(setq helm-ff-file-name-history-use-recentf t)


;;; Other usages of Helm:

;;; C-h b = describe keybindings using Helm
(define-key global-map [(control h)(b)] (function helm-descbinds))

;;; C-S-s = helm-swoop
(define-key global-map [(control shift s)] (function helm-swoop))

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

;;; C-e = Project explorer
(define-key global-map [(control c)(e)] (function project-explorer-open))


(provide 'init-helm-projectile)
