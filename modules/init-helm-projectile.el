;;;; Helm - see http://tuhdo.github.io/helm-intro.html
;;;; Projectile - see http://batsov.com/projectile/
;;;
;;; -------------- -------------------------------------------------------
;;; Key            Definition
;;; -------------- -------------------------------------------------------
;;; C-c h          Open file with helm/projectile
;;; C-c p f        Open file with ido/projectile

(require 'helm)
(require 'projectile)
(require 'helm-projectile)

(projectile-global-mode)
(global-set-key [(control c)(h)] 'helm-projectile)

(global-set-key [(control c)(p)(g)] 'projectile-grep)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Do not show these files in helm buffer
;; (add-to-list helm-boring-file-regexp-list
;;       '("\\.tsk$" "\\.log$"))

;; Use `recentf-list' instead of `file-name-history' in `helm-find-files'.
;;(setq helm-ff-file-name-history-use-recentf t)

;;(global-set-key (kbd "C-c h") 'helm-mini)
;;(global-set-key (kbd "C-c m") 'helm-M-x)

(provide 'init-helm-projectile)
