;;;; Helm - see http://tuhdo.github.io/helm-intro.html
;;;
;;; -------------- -------------------------------------------------------
;;; Key            Definition
;;; -------------- -------------------------------------------------------
;;; C-c h          Open anything
;;; M-h            Helm's version of M-x

(require 'helm)

;;; Do not show these files in helm buffer
;; (add-to-list helm-boring-file-regexp-list
;;       '("\\.tsk$" "\\.log$"))

;; Use `recentf-list' instead of `file-name-history' in `helm-find-files'.
;;(setq helm-ff-file-name-history-use-recentf t)

(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key (kbd "C-c m") 'helm-M-x)

(provide 'init-helm)
