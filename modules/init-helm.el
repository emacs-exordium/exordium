;;;; Helm - see http://tuhdo.github.io/helm-intro.html
;;;
;;; -------------- -------------------------------------------------------
;;; Key            Definition
;;; -------------- -------------------------------------------------------
;;; C-c h          Open anything
;;; M-h            Helm's version of M-x

(require 'helm)

(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key (kbd "M-h") 'helm-M-x)

(provide 'init-helm)
