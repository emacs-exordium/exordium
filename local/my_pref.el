;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file contains my own personal perferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; helm

(require 'helm-config)
(global-set-key (kbd "C-x r b") 'helm-bookmarks)  ;; bookmarks - set and jump
(global-set-key (kbd "C-x r x") 'helm-M-x)        ;; bookmarks - set and jump
(global-set-key (kbd "M-y") 'helm-show-kill-ring) ;; rotate previously copies


;; bring ycm config genrator function
(require 'ycm_generator)

(provide 'my_pref)
