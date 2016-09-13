;;;; ycmd - autcomplete

(require 'ycmd)
;(add-hook 'after-init-hook #'global-ycmd-mode)
(add-hook 'c++-mode-hook 'ycmd-mode)


(set-variable 'ycmd-server-command '("python" "/home/abhijit/.emacs.d/taps/ycmd/ycmd_server"))
; (set-variable 'ycmd-global-config "/home/abhijit/.emacs/taps/ycmd/cpp/ycm/.ycm_extra_conf.py")

;; company-ycmd integration
(require 'company-ycmd)
(company-ycmd-setup)

;; flycheck integration
(require 'flycheck-ycmd)
(flycheck-ycmd-setup)

;; in terminal mode flycheck and company may interfere
(when (not (display-graphic-p))
  (setq flycheck-indication-mode nil))

;; eldoc integration
(require 'ycmd-eldoc)
(add-hook 'ycmd-mode-map 'ycmd-eldoc-setup)

;; next error integration



(provide 'init-emacs-ycmd)
