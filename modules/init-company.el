;;;; company-mode

(use-package company
  :diminish "CA"
  :after (rtags)
  :defer
  :init
  ;; Turn on company mode everywhere
  (global-company-mode)
  :config
  (setq rtags-completions-enabled t)
  ;; Use ESC to escape company-complete (in addition to C-g)
  (define-key company-active-map (kbd "<escape>") 'company-abort)

  ;; Key to force trigger company-complete
  (define-key company-mode-map [(control .)] 'company-complete))




;; ;; Clean up mode-line.
;; (eval-after-load "company"
;;   '(diminish 'company-mode "CA"))

(provide 'init-company)
