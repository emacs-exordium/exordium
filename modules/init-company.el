;;;; company-mode

(use-package company
  :diminish "CA"
  :after (rtags)
  :defer
  :config
  (setq rtags-completions-enabled t)
  ;; Turn on company mode everywhere
  (global-company-mode)
  (add-to-list 'company-backends '(company-capf company-dabbrev))
  (setq company-idle-delay nil)
  :bind
  (("C-." . #'company-complete)
   :map company-active-map
        ("<escape>" . #'company-abort)))


(use-package company-statistics
  :ensure t
  :after company
  :init
  (company-statistics-mode))

(provide 'init-company)
