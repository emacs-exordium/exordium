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
  (:map company-active-map
        ;; Use ESC to escape company-complete (in addition to C-g)
        ("<escape>" . #'company-abort)
        ;; Key to force trigger company-complete
    :map global-map
        ("C-." . #'company-complete)))


(use-package company-statistics
  :ensure t
  :after company
  :init
  (company-statistics-mode))

(provide 'init-company)
