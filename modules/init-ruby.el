;;;; Configuration for Ruby
;;;; https://github.com/zenspider/enhanced-ruby-mode

(use-package enh-ruby-mode
  :ensure t
  :defer t
  :mode ("\\.rb'" . enh-ruby-mode)
  :interpreter ("ruby" . enh-ruby-mode))

(provide 'init-ruby)
