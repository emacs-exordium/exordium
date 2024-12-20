;;; init-ruby.el --- Configuration for Ruby          -*- lexical-binding: t -*-

;;; Commentary:
;;
;; See https://github.com/zenspider/enhanced-ruby-mode.

;;; Code:

(use-package enh-ruby-mode
  :defer t
  :mode ("\\.rb'" . enh-ruby-mode)
  :interpreter ("ruby" . enh-ruby-mode))

(provide 'init-ruby)

;;; init-ruby.el ends here
