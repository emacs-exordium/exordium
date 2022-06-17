;;;; Configuration of Docker related features

(use-package docker
  :bind ("C-c d" . docker))

(use-package dockerfile-mode
  :mode
  ("Dockerfile\\'" . dockerfile-mode)
  :config
  (setq-default docker-use-sudo nil))

(use-package docker-tramp)

(provide 'init-docker)
