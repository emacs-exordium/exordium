;;; init-docker.el --- Configuration of Docker related features -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package docker
  :bind ("C-c D" . #'docker))

(use-package dockerfile-mode
  :mode "Dockerfile\\'"
  :custom
  (docker-use-sudo nil))

(when (version< emacs-version "29")
  (use-package docker-tramp
    :after docker
    :defer t))

(provide 'init-docker)

;;; init-docker.el ends here
