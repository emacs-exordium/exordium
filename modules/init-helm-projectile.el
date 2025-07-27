;;; init-helm-projectile.el --- Setup helm with projectile -*- lexical-binding: t -*-

;;; Commentary:
;;
;; ----------------- -------------------------------------------------------
;; Key               Definition
;; ----------------- -------------------------------------------------------
;; C-c h             Open file with helm-projectile (current project).
;; C-c H             Same but first select the project.
;; or C-c M-h
;; C-c e             treemacs: toggle the directory tree.
;; C-c E             Open treemacs with a projectile project.
;; C-S-a             Search with Ag: in current projectile project.
;;                   See also`init-helm.el'.
;; C-S-r             Search with ripgrep: in current projectile project.
;;                   See also`init-helm.el'.

;;; Code:

(eval-when-compile
  (unless (featurep 'init-require)
    (load (file-name-concat (locate-user-emacs-file "modules") "init-require"))))
(exordium-require 'init-prefs)
(exordium-require 'init-helm)
(exordium-require 'init-projectile)

(use-package helm-projectile
  :demand t
  :init
  :bind
  (("C-c h"   . #'helm-projectile)
   ("C-c H"   . #'helm-projectile-switch-project)
   ("C-c M-h" . #'helm-projectile-switch-project)
   ("C-S-a"   . #'helm-projectile-ag)
   ("C-S-r"   . #'helm-projectile-rg))

  :config
  (when exordium-helm-everywhere
    (helm-projectile-on)))

(use-package treemacs-projectile
  :defer t
  :bind
  (("C-c e" . #'treemacs)
   ("C-c E" . #'treemacs-projectile)))

(provide 'init-helm-projectile)

;;; init-helm-projectile.el ends here
