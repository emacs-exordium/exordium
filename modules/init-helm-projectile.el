;;;; Projectile - see http://batsov.com/projectile/
;;;
;;; ----------------- -------------------------------------------------------
;;; Key               Definition
;;; ----------------- -------------------------------------------------------
;;; C-c p p           [Opt remap] Select project and open file with helm.
;;; C-c p f           [Opt remap] Open file with helm-projectile (current project).
;;; C-c p s g         Grep in project.
;;; ... and many more under C-c p
;;; C-c h             Open file with helm-projectile (current project).
;;; C-c H             Same but first select the project.
;;; or C-c M-h
;;; C-c e             treemacs: toggle the directory tree.
;;; C-c E             Open treemacs with a projectile project.
;;; C-S-a             Search with Ag: in current projectile project. (see also`init-helm.el')
;;; C-S-r             Search with ripgrep: in current projectile project. (see also`init-helm.el')

(require 'init-prefs)

(use-package helm-rg)

(use-package projectile
  :bind
  (:map projectile-command-map
        ("." . helm-projectile-find-file-dwim))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode))

(use-package helm-projectile
  :bind
  (:map global-map
        ("C-c h"   . #'helm-projectile)
        ("C-c H"   . #'helm-projectile-switch-project)
        ("C-c M-h" . #'helm-projectile-switch-project)
        ("C-S-a"   . #'helm-projectile-ag)
        ("C-S-r"   . #'helm-projectile-rg))
  :config
  (when exordium-helm-everywhere
    (helm-projectile-on)))

(use-package treemacs-projectile
  :bind
  (:map global-map
        ("C-c e" . #'treemacs)
        ("C-c E" . #'treemacs-projectile)))

;; Prevent Projectile from indexing the build directory.
(when exordium-rtags-cmake-build-dir
  (let ((top-level (car (split-string exordium-rtags-cmake-build-dir "/"))))
    ;; By default, top-level = "cmake.bld" (excluding the "<arch>")
    (when top-level
      (setq projectile-globally-ignored-directories
            (cons top-level projectile-globally-ignored-directories)))))

(provide 'init-helm-projectile)
