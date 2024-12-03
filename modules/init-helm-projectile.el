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
  :functions (exordium-projectile-switch-project-find-file-other-window
              exordium-helm-projectile--switch-project-and-do-rg
              exordium-helm-projectile--exit-helm-and-do-ag
              exordium-helm-projectile--exit-helm-and-do-rg)
  :commands (helm-projectile-switch-project)
  :init
  (defun exordium-projectile-switch-project-find-file-other-window ()
    "Switch to a project we have visited before then jump to a
project's file using completion and show it in another window."
    (interactive)
    (let ((projectile-switch-project-action #'projectile-find-file-other-window))
      (projectile-switch-project)))

  (use-package projectile
    :defer t
    :autoload (projectile-switch-project-by-name)
    :bind
    (:map projectile-command-map
     ("p" . #'helm-projectile-switch-project)
     ("4 p" . #'exordium-projectile-switch-project-find-file-other-window)))

  (defun exordium-helm-projectile--exit-helm-and-do-ag ()
    "Exit helm and run ag on first selected candidate."
    (interactive)
    (if-let* ((project (car (helm-marked-candidates))))
        (helm-run-after-exit #'helm-do-ag
                             project)
      (error "No candidates selected")))

  (defun exordium-helm-projectile--switch-project-and-do-rg (project)
    "Switch projct to PROJECT and run ripgrep there."
    (interactive)
    (let ((projectile-switch-project-action #'helm-projectile-rg))
      (projectile-switch-project-by-name project)))

  (defun exordium-helm-projectile--exit-helm-and-do-rg ()
    "Exit helm and switch project to first selected candidate and run rg there."
    (interactive)
    (if-let* ((project (car (helm-marked-candidates))))
        (helm-run-after-exit #'exordium-helm-projectile--switch-project-and-do-rg
                             project)
      (error "No candidates selected")))

  :bind
  (("C-c h"   . #'helm-projectile)
   ("C-c H"   . #'helm-projectile-switch-project)
   ("C-c M-h" . #'helm-projectile-switch-project)
   ("C-S-a"   . #'helm-projectile-ag)
   ("C-S-r"   . #'helm-projectile-rg)
   :map helm-projectile-projects-map
   ("C-S-a" . #'exordium-helm-projectile--exit-helm-and-do-ag)
   ("C-S-r" . #'exordium-helm-projectile--exit-helm-and-do-rg))

  :config
  (helm-add-action-to-source "Silver Searcher (ag) in project `C-S-a'"
                             #'helm-do-ag
                             helm-source-projectile-projects)

  (helm-add-action-to-source "ripgrep (rg) in project `C-S-r'"
                             #'exordium-helm-projectile--switch-project-and-do-rg
                             helm-source-projectile-projects)
  (when exordium-helm-everywhere
    (helm-projectile-on)))

(use-package treemacs-projectile
  :bind
  (("C-c e" . #'treemacs)
   ("C-c E" . #'treemacs-projectile)))

(provide 'init-helm-projectile)

;;; init-helm-projectile.el ends here
