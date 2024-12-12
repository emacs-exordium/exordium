;;; init-projectile.el --- Projectile - see http://batsov.com/projectile/ -*- lexical-binding: t -*-

;;; Commentary:
;;
;; ----------------- -------------------------------------------------------
;; Key               Definition
;; ----------------- -------------------------------------------------------
;; C-c p p           [Opt remap] Select project and open file with helm.
;; C-c p f           [Opt remap] Open file with helm-projectile (current project).
;; C-c p s g         Grep in project.
;; ... and many more under C-c p

;;; Code:

(eval-when-compile
  (unless (featurep 'init-require)
    (load (file-name-concat (locate-user-emacs-file "modules") "init-require"))))
(exordium-require 'init-prefs)

(use-package projectile
  :diminish
  :bind
  (:map projectile-command-map
   ("." . #'helm-projectile-find-file-dwim))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode)
  (add-to-list 'projectile-globally-ignored-directories "cmake.bld")
  (add-to-list 'projectile-globally-ignored-directories "build")
  (add-to-list 'projectile-globally-ignored-directories "bld")
  (add-to-list 'projectile-globally-ignored-directories "cmake-build")

  ;; TODO: The following feature seems to has changed and a new solution needs to
  ;; be developed.  Probably involving setting up `projectile-mode-line-function'
  ;; to something based on `projectile-default-mode-line'.
  ;; ;; Colorize the name of the current project in the modeline.
  ;; (defface exordium-project-name '((t (:inherit mode-line)))
  ;;   "Face for the name of the current project in the modeline."
  ;;   :group 'exordium)
  ;; (setq projectile-mode-line
  ;;       `(:eval (if (file-remote-p default-directory)
  ;;                   (list " ["
  ;;                         (propertize "*remote*"
  ;;                                     'face 'exordium-project-name)
  ;;                         "]")
  ;;                 (list " ["
  ;;                       (propertize (projectile-project-name)
  ;;                                   'face 'exordium-project-name)
  ;;                       "]"))))
  )

(provide 'init-projectile)

;;; init-projectile.el ends here
