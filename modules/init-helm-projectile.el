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
              exordium-helm-projectile--exit-helm-and-do-rg
              exordium-helm-projectile--make-source-with)
  :commands (helm-projectile-switch-project)
  :init
  (defun exordium-projectile-switch-project-find-file-other-window ()
    "Switch to a project we have visited before then jump to a
project's file using completion and show it in another window."
    (interactive)
    (let ((projectile-switch-project-action #'projectile-find-file-other-window))
      (helm-projectile-switch-project)))

  (use-package projectile
    :defer t
    :autoload (projectile-switch-project-by-name)
    :bind
    (:map projectile-command-map
     ("p" . #'helm-projectile-switch-project)
     ("4 p" . #'exordium-projectile-switch-project-find-file-other-window)))

  (defun exordium-helm-projectile--make-source-with (action)
    "Make a new `helm-source-projectile-buffer' with FUN as the first action.
ACTION is a cons in a form of (DESCRIPTION . FUNCTION)."
    (let ((source (helm-make-source "Project buffers"
                     'helm-source-projectile-buffer)))
     (helm-set-attr
      'action
      (cons
       action
       (cl-remove-if (lambda (act)
                       (eq (cdr act)
                           (cdr action)))
                     (pcase (helm-get-attr 'action  source)
                       ((and (pred symbolp) act) (eval act))
                       ((and (pred listp) act) act)
                       (act (list act)))))
      source)
     source))

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
  ;; The following lambda is constructed similarly to what
  ;; `helm-completing-read-default-1' does.  However, it's calculated once on
  ;; startup (when `helm-projectile' package is configured) and not on each
  ;; `helm-projectile-switch-to-buffer'/`helm-projectile' call (which is how
  ;; original behaves).  This has been done in an attempt to making this
  ;; solution slightly simpler as we need to modify a helm-source prototype
  ;; which would be cumbersome to do dynamically.  Caveat is that it doesn't
  ;; let user to use their desired annotation or affixation function via
  ;; `completion-extra-properties' when `completions-detailed' is non-nil.
  (when exordium-help-extensions
    (let* ((metadata (completion-metadata "" 'internal-complete-buffer nil))
           (category (completion-metadata-get metadata 'category))
           (sort-fn (unless (eq helm-completion-style 'helm-fuzzy)
                      (or
                       (completion-metadata-get
                        metadata 'display-sort-function)
                       (lambda (candidates)
                         (sort candidates #'helm-generic-sort-fn)))))
           (metadata (assoc-default category helm-completing-read-extra-metadata))
           (afun (completion-metadata-get metadata 'annotation-function))
           (afix (completion-metadata-get metadata 'affixation-function)))
      (when (or afun afix)
        (helm-set-attr
         'filtered-candidate-transformer
         (list
          'helm-skip-boring-buffers
          (lambda (candidates _source)
            (helm-completion--decorate
             (if (and sort-fn (> (length helm-pattern) 0))
                 (funcall sort-fn candidates)
               candidates)
             afun afix category)))
         helm-source-projectile-buffers-list))))

  (unless (fboundp 'helm-projectile-switch-to-buffer-other-window)
    (declare-function helm-projectile-switch-to-buffer-other-window nil)
    (helm-projectile-command
     "switch-to-buffer-other-window"
     (exordium-helm-projectile--make-source-with
      (cons "Switch to buffer(s) other window"
            #'helm-buffer-switch-buffers-other-window))
     "Switch to buffer: "
     nil helm-buffers-truncate-lines))

  (unless (fboundp 'helm-projectile-switch-to-buffer-other-frame)
    (declare-function helm-projectile-switch-to-buffer-other-frame nil)
    (helm-projectile-command
     "switch-to-buffer-other-frame"
     (exordium-helm-projectile--make-source-with
      (cons "Switch to buffer(s) other frame"
            #'helm-buffer-switch-to-buffer-other-frame))
     "Switch to buffer: "
     nil helm-buffers-truncate-lines))

  (when exordium-helm-everywhere
    (helm-projectile-on)
    (bind-keys :map projectile-mode-map
               ([remap projectile-switch-to-buffer-other-window]
                . helm-projectile-switch-to-buffer-other-window)
               ([remap projectile-switch-to-buffer-other-frame]
                . helm-projectile-switch-to-buffer-other-frame))))

(use-package treemacs-projectile
  :defer t
  :bind
  (("C-c e" . #'treemacs)
   ("C-c E" . #'treemacs-projectile)))

(provide 'init-helm-projectile)

;;; init-helm-projectile.el ends here
