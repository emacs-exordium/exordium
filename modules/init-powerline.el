;;; init-powerline.el --- A theme for Powerline.     -*- lexical-binding: t -*-

;;; Commentary:
;;
;;; Code:

(eval-when-compile
  (unless (featurep 'init-require)
    (load (file-name-concat (locate-user-emacs-file "modules") "init-require"))))
(exordium-require 'init-prefs)

(use-package powerline
  :autoload (powerline-set-selected-window))

(use-package all-the-icons
  :autoload (all-the-icons-octicon
             all-the-icons-octicon-family))

;;; Faces for our powerline theme. They are defined here and customized within
;;; each theme.

(defface exordium-powerline-active1 '((t (:inherit mode-line)))
  "Powerline active face 1."
  :group 'exordium)

(defface exordium-powerline-active2 '((t (:inherit mode-line)))
  "Powerline active face 1."
  :group 'exordium)

(defface exordium-powerline-active3 '((t (:inherit mode-line)))
  "Powerline active face 3 (buffer name)."
  :group 'exordium)

(defface exordium-powerline-inactive1 '((t (:inherit mode-line-inactive)))
  "Powerline inactive face 1."
  :group 'exordium)

(defface exordium-powerline-inactive2 '((t (:inherit mode-line-inactive)))
  "Powerline inactive face 2."
  :group 'exordium)

(defface exordium-powerline-inactive3 '((t (:inherit mode-line-inactive)))
  "Powerline inactive face 3 (buffer name)."
  :group 'exordium)

;;; Our Powerline theme.

(defun exordium-powerline-buffer-face (active)
  "Return the face to use for the buffer name depending on whether it is ACTIVE."
  (cond ((not active)
         ;; Gray
         'exordium-powerline-inactive3)
        (t
         ;; Purple
         'exordium-powerline-active3)))

(defun exordium-powerline-git (face)
  "Return the git branch string to display using the specified FACE.
This includes a branch icon."
  (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
    (concat
     ;; branch icon
     (propertize (format " %s" (all-the-icons-octicon "git-branch"))
                 'face `(:height 0.9
                         :family ,(all-the-icons-octicon-family)
                         :background ,(face-background face)))
     ;; branch name
     (format " %s" branch))))

(defun exordium-powerline-svn (face)
  "Return the SVN revision number string to display using the specified FACE.
This includes a branch icon."
  (let ((revision (cadr (split-string vc-mode "-"))))
    (concat
     ;; branch icon
     (propertize (format " %s" (all-the-icons-octicon "git-branch"))
                 'face `(:height 0.9
                         :family ,(all-the-icons-octicon-family)
                         :background ,(face-background face)))
     ;; revision number
     (format " %s" revision))))

(defun exordium-powerline-vc (face)
  "Return the version control string to display using the specified FACE.
Return nil if the current buffer is not under version control"
  (when vc-mode
    (cond
     ((string-match "Git[:-]" vc-mode)
      (exordium-powerline-git face))
     ((string-match "SVN-" vc-mode)
      (exordium-powerline-svn face))
     (t
      ;; fallback
      (format "%s" vc-mode)))))

(defun exordium-powerline-theme ()
  "Setup the default mode-line."
  (interactive)
  (when (eq exordium-powerline-theme :wave)
    (setq-default powerline-default-separator 'wave))
  (setq-default
   mode-line-format
   '("%e"
     (:eval
      (let* ((active (powerline-selected-window-active))
             (mode-line (if active 'mode-line 'mode-line-inactive))
             (face1 (if active 'exordium-powerline-active1 'exordium-powerline-inactive1))
             (face2 (if active 'exordium-powerline-active2 'exordium-powerline-inactive2))
             (face3 (if active 'exordium-powerline-active3 'exordium-powerline-inactive3))
             (separator-left (intern
                              (format "powerline-%s-%s"
                                      powerline-default-separator
                                      (car powerline-default-separator-dir))))
             (separator-right (intern
                               (format "powerline-%s-%s"
                                       powerline-default-separator
                                       (cdr powerline-default-separator-dir))))
             (lhs (list (powerline-raw "%*" face3 'l)
                        (powerline-raw "%b " face3 'l)
                        (when (and (boundp 'which-func-mode) which-func-mode)
                          (powerline-raw which-func-format face3 'l))
                        (funcall (if (eq exordium-powerline-theme :wave)
                                     separator-right
                                   separator-left)
                                 face3 mode-line)
                        ;;(funcall separator-left face3 face2)
                        (when (boundp 'erc-modified-channels-object)
                          (powerline-raw erc-modified-channels-object face1 'l))
                        (powerline-major-mode face1 'l)
                        (powerline-process face1)
                        (powerline-minor-modes face1 'l)
                        (powerline-narrow face1 'l)
                        (powerline-raw " " face1)
                        (funcall separator-left face1 face2)
                        (if (and exordium-powerline-enable-icons window-system)
                            (powerline-raw (exordium-powerline-vc face2) face2)
                          (powerline-vc face2 'r))))
             (rhs (list (powerline-raw global-mode-string face2 'r)
                        (funcall separator-right face2 face1)
                        (powerline-raw "%4l" face1 'l)
                        (powerline-raw ":" face1 'l)
                        (powerline-raw "%3c" face1 'r)
                        (funcall separator-right face1 mode-line)
                        (powerline-raw " ")
                        (powerline-raw "%6p" nil 'r))))
        (concat (powerline-render lhs)
                (powerline-fill face2 (powerline-width rhs))
                (powerline-render rhs)))))))

(defun display-powerline ()
  "(Re)Displays Powerline."
  (powerline-set-selected-window)
  (exordium-powerline-theme)
  (redraw-display))

;; Depending on the preferences, either display powerline immediately or after
;; a number of seconds of idle time. This is a fix for Emacs crashing on some
;; environments.
(if (eq exordium-display-powerline-after-idle-time 0)
    (exordium-powerline-theme)
  (run-with-idle-timer exordium-display-powerline-after-idle-time nil
                       #'display-powerline))

(provide 'init-powerline)

;;; init-powerline.el ends here
