;;;; Custom Themes
;;;
;;; These config provides 3 themes:
;;; - Tomorrow, which comes in several declinasons (night, day, etc.)
;;; - Monokai (only one flavor)
;;; - Solarized, which comes with solarized-dark and solarized-light.
;;;
;;; Usage:
;;; New way: M-x load-theme <tab>  or (load-theme 'tomorrow-night t)
;;; Old way: M-x set-colors-XXX    or (set-colors-tomorrow-night)
;;;
;;; --------- ------------------------------------ ----------------------------
;;; Theme     Dark                                 Light
;;; --------- ------------------------------------ ----------------------------
;;; Tomorrow  `set-colors-tomorrow-night'          `set-colors-tomorrow-day'
;;;           `set-colors-tomorrow-night-bright'
;;;           `set-colors-tomorrow-night-eighties'
;;;           `set-colors-tomorrow-night-blue'
;;; --------- ------------------------------------ ----------------------------
;;; Monokai   `set-colors-monokai-default'
;;; --------- ------------------------------------ ----------------------------
;;; Solarized `set-colors-solarized-dark'          `set-colors-solarized-light'
;;; --------- ------------------------------------ ----------------------------

(require 'init-prefs)

(when *init-theme*
  (load-theme *init-theme* t))

(defun current-theme ()
  "Return the current theme, or nil if no custom theme is
enabled"
  (car custom-enabled-themes))


;;; Org mode extra statuses (WORK and WAIT)

(cond ((featurep 'color-theme-tomorrow)
       (when (fboundp 'set-tomorrow-extra-org-statuses)
         (set-tomorrow-extra-org-statuses)))
      ((featurep 'color-theme-monokai)
       (when (fboundp 'set-monokai-extra-org-statuses)
         (set-monokai-extra-org-statuses)))
      ((featurep 'color-theme-solarized)
       (when (fboundp 'set-solarized-extra-org-statuses)
         (set-solarized-extra-org-statuses))))

;;; Linum extension
(unless *init-git-gutter-non-fringe*
  (load "~/.emacs.d/themes/hilinum-mode.el")
  (require 'hlinum)
  (hlinum-activate))

;;; Colorize the name of the current project in the modeline.
(when (featurep 'init-helm-projectile)
  (eval-after-load "projectile"
    `(setq projectile-mode-line
           `(:eval (list " ["
                         (propertize
                          (projectile-project-name)
                          'face `(:foreground
                                  ,(cond ((featurep 'color-theme-monokai)
                                          (with-monokai-colors
                                           'default violet))
                                         ((featurep 'color-theme-tomorrow)
                                          (with-tomorrow-colors
                                           (tomorrow-mode-name) purple))
                                         ((featurep 'color-theme-solarized)
                                          (second (assoc 'violet solarized-colors)))
                                         (t ;; loud red
                                          "#ff0000"))))
                         "]")))))


;;; Utilities

;;; FIXME: why isn't this recognized as an interactive function?
(defun switch-theme (theme)
  "Prompt for a theme name with auto-complete and loads it"
  (interactive
   (list
    (intern (completing-read "Load theme: "
                             (mapcar 'symbol-name
                                     (custom-available-themes))))))
  (load-theme theme t nil)
  (powerline-reset))

(defun what-face (pos)
  "Display the face at point"
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))


(provide 'init-themes)
