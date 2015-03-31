;;;; Custom Themes
;;;
;;; These config provides several themes:
;;; - Tomorrow, which comes in several flavors:
;;;   tomorrow-day,
;;;   tomorrow-night,
;;;   tomorrow-night-bright,
;;;   tomorrow-night-blue,
;;;   tomorrow-night-eighties.
;;; - Monokai.
;;; - Solarized-dark and solarized-light.
;;; - Zenburn.
;;; - Material.
;;; The default theme is tomorrow-night.
;;;
;;; Usage:
;;; Edit ~/.emacs.d/prefs.el and set your favorite theme like so:
;;;   (setq exordium-theme 'zenburn)
;;;
;;; At runtime, change the theme with: M-x load-theme <tab>
;;; or evaluate something like: (load-theme 'tomorrow-night t)

(require 'init-prefs)

(when exordium-theme
  (load-theme exordium-theme t))

(defun current-theme ()
  "Return the current theme, or nil if no custom theme is
enabled"
  (car custom-enabled-themes))


;;; Theme additions

(cond ((featurep 'color-theme-tomorrow)
       (when (fboundp 'set-tomorrow-extra-org-statuses)
         (set-tomorrow-extra-org-statuses)))
      ((featurep 'color-theme-monokai)
       (when (fboundp 'set-monokai-extra-org-statuses)
         (set-monokai-extra-org-statuses)))
      ((featurep 'color-theme-solarized)
       (when (fboundp 'set-solarized-extra-org-statuses)
         (set-solarized-extra-org-statuses)))
      ((featurep 'color-theme-zenburn)
       (when (fboundp 'set-zenburn-extra-org-statuses)
         (set-zenburn-extra-org-statuses)))
      ((featurep 'material)
       (when (fboundp 'set-material-extra-org-statuses)
         (set-material-extra-org-statuses))))

;;; linum extension
(when (and exordium-highlight-linum
           (not exordium-git-gutter-non-fringe))
  (load "hilinum-mode.el")
  (require 'hlinum)
  (hlinum-activate))

;;; Colorize the name of the current project in the modeline.
(defface exordium-project-name '((t (:inherit mode-line)))
  "Face for the name of the current project in the modeline"
  :group 'exordium)

(when (featurep 'init-helm-projectile)
  (eval-after-load "projectile"
    `(setq projectile-mode-line
           `(:eval (list " ["
                         (propertize (projectile-project-name)
                                     'face 'exordium-project-name)
                         "]")))))

;;; FCI color
(eval-when-compile
  (require 'fill-column-indicator))

(when exordium-fci-mode
  (require 'fill-column-indicator)
  (let ((color (and (facep 'vertical-border)
                    (face-foreground 'vertical-border))))
    (setq fci-rule-color (or color "dim gray"))))


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
