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
(eval-when-compile
  (require 'fill-column-indicator)
  (require 'hilinum-mode)
  (require 'powerline))

(when exordium-theme
  (load-theme exordium-theme t))

(defun current-theme ()
  "Return the current theme, or nil if no custom theme is enabled"
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
      ((featurep 'color-theme-material)
       (when (fboundp 'set-material-extra-org-statuses)
         (set-material-extra-org-statuses))))

;;; linum extension: highlight the current line number.
;;; TODO: this does not work with `nlinum' at this time.
;;; See http://stackoverflow.com/questions/25411108/how-to-highlight-the-current-line-number-in-nlinum-mode

(when (and exordium-highlight-linum
           (or (eq exordium-display-line-numbers t)
               (eq exordium-display-line-numbers :linum))
           (not exordium-git-gutter-non-fringe))
  ;;(load "hilinum-mode.el")
  (require 'hilinum-mode)
  (hlinum-activate))

;;; FCI (80-column marker) color

(when exordium-fci-mode
  (require 'fill-column-indicator)
  (let ((color (and (facep 'vertical-border)
                    (face-foreground 'vertical-border))))
    (setq fci-rule-color (or color "dim gray"))))


;;; Utilities

(defun switch-theme (theme)
  "Prompt for a theme name with auto-complete and loads it.
Use this function rather than `load-theme' because it makes
Powerline follow."
  (interactive
   (list
    (intern (completing-read "Load theme: "
                             (mapcar 'symbol-name
                                     (custom-available-themes))))))
  (load-theme theme t nil)
  (setq exordium-theme theme)
  (powerline-reset))

(defun what-face (pos)
  "Display the face at point"
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

;; Emacs daemon sometimes displays a some faces incorrectly: it seems like it
;; does not load all the theme colors/fonts when no frame is present during
;; loading time (which is the case when starting in daemon mode). Here we force
;; a reload of the theme when a new frame is created with Emacs client to work
;; around that bug:

(defvar exordium-theme-loaded-in-frame nil
  "Whether `reloa-current-theme-in-frame' was called")

(defun reload-current-theme-in-frame (frame)
  "Reload the current theme in FRAME"
  (unless exordium-theme-loaded-in-frame
    (select-frame frame)
    (load-theme exordium-theme t nil)
    (setq exordium-theme-loaded-in-frame t)))

(when (daemonp)
  (add-hook 'after-make-frame-functions #'reload-current-theme-in-frame))


(provide 'init-themes)
