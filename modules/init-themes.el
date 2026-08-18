;;; init-themes.el --- Custom Themes.                -*- lexical-binding: t -*-

;;; Commentary:
;;
;;
;; These config provides several themes:
;; - Tomorrow, which comes in several flavors:
;;   tomorrow-day,
;;   tomorrow-night,
;;   tomorrow-night-bright,
;;   tomorrow-night-blue,
;;   tomorrow-night-eighties.
;; - Monokai.
;; - Solarized-dark and Solarized-light.
;; - Zenburn.
;; - Material.
;; The default theme is tomorrow-night.
;;
;; Usage:
;; Edit ~/.emacs.d/prefs.el and set your favorite theme like so:
;;   (setq exordium-theme 'zenburn)
;;
;; At runtime, change the theme with: M-x load-theme <tab>
;; or evaluate something like: (load-theme 'tomorrow-night t)

;;; Code:

(eval-when-compile
  (unless (featurep 'init-require)
    (load (file-name-concat (locate-user-emacs-file "modules") "init-require"))))
(exordium-require 'init-prefs)

(when exordium-theme
  (load-theme exordium-theme t))

(defun current-theme ()
  "Return the current theme, or nil if no custom theme is enabled."
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


;;; Utilities

(defun switch-theme (theme)
  "Prompt for a THEME name with auto-complete and load it.
Use this function rather than `load-theme' because it makes
Powerline follow."
  (interactive
   (list
    (intern (completing-read "Load theme: "
                             (mapcar 'symbol-name
                                     (custom-available-themes))))))
  (load-theme theme t nil)
  (setq exordium-theme theme)
  (when (fboundp 'powerline-reset)
    (powerline-reset)))

(defun what-face (pos)
  "Display the faces at POS.
Faces set by text properties are reported separately from faces
set by overlays: the latter take priority over the former, and
would otherwise mask them.  For instance the overlay of
`hl-line-mode' covers the whole current line."
  (interactive "d")
  (let ((text-face (or (get-text-property pos 'read-face-name)
                       (get-text-property pos 'face)))
        (overlay-faces (delq nil
                             (mapcar (lambda (overlay)
                                       (or (overlay-get overlay 'read-face-name)
                                           (overlay-get overlay 'face)))
                                     (overlays-at pos)))))
    (cond ((and text-face overlay-faces)
           (message "Face: %s [overlays: %s]" text-face
                    (mapconcat (lambda (face) (format "%s" face))
                               overlay-faces ", ")))
          (text-face
           (message "Face: %s" text-face))
          (overlay-faces
           (message "Face: none [overlays: %s]"
                    (mapconcat (lambda (face) (format "%s" face))
                               overlay-faces ", ")))
          (t
           (message "No face at %d" pos)))))

;; Emacs daemon sometimes displays a some faces incorrectly: it seems like it
;; does not load all the theme colors/fonts when no frame is present during
;; loading time (which is the case when starting in daemon mode). Here we force
;; a reload of the theme when a new frame is created with Emacs client to work
;; around that bug:

(defvar exordium-theme-loaded-in-frame nil
  "Whether `reload-current-theme-in-frame' was called.")

(defun reload-current-theme-in-frame (&optional frame)
  "Reload the current theme in FRAME."
  (when exordium-theme
    (unless exordium-theme-loaded-in-frame
      (when frame (select-frame frame))
      (load-theme exordium-theme t nil)
      (setq exordium-theme-loaded-in-frame t))))

(when (daemonp)
  (add-hook 'server-after-make-frame-hook #'reload-current-theme-in-frame))



(provide 'init-themes)

;;; init-themes.el ends here
