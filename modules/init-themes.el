;;;; Themes
;;;
;;; Usage:
;;; New way: M-x load-theme <tab>  or (load-theme 'tomorrow-night t)
;;; Old way: M-x set-colors-XXX    or (set-colors-tomorrow-night)
;;;
;;; --------- ------------------------------------ -----------------------------
;;; Theme     Dark                                 Light
;;; --------- ------------------------------------ -----------------------------
;;; Tomorrow  `set-colors-tomorrow-night'          `set-colors-tomorrow-day'
;;;           `set-colors-tomorrow-night-bright'
;;;           `set-colors-tomorrow-night-eighties'
;;;           `set-colors-tomorrow-night-blue'
;;; --------- ------------------------------------ -----------------------------
;;; Monokai   `set-colors-monokai-default'
;;; --------- ------------------------------------ -----------------------------
;;; Solarized `set-colors-solarized-dark'          `set-colors-solarized-light'
;;; --------- ------------------------------------ -----------------------------

(require 'color-theme-tomorrow)
;;(require 'color-theme-monokai)

;; Defaults

;;(set-colors-tomorrow-night)
(load-theme *init-theme* t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Org mode extra statuses

(cond ((eq *init-theme* 'monokai)
       (with-monokai-colors
        'default
        (setq org-todo-keyword-faces
              `(("WORK" . (:foreground ,yellow :weight bold :box nil))
                ("WAIT" . (:foreground ,orange :weight bold :box nil))))))
      (t
       (with-tomorrow-colors
        'night
        (setq org-todo-keyword-faces
              `(("WORK" . (:foreground ,yellow :weight bold :box t))
                ("WAIT" . (:foreground ,orange :weight bold :box t)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Linum extension

(load "~/.emacs.d/themes/hilinum-mode.el")
(require 'hlinum)
(hlinum-activate)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utilities

(defun what-face (pos)
  "Display the face at point"
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(provide 'init-themes)
