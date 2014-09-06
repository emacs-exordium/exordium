;;;; Themes
;;;
;;; Usage: M-x set-colors-XXX
;;;
;;; --------- ------------------------------------ -----------------------------
;;; Theme     Dark                                 Light
;;; --------- ------------------------------------ -----------------------------
;;; Tomorrow  `set-colors-tomorrow-night'         `set-colors-tomorrow-day'
;;;           `set-colors-tomorrow-night-bright'
;;;           `set-colors-tomorrow-night-eighties'
;;;           `set-colors-tomorrow-night-blue'
;;; --------- ------------------------------------ -----------------------------
;;; Monokai   `set-colors-monokai-default'
;;; --------- ------------------------------------ -----------------------------
;;; Solarized `set-colors-solarized-dark'          `set-colors-solarized-light'
;;; --------- ------------------------------------ -----------------------------

;;(require 'color-theme-test)
(require 'color-theme-tomorrow)
;;(require 'color-theme-solarized)
;;(require 'color-theme-monokai)

;; Defaults

;;(define-key global-map [f3] 'set-colors-tomorrow-night)
;;(define-key global-map [f4] 'set-colors-tomorrow-day)
(set-colors-tomorrow-night)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Org mode

(with-tomorrow-colors
 'night
 (setq org-todo-keyword-faces
       `(("WORK" . (:foreground ,yellow :weight bold :box t))
         ("WAIT" . (:foreground ,orange :weight bold :box t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Linum extension

(load "~/.emacs.d/themes/hilinum-mode.el")
(require 'hlinum)
(hlinum-activate)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utilities

(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(provide 'init-themes)
