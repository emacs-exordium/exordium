;;;; Themes

;;; --------- ----------------------------------- -----------------------------
;;; Theme     Dark                                Light
;;; --------- ----------------------------------- -----------------------------
;;; Tomorrow  set-colors-tomorrow-night           set-colors-tomorrow-day
;;;           set-colors-tomorrow-night-bright
;;;           set-colors-tomorrow-night-eighties
;;;           set-colors-tomorrow-night-blue
;;; --------- ----------------------------------- -----------------------------
;;; Monokai   set-colors-monokai-default
;;; --------- ----------------------------------- -----------------------------
;;; Solarized set-colors-solarized-dark           set-colors-solarized-light
;;; --------- ----------------------------------- -----------------------------
(require 'color-theme-test)
(require 'color-theme-tomorrow)
(require 'color-theme-solarized)
(require 'color-theme-monokai)

;; Defaults

(define-key global-map [f3] 'set-colors-tomorrow-night)
(define-key global-map [f4] 'set-colors-tomorrow-day)
(set-colors-tomorrow-night)
