;;;; Themes

(require 'color-theme-test)
(require 'color-theme-tomorrow)
(require 'color-theme-solarized)
(require 'color-theme-monokai)

;; Defaults

;; (define-key global-map [f3] 'set-colors-solarized-dark)
;; (define-key global-map [f4] 'set-colors-solarized-light)
;; (set-colors-solarized-dark)

(define-key global-map [f3] 'set-colors-tomorrow-night)
(define-key global-map [f4] 'set-colors-tomorrow-day)
(set-colors-tomorrow-night)
