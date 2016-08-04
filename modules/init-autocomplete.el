;;;; Autocomplete

(require 'auto-complete)
(require 'init-lib)

;; Default config for auto-complete
(require 'auto-complete-config)
(ac-config-default)

;; Case sensitivity is important when finding matches
;; Values are: t, nil, or 'smart
(setq ac-ignore-case nil)

;; Start auto-completion after 2 characters of a word
;; Values are: an integer, or nil to disable
(setq ac-auto-start 2)

;; Use ESC to escape auto-complete (in addition to C-g)
(exordium-define-key ac-completing-map 'ac-stop)
(exordium-define-key ac-completing-map 'ac-complete)

;; Key to force trigger auto-complete (useful if ac-auto-start is set to nil)
(exordium-define-key global-map 'auto-complete)

(provide 'init-autocomplete)
