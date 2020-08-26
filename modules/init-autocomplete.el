;;;; Autocomplete

(use-package auto-complete :ensure nil)

;; Default config for auto-complete
(use-package auto-complete-config :ensure nil)
(ac-config-default)

;; Case sensitivity is important when finding matches
;; Values are: t, nil, or 'smart
(setq ac-ignore-case nil)

;; Start auto-completion after 2 characters of a word
;; Values are: an integer, or nil to disable
(setq ac-auto-start 2)

;; Use ESC to escape auto-complete (in addition to C-g)
(define-key ac-completing-map (kbd "<escape>") 'ac-stop)
(define-key ac-completing-map [return] 'ac-complete)

;; Key to force trigger auto-complete (useful if ac-auto-start is set to nil)
(global-set-key [(control .)] 'auto-complete)

(provide 'init-autocomplete)
