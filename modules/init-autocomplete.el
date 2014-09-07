;;;; Autocomplete

(require 'auto-complete)

;; Default config for auto-complete
(require 'auto-complete-config)
(ac-config-default)

;; Use ESC to escape auto-complete (in addition to C-g)
(define-key ac-completing-map (kbd "<escape>") 'ac-stop)

;; Use Meta-/ to force trigger auto-complete
(global-set-key "\M-/" 'auto-complete)

(provide 'init-autocomplete)
