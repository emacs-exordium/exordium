;;;; Autocomplete

(require 'auto-complete)

;; Default config for auto-complete
(require 'auto-complete-config)
(ac-config-default)

;; Use ESC to escape autocomplete (in addition to C-g)
(define-key ac-completing-map (kbd "<escape>") 'ac-stop)

(provide 'init-autocomplete)
