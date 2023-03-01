;;;; Autocomplete

(use-package auto-complete
  :custom
  ;; Case sensitivity is important when finding matches
  ;; Values are: t, nil, or 'smart
  (ac-ignore-case nil)
  ;; Start auto-completion after 2 characters of a word
  ;; Values are: an integer, or nil to disable
  (ac-auto-start 2)
  :config
  ;; Default config for auto-complete
  (ac-config-default)
  :bind
  (;; Key to force trigger auto-complete (useful if ac-auto-start is set to nil)
   ("C-." . #'auto-complete)
   :map ac-completing-map
        ("<escape>" . #'ac-stop)
        ([return] . #'ac-complete)))

(provide 'init-autocomplete)
