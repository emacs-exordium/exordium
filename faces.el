;; (set-face-foreground 'modeline  "red")
(font-lock-mode)

(if (not (string-match "XEmacs" emacs-version))
  (cond (window-system
       (setq hilit-mode-enable-list  '(not text-mode)
             hilit-background-mode   'dark
             hilit-inhibit-hooks     nil
             hilit-inhibit-rebinding nil
	     hilit-auto-highlight-maxout 2500
	     hilit-auto-rehighlight-fallback '(2000 . 1000))

;;       (set-face-background 'highlight "brown")
;;       (set-face-background 'region    "blue")
;;       (set-face-foreground 'modeline  "yellow")

       (require 'hilit19)))
)
