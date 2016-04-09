;;;; Let's give Light Table a run for its money
;;;;
;;;; Installed packages (and dependencies):
;;;; - cider
;;;; - rainbow-delimiters
;;;; - paredit
;;;; And you need 'lein' from leiningen.org in your path.
;;;; Note that clojure.el is in subdir 'extensions'.
;;;;
;;;; Usage:
;;;;   M-x cider-jack-in
;;;;   (Ignore the error when it starts). Use M-C-x to evaluate Clojure forms.
;;;;   C-c C-q to quit in the REPL (or use teh CIDER menu)

;;; Env PATH
(defun set-exec-path-for-lein ()
  (let ((path-from-shell
         (shell-command-to-string "$SHELL -i -c -n 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(set-exec-path-for-lein)

;;; Flyspell often slows down editing so it's turned off
(remove-hook 'text-mode-hook 'turn-on-flyspell)

(load "clojure.el")

;;; Hippie expand - don't try to complete with file names
(setq hippie-expand-try-functions-list
      (delete 'try-complete-file-name hippie-expand-try-functions-list))
(setq hippie-expand-try-functions-list
      (delete 'try-complete-file-name-partially hippie-expand-try-functions-list))

;; (setq ido-use-filename-at-point nil)

(provide 'init-clojure)
