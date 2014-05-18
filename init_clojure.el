;;;; Let's give Light Table a run for its money

;;;; TODO This is not yet ready

;;; Env PATH
(defun set-exec-path-for-lein ()
  (let ((path-from-shell
         (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(set-exec-path-for-lein)

(add-to-list 'load-path "~/.emacs.d/vendor")

;;; Flyspell often slows down editing so it's turned off
(remove-hook 'text-mode-hook 'turn-on-flyspell)

(load "~/.emacs.d/vendor/clojure")

;;; Hippie expand - don't try to complete with file names
(setq hippie-expand-try-functions-list
      (delete 'try-complete-file-name hippie-expand-try-functions-list))
(setq hippie-expand-try-functions-list
      (delete 'try-complete-file-name-partially hippie-expand-try-functions-list))

;; (setq ido-use-filename-at-point nil)
