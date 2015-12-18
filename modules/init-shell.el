;;;; Shell mode

(require 'comint)

;;; This is needed so that clearing shell buffer works as expected
(remove-hook 'comint-output-filter-functions
             'comint-postoutput-scroll-to-bottom)

;;; Display colors in shell
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(custom-set-variables
 '(comint-scroll-to-bottom-on-input t)  ; always insert at the bottom
 '(comint-scroll-to-bottom-on-output nil) ; always add output at the bottom
 '(comint-scroll-show-maximum-output t) ; scroll to show max possible output
 '(comint-input-ignoredups t)           ; no duplicates in command history
 '(comint-completion-addsuffix t)       ; insert space after file completion
 '(comint-buffer-maximum-size 20000)    ; max length of the buffer in lines
 '(comint-prompt-read-only nil)         ; if this is t, it breaks shell-command
 '(comint-get-old-input (lambda () "")) ; what to run when I press enter on a
                                        ; line above the current prompt
 '(comint-input-ring-size 5000)         ; max shell history size
 '(protect-buffer-bury-p nil)
)

;;; Clear the buffer
(defun clear-buffer ()
  "Clears the current buffer, removing all its content"
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

;;; Set $PAGER to `cat` instead of `less` etc in shell mode
(setenv "PAGER" "cat")

(provide 'init-shell)
