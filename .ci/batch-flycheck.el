;;; batch-flycheck.el --- Run flycheck on command line arguments -*- lexical-binding: t -*-

;;; Commentary:
;; This is a transformation of batch-checkdoc.el to handle support `emacs-lisp'
;; checker, with `flycheck-checker-shell-command'.

;;; Code:

(require 'flycheck)
(require 'rx)
(require 'cl-lib)

(defun exordium--relevant-flycheck-warnings-and-errors (buffer source)
  "Find relevant flycheck warning and errors in BUFFER, in reverse order.
SOURCE is the source file used to compile with
`flycheck-checker-shell-command'."
  (with-current-buffer buffer
    (goto-char (point-min))
    (let (matches
          (not-errors
           (list
            (rx "the function ‘exordium--require-load’ might not be defined at runtime.")
            (rx "the function `exordium--require-load' might not be defined at runtime.")
            (rx "‘package-vc-install-from-checkout’ is an obsolete function (as of 31.1); use the User Lisp directory instead.")
            (rx "`package-vc-install-from-checkout' is an obsolete function (as of 31.1); use the User Lisp directory instead."))))
      (when-let* (((re-search-forward (rx-to-string `(seq " -- " ,source line-end))
                                      nil t))
                  (pattern (rx-to-string
                            `(group line-start ,(file-name-nondirectory source)
                                    (optional ":" (one-or-more digit)
                                              ":" (optional (one-or-more digit)))
                                    ": " (or "Warning" "Error") ": "
                                    (one-or-more not-newline) line-end))))
        (while (re-search-forward pattern nil t)
          (push (buffer-substring-no-properties (match-beginning 1) (match-end 1))
                matches)))
      (cl-remove-if
       (lambda (match)
         (cl-some (lambda (not-error)
                    (string-match not-error match))
                  not-errors))
       matches))))

(defun batch-flycheck ()
  "Run `flycheck-checker-shell-command' on the files remaining on the command line."
  (let (errors
        (number-of-errors 0))
    (unwind-protect
        (let
            ((jka-compr-inhibit t))
          (when
              (equal
               (car command-line-args-left)
               "--")
            (setq command-line-args-left
                  (cdr command-line-args-left)))
          (unless
              (require 'elisp-mode nil 'no-error)
            (require 'lisp-mode))
          (require 'cl-lib)
          (while command-line-args-left
            (let
                ((source
                  (car command-line-args-left))
                 (buffer-name "*exordium flycheck compile*")
                 (process-default-directory default-directory))
              (with-temp-buffer
                (insert-file-contents source 'visit)
                (setq buffer-file-name source)
                (setq default-directory process-default-directory)
                (with-demoted-errors "Error in flycheck: %S"
                  (delay-mode-hooks
                    (emacs-lisp-mode))
                  (setq delayed-mode-hooks nil)
                  (let ((comp-buf
                         (compilation-start
                          (flycheck-checker-shell-command 'emacs-lisp)
                          nil (lambda (&rest _)  buffer-name))))
                    (when-let* ((comp-proc (get-buffer-process comp-buf)))
                      (while (process-live-p comp-proc)
                        (sleep-for 0.1)))
                    (with-current-buffer comp-buf
                      (princ
                       (buffer-substring-no-properties
                        (point-min)
                        (point-max)))
                      (princ "\n")
                      (dolist (err (exordium--relevant-flycheck-warnings-and-errors
                                    comp-buf source))
                        (push err errors)
                        (cl-incf number-of-errors))
                      (kill-buffer))))))
            (setq command-line-args-left (cdr command-line-args-left))))
      (setq command-line-args-left nil)
      (when (< 0 number-of-errors)
        (message "\n===Errors and Warnings===\n%s\n"
                 (mapconcat #'identity errors "\n"))
        (let ((msg (format "There are %s flycheck errors!" number-of-errors)))
          (error msg))))))

(provide 'batch-flycheck)

;;; batch-flycheck.el ends here
