;;; batch-checkdoc.el --- Run checkdoc on command line arguments -*- lexical-binding: t -*-

;;; Commentary:
;; This is a transformation of
;;   (flycheck-checker-shell-command 'emacs-lisp-checkdoc)
;;
;; See batch-checkdoc-and-flycheck.org file for a tips how to get a template to
;; fiddle with to produce function `batch-checkdoc'.

;;; Code:

(defvar jka-compr-inhibit)
(defvar checkdoc-diagnostic-buffer)

(defun batch-checkdoc ()
  "Run `checkdoc' on the files remaining on the command line."
  (let ((number-of-errors 0))
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
          (require 'checkdoc)
          (require 'cl-lib)
          (while command-line-args-left
            (let
                ((source
                  (car command-line-args-left))
                 (process-default-directory default-directory))
              (with-temp-buffer
                (insert-file-contents source 'visit)
                (setq buffer-file-name source)
                (setq default-directory process-default-directory)
                (with-demoted-errors "Error in checkdoc: %S"
                  (delay-mode-hooks
                    (emacs-lisp-mode))
                  (setq delayed-mode-hooks nil)
                  (checkdoc-current-buffer t)
                  (with-current-buffer checkdoc-diagnostic-buffer
                    ;; When there are no errors, there are only a few lines in the
                    ;; beginning of the buffer, followed by a single line
                    ;; containing a `checkdoc' stamp.  Remove them and see if
                    ;; buffer has more lines than 1.
                    (let ((inhibit-read-only t))
                      (goto-char (point-min))
                      (when-let* (((looking-at
                                    (rx string-start (group (zero-or-more (or whitespace "\n"))
                                                            (or "\n" line-end))))))
                        (delete-region (match-beginning 1) (match-end 1))))
                    (cl-incf number-of-errors (- (car (buffer-line-statistics))
                                                 1))
                    (princ
                     (buffer-substring-no-properties
                      (point-min)
                      (point-max)))
                    (princ "\n")
                    (kill-buffer)))))
            (setq command-line-args-left (cdr command-line-args-left))))
      (setq command-line-args-left nil)
      (when (< 0 number-of-errors)
        (error "There are %s checkdoc errors!" number-of-errors)))))

(provide 'batch-checkdoc)
;;; batch-checkdoc.el ends here
