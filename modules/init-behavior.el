;;;; Behavior preferences

;;; Indent with spaces, not tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;;; Autofill at 79 characters
(setq-default fill-column 79)
;;;(global-visual-line-mode 1) ; Wordwrap at word boundaries

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File saving and opening

;; warn when opening files bigger than 100MB (use nil to disable it entirely)
(setq large-file-warning-threshold 100000000)

;; Remove trailing blanks on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; Disable backup files (e.g. file~)
(defun no-backup-files ()
  "Disable creation of backup files"
  (interactive)
  (setq make-backup-files nil))
(no-backup-files)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
;;(setq gc-cons-threshold 50000000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Show only 1 window on startup (useful if you open multiple files)

(add-hook 'emacs-startup-hook (lambda () (delete-other-windows)) t)

(provide 'init-behavior)
