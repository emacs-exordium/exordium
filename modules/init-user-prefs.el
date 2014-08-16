;;;; User preferences

;;; Indent with spaces, not tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;;; Autofill at 79 characters
(setq-default fill-column 79)
;;;(global-visual-line-mode 1) ; Wordwrap at word boundaries

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File saving and opening

;; Don't tell me this file is too large
(setq large-file-warning-threshold nil)

;; Remove trailing blanks on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; Disable backup files (e.g. file~)
(defun no-backup-files ()
  "Disable creation of backup files"
  (interactive)
  (setq make-backup-files nil))
(no-backup-files)

(provide 'init-user-prefs)
