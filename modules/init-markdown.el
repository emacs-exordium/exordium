;;;; Markdown
;;; See http://jblevins.org/projects/markdown-mode/

(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))

;;; Loud face for TODOs in markdown documents
(require 'init-prefs)
(require 'markdown-mode)
(when exordium-font-lock
  (setq markdown-mode-font-lock-keywords-core
        (list
         (cons markdown-regex-italic '(2 markdown-italic-face))
         (cons "\\<\\(TODO\\|FIXME\\|TBD\\):" '(1 font-lock-warning-face)))))


;;; Render a markdown buffer.
;;;
;;; Before you can use the functions below, you need to set the variable
;;; `markdown-command' to the command to execute to render a markdown file into
;;; HTML.
;;;
;;; To use the GitHub command, clone https://github.com/github/markup and set
;;; `markdown-command' to the path of bin/github-markup in your after-init.el.
;;; Then use M-x preview-github-markdown.
;;;
;;; Daring Fireball has this: https://daringfireball.net/projects/markdown.
;;; Use M-x preview-markdown.
;;;
;;; The difference between the 2 functions is that preview-github-markdown
;;; saves the buffer before running `markdown-command', because the GH command
;;; requires this.

(unless (version< emacs-version "24.4")
  (require 'shr)
  (require 'eww)

  (defun preview-markdown ()
    "Render the current markdown buffer using
`markdown-command' (which must be set to the path of your local
command) in HTML and display it with eww. This function works
only if the markdown command accepts standard input."
    (interactive)
    (let* ((buf-md (buffer-name (current-buffer)))
           (buf-html (get-buffer-create
                      (format "*preview %s*" buf-md))))
      (markdown-other-window (buffer-name buf-html))
      (shr-render-buffer buf-html)
      (eww-mode)
      (kill-buffer buf-html)))

  (defun preview-github-markdown ()
    "Save the current markdown buffer and render the file using
`markdown-command' (which must be set to the path of your local
command) in HTML and display it with eww. This function is
intended to be used with github-markup which requires a file on
disk."
    (interactive)
    (save-buffer)
    (let* ((buf-md (buffer-name (current-buffer)))
           (buf-html (get-buffer-create
                      (format "*preview %s*" buf-md)))
           (markdown-command (concat markdown-command " " (buffer-file-name))))
      (markdown-other-window (buffer-name buf-html))
      (shr-render-buffer buf-html)
      (eww-mode)
      (kill-buffer buf-html)))
  ) ; unless

(provide 'init-markdown)
