;;;; Markdown
;;; See http://jblevins.org/projects/markdown-mode/
;;;
;;; This module provide a minor mode "impatient markdown mode", which similar
;;; to impatient-mode but for markdown buffers as opposed to HTML buffers. It
;;; is actually implemented with the impatient-mode itself.
;;;
;;; When you type command `impatient-markdown-mode' in a markdown buffer, Emacs
;;; starts an embedded HTTP server listening to port 8080 (by default), and it
;;; will direct your favorite web browser to URL
;;; "http://localhost:8080/imp/live/<buffer-name.md>"
;;;
;;; Any change you make in the buffer from that point is automatically rendered
;;; in real-time in the web browser. To stop the HTTP server, run
;;; `impatient-markdown-mode' again. Note that Emacs will complain if you quit
;;; before stopping the server.
;;;
;;; Before you can use it, you need to set the variable `markdown-command' to
;;; the command to execute to render a markdown file into HTML.  To use the
;;; GitHub command, clone https://github.com/github/markup and set
;;; `markdown-command' to the path of bin/github-markup in your after-init.el.
;;; Other options include Pandoc or RedCarpet.
;;;
;;; Note: you can change the variable `httpd-port' if 8080 does not work in
;;; your environment. Also the current implementation uses a temporary file
;;; whose path is defined in `exordium-markdown-file' which can also be
;;; changed.

(require 'init-prefs)
(require 'markdown-mode)

(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))

;;; FIXME: quick workaround for a bug in markdown-mode 2.1 (font lock is broken)
(when (and (boundp 'markdown-mode-version)
           (equal markdown-mode-version "2.1"))
  (add-hook 'markdown-mode-hook 'font-lock-mode))

;;; Loud face for TODOs in markdown documents
(when exordium-font-lock
  (setq markdown-mode-font-lock-keywords-core
        (list
         (cons markdown-regex-italic '(2 markdown-italic-face))
         (cons "\\<\\(TODO\\|FIXME\\|TBD\\):" '(1 font-lock-warning-face)))))

;;; Markdown-mode uses M-p to go to the previous link, which is useless and
;;; conflicts with ace-window, so let's change this:
(when (fboundp 'ace-window)
  (define-key markdown-mode-map (kbd "M-p") #'ace-window))


;;; Impatient markdown mode

(require 'impatient-mode)

(define-minor-mode impatient-markdown-mode
  "Markdown rendering for people who lack patience"
  :group 'exordium
  :lighter "" ;; impatient-mode already has a modeline marker "imp"
  :keymap nil
  :global nil
  ;; Body
  (cond (impatient-markdown-mode
         (start-imp-markdown))
        (t
         (stop-imp-markdown))))

(defcustom exordium-markdown-file "/tmp/imp-markdown-temp.md"
  "Temporary file for markdown rendering"
  :group 'exordium
  :type  'string)

(defun markdown-to-html (buffer)
  (let ((md-file exordium-markdown-file))
    (unwind-protect
        (progn
          (with-temp-file md-file
            (kill-region (point-min) (point-max))
            (insert (with-current-buffer buffer (buffer-string))))
          (shell-command-to-string
           (concat markdown-command " " md-file)))
      (delete-file md-file))))

(defun imp-markdown-visit-buffer ()
  "Visit the buffer in a browser."
  (browse-url
   (format "http://localhost:%d/imp/live/%s/"
           httpd-port (url-hexify-string (buffer-name)))))

(defun start-imp-markdown ()
  "Start the impatient mode for markdown and opens the rendering
in the user's default web browser. Note that if the web browser
wasn't running, Emacs starts it - you may want to close the
browser before Emacs in this case (Emacs will complain at quit
time otherwise)"
  (httpd-start)
  (impatient-mode 1)
  ;; Save the old function
  (unless (fboundp 'imp--send-state-old)
    (defalias 'imp--send-state-old (symbol-function 'imp--send-state)))
  ;; Define a new function
  (defun imp--send-state (proc)
    (let ((id (number-to-string imp-last-state))
          (buffer (current-buffer)))
      (with-temp-buffer
        (insert (markdown-to-html buffer))
        (httpd-send-header proc "text/html" 200
                           :Cache-Control "no-cache"
                           :X-Imp-Count id))))
  (imp-markdown-visit-buffer))

(defun stop-imp-markdown ()
  "Stop the impatient mode for markdown"
  (impatient-mode 0)
  (httpd-stop)
  ;; Restore the old function
  (defalias 'imp--send-state 'imp--send-state-old))

(provide 'init-markdown)
