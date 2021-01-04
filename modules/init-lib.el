;;;; Init lib
;;;
;;; This file defines utility functions reused in other modules. It should be
;;; loaded before any other module.

(use-package cl-lib :ensure nil)


;;; Files

(defun exordium-directory-tree (dir)
  "Returns the list of subdirs of 'dir' excluding any dot
dirs. Input is a string and output is a list of strings."
  (let* ((dir   (directory-file-name dir))
         (dirs  '())
         (files (directory-files dir nil nil t)))
    (dolist (f files)
      (unless (string-equal "." (substring f 0 1))
        (let ((f (concat dir "/" f)))
          (when (file-directory-p f)
            (setq dirs (append (cons f (exordium-directory-tree f))
                               dirs))))))
    dirs))

(defun exordium-read-file-lines (file)
  "Return a list of lines (strings) of the specified file"
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n" t)))

(defun exordium-read-file-as-string (file)
  "Return the content of the specified file as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun exordium-parent-directory (dir)
  "Return the path of the dir's parent directory"
  (file-name-directory (directory-file-name dir)))


;;; String manipulation functions

(defun exordium-string-truncate (string n)
  "Return STRING minus the last N characters."
  (substring string 0 (max 0 (- (length string) n))))


;;; Add backtick to electric pair mode. It makes buffer local variable with
;;; an extra back tick added
(defun exordium-electric-mode-add-back-tick ()
  (when exordium-enable-electric-pair-mode
    (setq-local electric-pair-pairs
                (append electric-pair-pairs '((?` . ?`))))
    (setq-local electric-pair-text-pairs
                (append electric-pair-text-pairs '((?` . ?`))))))


(defun exordium-browse-url-at-point ()
  "Open an URL at point."
  (interactive)
  (when-let ((url (thing-at-point 'url)))
    (browse-url url)))


(provide 'init-lib)
