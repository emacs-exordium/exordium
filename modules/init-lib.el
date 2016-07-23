;;;; Init lib
;;;
;;; This file defines utility functions reused in other modules. It should be
;;; loaded before any other module.

(with-no-warnings (require 'cl))


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

(require 'subr-x)

;; string-prefix-p has been in Emacs for years, but string-suffix-p was
;; introduced only in Emacs 24.4.

(unless (fboundp 'string-suffix-p)
  (defun string-suffix-p (suffix string  &optional ignore-case)
    "Return non-nil if SUFFIX is a suffix of STRING.
If IGNORE-CASE is non-nil, the comparison is done without paying
attention to case differences."
    (let ((start-pos (- (length string) (length suffix))))
      (and (>= start-pos 0)
           (eq t (compare-strings suffix nil nil
                                  string start-pos nil ignore-case))))))

;; Other string functions introduced in Emacs 24.4:

(unless (fboundp 'string-trim-left)
  (defsubst string-trim-left (string)
    "Remove leading whitespace from STRING."
    (if (string-match "\\`[ \t\n\r]+" string)
        (replace-match "" t t string)
      string)))

(unless (fboundp 'string-trim-right)
  (defsubst string-trim-right (string)
    "Remove trailing whitespace from STRING."
    (if (string-match "[ \t\n\r]+\\'" string)
        (replace-match "" t t string)
      string)))

(unless (fboundp 'string-trim)
  (defsubst string-trim (string)
    "Remove leading and trailing whitespace from STRING."
    (string-trim-left (string-trim-right string))))

(eval-when-compile (assert (not (fboundp 'string-truncate))))

(defun string-truncate (string n)
  "Return STRING minus the last N characters."
  (substring string 0 (max 0(- (length string) n))))


(provide 'init-lib)
