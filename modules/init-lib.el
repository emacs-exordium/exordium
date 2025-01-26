;;; init-lib.el --- Init lib -*- lexical-binding: t -*-

;;; Commentary:
;;
;; This file defines utility functions reused in other modules.  It should be
;; loaded before any other module.

;;; Code:
(eval-when-compile
  (unless (featurep 'init-require)
    (load (file-name-concat (locate-user-emacs-file "modules") "init-require"))))
(exordium-require 'init-prefs)

(require 'cl-lib)

(use-package compat)

;;; Files

(defun exordium-directory-tree (dir)
  "Return the list of subdirs of DIR excluding any dot dirs.
Input is a string and output is a list of strings."
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
  "Return a list of lines (strings) of the specified FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n" t)))

(defun exordium-read-file-as-string (file)
  "Return the content of the specified FILE as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun exordium-parent-directory (dir)
  "Return the path of the DIR's parent directory."
  (file-name-directory (directory-file-name dir)))

(defun exordium-add-directory-tree-to-load-path (dir &optional ignore-if-absent)
  "Add DIR and all its subdirs to the load path.
Warn if DIR is not a directory and IGNORE-IF-ABSENT is nil."
  (cond ((file-directory-p dir)
         (add-to-list 'load-path dir)
         (let ((default-directory dir))
           (normal-top-level-add-subdirs-to-load-path)))
        ((not ignore-if-absent)
         (warn "Missing directory: %s" dir))))


;;; String manipulation functions

(defun exordium-string-truncate (string n)
  "Return STRING minus the last N characters."
  (substring string 0 (max 0 (- (length string) n))))


(defun exordium-electric-mode-add-back-tick ()
 "Add backtick to electric pair mode.
It makes buffer local variable with an extra back tick added."
  (when exordium-enable-electric-pair-mode
    (setq-local electric-pair-pairs
                (append electric-pair-pairs '((?` . ?`))))
    (setq-local electric-pair-text-pairs
                (append electric-pair-text-pairs '((?` . ?`))))))


(defun exordium-browse-url-at-point ()
  "Open an URL at point."
  (interactive)
  (when-let* ((url (thing-at-point 'url)))
    (browse-url url)))


(defmacro exordium-setf-when-nil (&rest args)
                                        ; checkdoc-params: (args)
  "Like `setf', but check each PLACE before evaluating corresponding VAL.
When the PLACE is non nil return it.  Otherwise set the PLACE to
evaluated VAL and return it.  Note, that the VAL will be
evaluated if and only if when the PLACE is nil.

Example use:
  (let* ((alist \\='((a . 1)))
         (a (exordium-setf-when-nil (alist-get \\='a alist) 2))
         (b (exordium-setf-when-nil (alist-get \\='a alist) 3
                                    (alist-get \\='b alist) 2)))
    (format \"alist=%s, a=%s, b=%s\" alist a b))
yields:
  \"alist=((b . 2) (a . 1)), a=1, b=2\""
  (declare (debug setf))
  (if (/= (logand (length args) 1) 0)
      (signal 'wrong-number-of-arguments (list 'setf (length args))))
  (if (and args (null (cddr args)))
      (let ((place (pop args))
            (val (car args)))
        (gv-letplace (getter setter) place
          `(or ,getter
               ,(macroexp-let2 nil v val
                  (funcall setter `,v)))))
    (let ((sets nil))
      (while args
        (push `(exordium-setf-when-nil ,(pop args) ,(pop args)) sets))
      (cons 'progn (nreverse sets)))))


(provide 'init-lib)

;;; init-lib.el ends here
