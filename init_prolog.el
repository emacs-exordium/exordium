;;;; Init prolog
;;;
;;; This file defines utility functions reused in many modules, such as themes,
;;; CEDET etc. It should be loaded before any other module.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Environment functions

(defun emacs-nw-p ()
  (not window-system))

(defun emacs-osx-p ()
  (eq window-system 'ns))

(defun emacs-x-p ()
  (eq window-system 'x))

(defun gnu-emacs-p ()
  (string-match "GNU Emacs" (version)))

(defun emacs-24-p ()
  (>= emacs-major-version 24))

(defun emacs-23-p ()
  (>= emacs-major-version 23))

;; (defun emacs-22-p ()
;;   (string-match "GNU Emacs 22" (version)))

(defmacro emacs-linux-p ()
  (string-match "linux" (prin1-to-string system-type)))

(defmacro emacs-bloomberg-p ()
  (and (emacs-x-p) (getenv "MBIG_NUMBER")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utility functions reused by several modules

(defun directory-tree (dir)
  "Returns the list of subdirs of 'dir' excluding any dot
dirs. Input is a string and output is a list of strings."
  (let* ((dir   (directory-file-name dir))
         (dirs  '())
         (files (directory-files dir nil nil t)))
    (dolist (f files)
      (unless (string-equal "." (substring f 0 1))
        (let ((f (concat dir "/" f)))
          (when (file-directory-p f)
            (setq dirs (append (cons f (directory-tree f))
                               dirs))))))
    dirs))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Strings

(defun string/ends-with (string suffix)
  "Return t if STRING ends with SUFFIX."
  (and (string-match (rx-to-string `(: ,suffix eos) t)
                     string)
       t))

(defun string/starts-with (string prefix)
  "Return t if STRING starts with prefix."
  (and (string-match (rx-to-string `(: bos ,prefix) t)
                     string)
       t))
