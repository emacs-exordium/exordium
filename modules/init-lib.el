;;;; Init lib
;;;
;;; This file defines utility functions reused in other modules. It should be
;;; loaded before any other module.



;;; Files

(defun pg/directory-tree (dir)
  "Returns the list of subdirs of 'dir' excluding any dot
dirs. Input is a string and output is a list of strings."
  (let* ((dir   (directory-file-name dir))
         (dirs  '())
         (files (directory-files dir nil nil t)))
    (dolist (f files)
      (unless (string-equal "." (substring f 0 1))
        (let ((f (concat dir "/" f)))
          (when (file-directory-p f)
            (setq dirs (append (cons f (pg/directory-tree f))
                               dirs))))))
    dirs))

(defun pg/read-file-lines (file)
  "Return a list of lines (strings) of the specified file"
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n" t)))

(defun pg/read-file-as-string (file)
  "Return the content of the specified file as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))


;;; Strings

(defun pg/string-without-last (string n)
  "Return string minus the last n characters."
  (substring string 0 (max 0(- (length string) n))))

(defun pg/string-ends-with (string tail)
  "Predicate checking whether string ends with the given tail."
  (string= tail (substring string (- (length tail)))))

(defun pg/string-starts-with (string prefix)
  "Return t if STRING starts with prefix."
  (and (string-match (rx-to-string `(: bos ,prefix) t)
                     string)
       t))

(defun pg/string-trim-end (str)
  "Return a new string with tailing whitespace removed"
  (replace-regexp-in-string (rx (* (any " \t\n")) eos)
                            ""
                            str))

(defun pg/string-trim (str)
  "Return a new string with leading and tailing whitespace removed"
  (replace-regexp-in-string (rx (or (: bos (* (any " \t\n")))
                                    (: (* (any " \t\n")) eos)))
                            ""
                            str))


;;; Others

(defun pg/find-if (predicate list)
  (catch 'found
    (dolist (elt list nil)
      (when (funcall predicate elt)
        (throw 'found elt)))))


(provide 'init-lib)
