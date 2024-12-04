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


;; Packages management
(require 'package)
(eval-when-compile
  (use-package use-package-core
    :ensure use-package
    :defer t
    :autoload (use-package-only-one
               use-package-process-keywords)))

(defun use-package-normalize/:exordium-force-elpa (_name keyword args)
                                        ; checkdoc-params: (keyword args)
  "Allow either a single string or a single symbol."
  (use-package-only-one (symbol-name keyword) args
    #'(lambda (_label arg)
        (cond
         ((stringp arg) arg)
         ((use-package-non-nil-symbolp arg) (symbol-name arg))
         (t
          (use-package-error
           ":exordium-force-elpa wants an archive name (a string)"))))))

(defun exordium--use-package-force-elpa (pkg archive)
  "Return the form that enforces installation of a built-in PKG from ARCHIVE."
  `(let ((package ',(use-package-as-symbol pkg)))
     ;; Ensure package is pinned even if it won't be installed
     (use-package-pin-package package ,archive)
     (package-read-all-archive-contents)
     (when-let* (((package-built-in-p package))
                 (builtin-version (alist-get package package--builtin-versions))
                 (pkg-desc (or
                            (cl-find-if (lambda (desc)
                                          (equal (package-desc-archive desc)
                                                 ,archive))
                                        (alist-get package
                                                   package-archive-contents))
                            (progn
                              (package-refresh-contents)
                              (cl-find-if (lambda (desc)
                                            (equal (package-desc-archive desc)
                                                   ,archive))
                                          (alist-get package
                                                     package-archive-contents)))))
                 (archive-version (package-desc-version pkg-desc))
                 ((not (package-installed-p package archive-version)))
                 ((version-list-< builtin-version archive-version)))
       (package--save-selected-packages (cons package
                                              package-selected-packages))
       (condition-case-unless-debug err
           (let* ((package-install-upgrade-built-in t)
                  (transaction (package-compute-transaction
                                (list pkg-desc)
                                (package-desc-reqs pkg-desc)))
                  (transaction-load-path (mapcar
                                          (lambda (desc)
                                            (expand-file-name
                                             (package-desc-full-name desc)
                                             package-user-dir))
                                          transaction))
                  (with-load-path
                   (lambda (orig-fun &rest args)
                     (let (new-load-path)
                       ;; Ensure the newly installed package and its
                       ;; dependencies are is in `load-path', when they are
                       ;; reloaded.
                       (let ((load-path (append transaction-load-path load-path)))
                         (apply orig-fun args)
                         (setq new-load-path load-path))
                       ;; After reload, ensure all directories that were added
                       ;; during reload are in the original `load-path'.
                       (dolist (dir new-load-path)
                         (unless (or (member dir transaction-load-path)
                                     (member dir load-path))
                           (push dir load-path)))))))
             (unwind-protect
                 (progn
                   ;; `packgage-activate-1' calls
                   ;; `package--reload-previously-loaded' and then adds the
                   ;; newly installed package's directory to `load-path'. This
                   ;; however may be not sufficient when some files `require'
                   ;; files from the package.  Ensure such `require'd files are
                   ;; visible for the latter call, and allow the original
                   ;; `load-path' to be updated by the former (likely when
                   ;; loading package autoloads).
                   (advice-add 'package--reload-previously-loaded
                               :around
                               with-load-path)
                   (package-download-transaction transaction)
                   (package--quickstart-maybe-refresh)
                   t)
               (advice-remove 'package--reload-previously-loaded
                              with-load-path)))
         (error
          (display-warning 'use-package
                           (format "Failed to force ELPA installation %s: %s"
                                   name (error-message-string err))
                           :error))))))

(defun use-package-handler/:exordium-force-elpa (name _keyword archive-name rest state)
                                        ; checkdoc-params: (rest state)
  "Pin package NAME to ELPA archive ARCHIVE-NAME and install it from there.
Installation and pinning only hapens when the package is a
built-in package and the archive ARCHIVE-NAME has a newer
version of it (according to `version-list-<').

Note that after the package NAME has been forcefully installed
from ELPA archive it shadows the built-in package and it becomes
eligible for upgrading while, i.e., `package-upgrade' is called,
see Info node `(emacs) Package Installation'."
  (let ((body (use-package-process-keywords name rest state))
        (force-elpa-form (when archive-name
                           (exordium--use-package-force-elpa name
                                                             archive-name))))
    ;; Pinning should occur just before ensuring
    ;; See `use-package-handler/:ensure'.
    (if (bound-and-true-p byte-compile-current-file)
        (eval force-elpa-form)              ; Eval when byte-compiling,
      (push force-elpa-form body))          ; or else wait until runtime.
    body))

(add-to-list 'use-package-keywords :exordium-force-elpa)

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
