;;; init-force-elpa.el --- Force installation from ELPA -*- lexical-binding: t -*-

;;; Commentary:
;;
;; This module is a keyword extension for `use-pacakage'.  It adds keyword
;; `:exordium-force-elpa'.  It takes a single argument, that is an archive name
;; from which the package should be installed, and pinned to.  When containing
;; `use-package' is evaluated, and the package is a built in package, and the
;; package has a newer version in the specified archive then it is installed
;; from the specified archive.
;;
;; This alleviates a need for a manual installation in case of
;; upgrading a built-in package as described in Info node Info node `(emacs)
;; Package Installation'.
;;
;; N.B. Using this keyword achieves similar result to what `:pin' does.  Using
;; the latter is not necessary, when `:exordium-force-elpa' is used.  Should
;; the `:pin' be used pay extra care to make sure it points to the same archive
;; as `:exordium-force-elpa'.

;;; Code:
(require 'package)

(eval-when-compile
  (use-package use-package-core
    :ensure nil
    :defer t
    :autoload (use-package-only-one
               use-package-process-keywords)))

(defun exordium--use-package-force-elpa-normalize (_name keyword args)
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
                 (find-archive-desc
                  (lambda ()
                    (cl-find-if (lambda (desc)
                                  (equal (package-desc-archive desc)
                                         ,archive))
                                (alist-get package
                                           package-archive-contents))))
                 (pkg-desc (or
                            (funcall find-archive-desc)
                            (progn
                              (package-refresh-contents)
                              (funcall find-archive-desc))))
                 (archive-version (package-desc-version pkg-desc))
                 ;; package has been previously installed, either not a
                 ;; built-in package or a built-in package has been shadowed by
                 ;; installation
                 ((not (assq package package-alist)))
                 ;; package with archive-version that is higher than the
                 ;; built-in one
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

(defun exordium--use-package-handler-force-elpa (name _keyword archive-name rest state)
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

(defalias 'use-package-normalize/:exordium-force-elpa
  #'exordium--use-package-force-elpa-normalize)

(defalias 'use-package-handler/:exordium-force-elpa
  #'exordium--use-package-handler-force-elpa)

(eval-after-load 'use-package-core
  '(add-to-list 'use-package-keywords :exordium-force-elpa))

(provide 'init-force-elpa)

;;; init-force-elpa.el ends here
