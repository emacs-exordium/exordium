;;; init-vc-checkout.el --- Install packages from VC -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Description
;; ===========
;;
;; This module provides implementation for `:exodrium-vc-checkout' keyword for
;; `use-package'.
;;
;; When `:exordium-vc-checkout' keyword is specified, containing `use-package'
;; form will first try to install the package from an existing checkout
;; directory.  Such a directory can be either specified as an argument to the
;; `:exordium-vc-checkout' keyword or can be inferred from
;; `exordium-vc-checkout-alist' (which see).  For example:
;;
;; (use-package a-package
;;   :exordium-vc-checkout "/a/path")
;;
;; will attempt to install `a-package' from existing checkout located in
;; "/a/path", where as:
;;
;; (use-package a-package
;;   :exordium-vc-checkout)
;;
;; will search for directory in `exordium-vc-checkout-alist' and, should it
;; exist the `a-package' will be installed form there.
;;
;; Alternatively, when `exordium-always-vc-checkout' is non nil all
;; `use-package' declarations behave as if they had `:exordium-vc-checkout'
;; keyword specified.  That is for each package a checkout directory will be
;; inferred from `exordium-vc-checkout-alist'.  For example:
;;
;; (setq exordium-always-vc-checkout t)
;; (setq exordium-vc-checkout-alist '((a-package . "/a/path")))
;; (use-package a-package)
;;
;; will search for directory in `exordium-vc-checkout-alist' and, should it
;; exist the `a-package' will be installed form there.
;;
;; When `exordium-always-vc-checkout' is non nil and `use-package' form has
;; specified `:exordium-vc-checkout nil' the latter takes precedence.  That is
;; no installation from checkout directory will be attempted.
;;
;; Package is only ever installed from a directory if it seems be valid
;; checkout directory: it exists and is recognisable by
;; `vc-responsible-backend' (which see).
;;
;; When `:ensure' keyword is present and it names `other-package', then the
;; `other-package' will be used as a package name by `:exordium-vc-checkout'.
;; For example:
;;
;; (use-package a-pacakge
;;   :ensure other-package
;;   :exordium-vc-checkout)
;;
;; will search for `other-package' in `exordium-vc-checkout-alist'.
;;
;; Motivation And Main Use Case
;; ============================
;;
;; At first glance the form following form:
;;
;; (use-package a-package
;;   :vc-checkout "/a/path")
;;
;; seems to be very similar with regards to side effects to the:
;;
;; (use-package a-package
;;   :vc t
;;   :load-path "/a/path")
;;
;; Indeed, both forms behave very similar.  Both will install `a-package' from
;; "/a/path" should it exists.  However, the `:exordium-vc-checkout' will also
;; uninstall `a-package' should it be previously installed (for example from
;; ELPA).  Such a behaviour is intentional and is main motivation for this
;; module.
;;
;; It allows Exordium to install packages as usual, while allowing users to
;; shadow a certain subset of them to be automatically, without a need to
;; update Exordium's code.  And as a bonus, when checkouts don't exist, for
;; example when Exordium and taps have been freshly cloned on a new computer,
;; packages will be installed from default locations as specified in Exordium.
;; Or when the tap is shared between multiple computers, each of them can use
;; different version of `a-package'.
;;
;; It is useful for developing external packages used by Exordium, as it
;; provides full control over the checkouts, while avoiding manual packages
;; (re)installations and manual `package-install-from-vc-checkout'
;; specifications.
;;
;; Consider the following setup:
;;
;; in taps/my-tap/prefs.el:
;;
;; (setq exordium-always-vc-checkout t)
;; (setq exordium-vc-checkout-alist '((flycheck "~/src/flycheck")))
;;
;; in modules/init-flycheck.el:
;;
;; (use-package flycheck
;;   ;; Exordium's secret sauce...)
;;
;; When Emacs is started for a first time `flycheck' is installed from MELPA.
;; However, when at some point user decides they want to continue their work on
;; some `flycheck' feature, all they need to do is to clone `flycheck'
;; repository to "~/src/flycheck" and eval the relevant `use-package' form, say
;; by putting a point in it and pressing `C-M-x'.  Alternatively, Emacs can be
;; restarted to the same effect.
;;
;; Please see macroexpansion tests in init-vc-checkout.t.el to get sense
;; of how `:exordium-vc-checkout' interacts with `:ensure' and `:vc'.
;;
;; N.B. All the caveats of reinstalling previously installed packages still
;; apply here.
;;
;; N.B.B. Calls to `exordium--vc-checkout-install' are generated during
;; macroexpansion and value of `exordium-always-vc-checkout' gates their
;; inclusion.  Pay attention to these when byte compiling your code.


;;; Code:
(require 'package)
(require 'use-package)
(require 'vc)

(defcustom exordium-vc-checkout-alist nil
  "An alist of packages to install from VC checkout.
Each element is of a form (PACKAGE DIR), where PACKAGE is the
package name (symbol) and DIR is a directory containing a VC
checkout with the package.  When DIR is relative it will be
expanded within `user-emacs-direcory'."
  :type '(alist :key-type (symbol :tag "Package")
                :value-type (filepath :tag "Directory with checkout"))
  :group 'exordium)

(defcustom exordium-always-vc-checkout nil
  "Treat every package as though it had specified using `:exordium-vc-checkout'.
Note that this will cause already installed packages to be
overwritten with the checked out version, should they have an
entry in `exordium-vc-checkout-alist' and the directory with the
checkout exists when the corresponding `use-package' is
evaluated.  For example this can happen when Emacs is restarted
or when `use-package' form is evaluated with `eval-buffer',
`eval-last-sexp' etc."
  :type 'boolean
  :group 'exordium)

(use-package use-package-core
  :ensure nil
  :defer t
  :autoload (use-package-process-keywords))


(defun exordium--vc-checkout-use-package-normalize (name keyword args)
                                        ; checkdoc-params: (name keyword args)
  "Normalize possible arguments to the :exordium-vc-checkout."
  (cond
   ((and (listp args) (listp (cdr args))
         (= (length args) 1))
    (let ((arg (car args)))
      (pcase arg
        ('nil nil) ; don't install when `:exordium-vc-checkout' is nil
        ('t (list name))
        ((and (pred stringp)
              (guard (< 0 (length arg))))
         (let ((path (if (file-name-absolute-p arg)
                         arg
                       (expand-file-name arg user-emacs-directory))))
           (list name path)))
        (_ (use-package-error (format "Unrecognized argument to %s. \
The keyword wants no arguments or an argument of nil, t, \
or a directory with a checkout."
                                      (symbol-name keyword)))))))
   ((not args)
    (list name)) ; install when `:exordium-vc-checkout' without any argument
   (t
    (use-package-error (concat (symbol-name keyword)
                               " wants at most one argument")))))

(defun exordium--vc-checkout-valid-p (dir)
  "Return non nil when DIR is good enough to attempt using it as a VC checkout."
  (and (stringp dir)
       (not (equal "" dir))
       (file-directory-p dir)
       (vc-responsible-backend dir t)))

(defun exordium--vc-checkout-package-delete (desc)
  "Forcibly delete package DESC and remove it from `load-path'."
  (package-delete desc 'force)
  (setq load-path (cl-remove-if
                   (lambda (dir)
                     (equal dir (package-desc-dir desc)))
                   load-path)))

(defun exordium--vc-checkout-install (name &optional dir)
  "Install the package NAME from a VC checkout.
Package is installed from DIR (if non nil) or form a directory
specified in corresponding entry in `exordium-vc-checkout-alist'.
Delete package NAME if it has been previously installed from VC
or ELPA, except for the case when the package has already been VC
installed from DIR."
  (when-let* ((dir (or dir (alist-get name exordium-vc-checkout-alist)))
              ((exordium--vc-checkout-valid-p dir)))
    (let (installed)
     (when-let* ((desc (cadr (assq name package-alist)))
                 (pkg-dir (package-desc-dir desc)))
       (pcase (package-desc-kind desc)
         ((and
           'vc
           (guard (not
                   (setq installed
                         (equal (file-truename (file-name-as-directory dir))
                                (file-truename (file-name-as-directory
                                                (package-desc-dir desc))))))))
          ;; Package has been previously installed from :vc, but checkout
          ;; appeared on a subsequent eval.  This is to avoid deleting package
          ;; when it has been already VC installed from the specified checkout
          ;; in DIR.
          (message ":exordium-vc-checkout overriding VC package %s in \
%s with checkout in %s"
                   name (package-desc-dir desc) dir)
          (exordium--vc-checkout-package-delete desc))
         ((and kind
               (guard (not (eq kind 'vc))))
          ;; Package has been previously installed from ELPA (or otherwise), but
          ;; checkout appeared on a subsequent eval.
          (message ":exordium-vc-checkout overriding %s package %s \
with checkout in %s"
                   (or kind "ELPA") name dir)
          (exordium--vc-checkout-package-delete desc))))
     (unless installed
       (package-vc-install-from-checkout dir (symbol-name name))))))

(defun exordium--vc-checkout-use-package-handler (name _keyword arg rest state)
  "Generate code to install package NAME from a VC checkout, or do so directly.
When the `use-package' declaration is part of a byte-compiled
file, install the package during compilation; otherwise, add it
to the macro expansion and wait until runtime.  Package NAME is
VC installed from the checkout in DIR being (car ARG).  No code
is generated when ARG is nil.  The remaining arguments are as
follows:

_KEYWORD is ignored.

ARG is the normalized input to the `:exordium-vc-checkout'
keyword, as returned by the
`use-package-normalize/:exordium-vc-checkout' function.

REST is a plist of other (following) keywords and their
arguments, each having already been normalized by the respective
function.

STATE is a plist of any state that keywords processed before
`:exordium-vc-checkout' (see `use-package-keywords') may have
accumulated.

Also see the Info node `(use-package) Creating an extension'."
  (let ((body (use-package-process-keywords name rest state)))
    (when arg ; `:exordium-vc-checkout' is non-nil
      ;; If there's `:ensure' keyword, use it as a name, except for when either
      ;; it is t or it comes from `use-package-always-ensure'.
      (let ((arg (pcase (car (plist-get rest :ensure))
                       ('t arg)
                       ((and (pred consp)
                             name-pin)
                        (list (car name-pin) (cadr arg)))
                       ((and (pred symbolp)
                             name)
                        (list name (cadr arg)))
                       (_ arg))))
        (if (bound-and-true-p byte-compile-current-file)
            (apply #'exordium--vc-checkout-install arg)          ; compile time
          (push `(exordium--vc-checkout-install ',(car arg) ,(cadr arg))
                body))))                                         ; runtime
    body))

(defalias 'use-package-normalize/:exordium-vc-checkout
  #'exordium--vc-checkout-use-package-normalize)

(defalias 'use-package-handler/:exordium-vc-checkout
  #'exordium--vc-checkout-use-package-handler)

(eval-after-load 'use-package-core
  '(progn
     (setf (alist-get :exordium-vc-checkout use-package-defaults)
           '((lambda (name _args)
               (list name))
             (lambda (name args)
               (and exordium-always-vc-checkout
                    (not (plist-get args :exordium-vc-checkout))))))
     (add-to-list 'use-package-keywords :exordium-vc-checkout)))



(provide 'init-vc-checkout)

;;; init-vc-checkout.el ends here
