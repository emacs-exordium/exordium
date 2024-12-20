;;; init-require.el --- Exordium require/loading library -*- lexical-binding: t -*-

;;; Commentary:
;;
;; The `exordium-require' has been designed to be used in Exordium as a
;; replacement to built-in `load', `require', and `use-package'.  It is heavily
;; influenced by implementations of `require' and `use-package'.
;;
;; In order use the `exordium-require' you need to manually load it in the
;; module.  For example:
;;
;; (eval-when-compile
;;   (unless (featurep 'init-require)
;;     (load (file-name-concat (locate-user-emacs-file "modules") "init-require"))))
;;
;; Macro arguments were inspired by `require' and `use-package', although they
;; were adjusted to support needs of Exordium.  In particular, the LOCATION
;; argument is used as a relative directory inside Exordium.  For example
;; "modules", "themes", or "taps/my-tap".  The default LOCATION is "modules".
;;
;; The DECLARATION is a plist that can be used to generate `declare-function'
;; and `defvars' forms, should the compiler not infer them from the loaded
;; module.  For example, the folllowing expression:
;;
;; (exordium-require 'init-foo
;;                   :location "foo-dir"
;;                   :functions (foo-simple (foo-args . (arg1 arg2)))
;;                   :defines (foo-var))
;;
;; will:
;;  - when the module either is evaluated or is compiled, it will `load' a
;;    feature 'init-foo a file "foo-dir/init-foo" (following `load-suffixes'
;;    and `load-prefer-newer', which see),
;;  - when the file containing the `exordium-require' is compiled the following
;;    forms will be generated:
;;
;; (declare-function foo-simple nil)
;; (declare-function foo-args nil (arg1 arg2))
;; (defvar foo-var)
;;
;;    Note, the forms will not be generated in files that contain the
;;    `exordium-require' but are merely loaded by the file as part of the
;;    compilation or evaluation.
;;
;;; Development guidelines:
;;
;; When developing Exordium, please follow these guidelines when "including"
;; functionality from other modules, packages, and libraries.
;;
;; For a given package X only a single Exordium module should contain form
;;
;; (use-package X
;;   ...
;;   )
;;
;; to install, load, and configure the package X.  The form may be repeated if
;; necessary, for example to allow for different configurations or to split
;; long configurations into functional parts.  This is dictated by the
;; difficulty of syncing different calls to `use-package' when these are spread
;; across different Exordium modules.  One example is the
;; `:diminish'/`:delight' which may be clobbered by later `use-package' that
;; doesn't specify it.
;;
;; The exception is when some package's `:autoload', `:command', `:hook',
;; `:custom' or `:bind' family with `:map' is required to implement
;; configuration in another package.  This effectively means that the former
;; package is embedded in the latter.  Make sure to `exordium-require' the
;; primary Exordium's module for the former package as well as use appropriate
;; `:ensure' (when referring to libraries or built-in packages) and add
;; `:defer' t to avoid reloading the former package.  For example:
;;
;; (exordium-require 'init-projectile)
;; ...
;; (use-package helm-projectile)
;;   ...
;;   :init
;;   ...
;;   (use-package projectile
;;     :defer t
;;     :autoload (projectile-switch-project-by-name)
;;     :bind
;;     (:map projectile-command-map
;;      ("p" . #'helm-projectile-switch-project)
;;      ("4 p" . #'exordium-projectile-switch-project-find-file-other-window))
;;   ...
;;   )
;;
;; For a non built-in package use the `use-package' form.  Exordium sets
;; `use-package-always-ensure' to t, so the `:ensure' is not necessary.  It may
;; be advisable to add `:defer' keyword when there are sufficient autoloads set
;; up by the package itself and it is not inferred from other keywords, which
;; see `use-package' Info node `(use-package) Deferring loading'.
;;
;; For a built-in package, that needs to be configured, for example to add
;; keybindings, change default values for variables, or any other extra
;; functionality, use `use-package' form and add `:ensure' nil to the form, for
;; example:
;;
;; (use-package emacs-lisp
;;   :ensure nil
;;   :bind
;;   ...
;;   )
;;
;;
;; For a built-in package, that needs to be shadowed (installation from ELPA
;; should be enforced) use `use-package' add `:exordium-force-elpa', for
;; example:
;;
;; (use-package org
;;   :exordium-force-elpa gnu
;;   :config
;;   ...
;; )
;;
;; For any other built-in package, that doesn't require any extra settings use
;; plain `require' form, for example:
;;
;; (require 'cl-lib)
;;
;; When the package X is required by an Exordium module and the package X
;; already has been loaded and configured in another Exordium module use
;; `exordium-require' to load the another Exordium module.  Sometimes it may be
;; necessary to add `:functions' and `:defines' to the `exordium-require' form
;; to silence compiler warnings. For example, the init-forge.el module requires
;; `magit' that is configured in init-git.el module, the former should use
;;
;; (exordium-require 'init-git)
;;
;; Usage of `exordium-require' for shadowed built-in packages is to ensure that
;; during compilation of a single Exordium module the ELPA version is used
;; consistently.
;;
;; Like the `use-package', `exordium-require' supports statistics gathering
;; when `use-package-compute-statistics' is non-nil.  Statistics are gathered
;; into internal `use-package' buffers, and can be reviewed with
;; `use-package-report' (which see).  Note that Exordium modules call
;; `use-package' when they are evaluated.  This causes their entries in the
;; report to have time for packages loaded by `use-package' loading/configuring
;; included.
;;
;;; Implementation details:
;;
;; When a FEATURE is loaded the `load' function is used (which see) with PATH
;; set to equivalent of:
;;
;; (expand-file-name (locate-user-emacs-file LOCATION)
;;                   FEATURE)
;;
;; Note that the filename extension is omitted to allow `load' to follow rules
;; for `load-suffixes' and `load-prefer-newer'.
;;
;; When macro is expanded for a compiled file errors are demoted while `load'
;; is executing.  This is to suppress errors from the loaded file propagating
;; to the caller.  Also the `package-archives' are set to nil (technically: a
;; value of the `exordium--require-package-archives' which is nil by default).
;; These measures are to allow for suppressing modules download for example
;; when running byte compilation as part of a lint process, for example like
;; `flycheck'.
;;
;; However, when a module is byte-compiled the
;; `exordium--require-package-archives' should be set to the desired value for
;; `package-archives', such that required packages can be installed from ELPAs.


;;; Code:
(require 'use-package nil t)
(require 'cl-lib)

(defvar exordium--require-package-archives nil
  "Package archives to be used when compiling.")

(defvar exordium--require-nesting-list nil
  "Record recursive calls of `exordium-require'.
From `require' implementation: A certain amount of recursive
require' is legitimate, but if we require the same feature
recursively 3 times, signal an error.")

(defun exordium--require-load (feature location &optional nomessage)
  "Load FEATURE from Exordium LOCATION.
The NOMESSAGE is passed to `load', which see.  This is a helper
for `exordium-require', which see."
  (let ((file (file-name-concat
               (expand-file-name (locate-user-emacs-file (or location
                                                             "modules")))
               (symbol-name feature))))
    (save-match-data
      (load file nomessage))))

;;;###autoload
(defmacro exordium-require (feature &rest options)
  "If FEATURE is not already loaded, load it from Exordium modules directory.
OPTIONS is a plist, with up to three properties: `:location',
`:functions', and `:defines'.

Like `require', but concatenate :location, that is relative in
Exordium project, with printname of FEATURE as a FILE argument passed to
`load' (which see).  Default :location is \"modules\".

To suppress compiler warnings use `:functions' and `:defines'
keywords in OPTIONS argument.  The `:functions' is a list where
each element either is a function FN or is a cons (FN ARGLIST)
that are passed to `declare-function' (which see).  Note that
`declare-function' forms has FILE argument set to nil.  The
`:defines' is a list where each element is a SYMBOL that is
passed to `defvar' (which see).

Please see Commentary in init-require.el for more details."
  (declare (debug t) (indent 1))
  (when-let* (((if (and (consp feature) (eq #'quote (car feature))
                        (consp (cdr feature)) (symbolp (cadr feature))
                        (not (keywordp (cadr feature)))
                        (not (cddr feature)))
                 ;; 'symbol is a (cons #'quote (cons symbol nil))
                 t (error "Wrong type argument: symbolp, %S" feature)))

              ((let ((location (plist-get options :location)))
                 (if (or (and (stringp location) (< 0 (length location)))
                         (and (symbolp location) (not location)))
                     ;; "string" is a string, nil is a symbol
                     t (error "Wrong type argument: stringp or nil, %S" location))))
              (declare-forms
               (or (append
                    (mapcar (lambda (fn)
                              (cond
                               ((and (symbolp fn) (not (memq fn '(nil t quote)))
                                     (not (keywordp fn)))
                                `(declare-function ,fn nil))
                               ((and (listp fn) (not (memq fn '(nil t quote)))
                                     (cl-every
                                      (lambda (elt)
                                        (and (symbolp elt)
                                             (not (memq elt '(nil t quote)))
                                             (not (keywordp elt))))
                                      fn))
                                `(declare-function ,(car fn) nil ,(cdr fn)))
                               (t
                                (error
                                 "Wrong type argument: symbolp or (and (consp) (symbolp car)), %S"
                                 fn))))
                            (plist-get options :functions))
                    (mapcar (lambda (var)
                              (if (and (symbolp var) (not (memq var '(nil t quote)))
                                       (not (string-prefix-p ":" (symbol-name var))))
                                  `(defvar ,var)
                                (error "Wrong type argument: symbolp, %S" var)))
                            (plist-get options :defines)))
                   t)))
    (append
     `(progn
        ,@(when (bound-and-true-p use-package-compute-statistics)
            `((use-package-statistics-gather :use-package ,feature nil))))
     (when (bound-and-true-p byte-compile-current-file)
       ;; Like in `use-package-normalize-keywords' for byte-compiling,
       ;; pre-load the package so all its symbols are in scope. With a few
       ;; extensions:
       ;;  - set `package-archives' to nil to prevent `use-package'
       ;;    `:ensure' in loaded files from downloading from ELPAs when
       ;;    compilation is run as a lint,
       ;;  - this is overridable by `exordium--require-package-archives' when
       ;;  - compilation is actually producing results,
       ;;  - use `eval-and-compile' such that functions and variables are brought
       ;;    into scope,
       ;;  - add `declare-functions' and `defvar' should the above not work.
       `((progn
             ,@(when (bound-and-true-p use-package-compute-statistics)
                 `((use-package-statistics-gather :preface ,feature nil)))
             (eval-and-compile
               (let ((package-archives (bound-and-true-p
                                        exordium--require-package-archives)))
                 (with-demoted-errors
                     ,(format "(exordium-require) Cannot load %s: %%S" feature)
                   (unless (featurep ,feature)
                     (exordium--require-load ,feature
                                             ,(plist-get options :location)
                                             t))))
               ,@(when (listp declare-forms)
                   declare-forms))
             ,@(when (bound-and-true-p use-package-compute-statistics)
                 `((use-package-statistics-gather :preface ,feature t))))))
     ;; Like in `require' handle errors and ensure everything is loaded while
     ;; eval.
     `(,@(when (bound-and-true-p use-package-compute-statistics)
           `((use-package-statistics-gather :config ,feature nil)))
       (prog1
           (if (featurep ,feature)
               ,feature
             (condition-case-unless-debug err
                 (unwind-protect
                     (if (< 3 (cl-count ,feature exordium--require-nesting-list))
                         (error
                          "Recursive `exordium-require' for feature `%s', require-nesting-list: %s"
                          ,feature exordium--require-nesting-list)
                       (push ,feature exordium--require-nesting-list)
                       (exordium--require-load ,feature
                                               ,(plist-get options :location))
                       (if (featurep ,feature)
                           ,feature
                         (if-let* ((file (caar load-history)))
                             (error "Loading file %s failed to provide feature `%s'"
                                    file ,feature)
                           (error "Required feature `%s' was not provided"
                                  ,feature))))
                   (when (eq ,feature (car exordium--require-nesting-list))
                     (pop exordium--require-nesting-list)))
               (error
                (display-warning 'exordium-require
                                 (format "%s: %s"
                                         ,feature (error-message-string err)))
                nil)))
         ,@(when (bound-and-true-p use-package-compute-statistics)
             `((use-package-statistics-gather :config ,feature t)))
         ,@(when (bound-and-true-p use-package-compute-statistics)
             `((use-package-statistics-gather :use-package ,feature t))))))))

(provide 'init-require)

;;; init-require.el ends here
