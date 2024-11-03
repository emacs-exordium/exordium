;;; init-require.el --- Exordium require/loading library -*- lexical-binding: t -*-

;;; Commentary:
;; The `exoridum-require' is a substitute to the built-in require, to be used
;; in the Exordium.
;; @todo: how to use

;;; Code:
(defvar exordium--require-nesting-list nil
  "Record recursive calls of `exordium-require'.
From `require' implementation: A certain amount of recursive
require' is legitimate, but if we require the same feature
recursively 3 times, signal an error.")

(defmacro exordium--require-load (feature location &optional nomessage)
  "Load FEATURE from Exordium LOCATION.
The NOMESSAGE is passed to `load', which see.  This is a helper
for `exordium-require', which see."
  `(let ((file (file-name-concat
                (expand-file-name (locate-user-emacs-file (or ,location
                                                              "modules")))
                (symbol-name ,feature))))
     (save-match-data
       (load file ,nomessage))))

;;;###autoload
(defmacro exordium-require (feature &optional location &rest declarations)
  "If FEATURE is not already loaded, load it from Exordium LOCATION.
Like `require', but concatenate LOCATION, that is relative in
Exordium project, with printname of FEATURE as FILE passed to
`load', which see.

To suppress compiler warnings use DECLARATIONS which is a plist
with two properties: :functions and :variables.  The :functions
is a list where each element either is a function FN or is a
cons (FN ARGLIST) that are passed to `declare-function' (which
see).  The :variables is a list where each element is a SYMBOL
that is passed to `defvar' (which see)."
  (if (bound-and-true-p byte-compile-current-file)
      ;; Like in `use-package-normalize-keywords', when byte-compiling,
      ;; pre-load the package so all its symbols are in scope. With a few
      ;; extensions:
      ;; - set `package-archives' to nil to prevent `use-package'
      ;;   :ensure from downloading from ELPAs,
      ;; - use `eval-and-compile' such that function names are autoloaded,
      ;; - handle `declare-functions', should the above not work.
      (let ((declare-forms
             (append
              (when-let* ((fns (plist-get declarations :functions)))
               (cons '(require 'loadhist)
                     (mapcar
                      (lambda (fn)
                        (cond
                         ((symbolp fn)
                          `(declare-function ,fn nil))
                         ((and (consp fn) (symbolp (car fn)))
                          `(declare-function ,(car fn) nil ,(cdr fn)))
                         (t
                          (error
                           "Wrong type argument: symbolp or (and consp (symbolp car)), %S"
                           fn))))
                      fns)))
              (mapcar (lambda (var)
                        (if (symbolp var)
                            `(defvar ,var)
                          (error "Wrong type argument: symbolp, %S" var)))
                      (plist-get declarations :variables)))))
        `(eval-and-compile
           (let (package-archives)
             (with-demoted-errors
                 ,(format "(exordium-require) Cannot load %s: %%S" feature)
               (unless (featurep ,feature)
                 (exordium--require-load ,feature ,location t))))
           ,@declare-forms))
    ;; Like in `require' handle errors and ensure everything is loaded when
    ;; in runtime.
    `(if (and (symbolp ,feature)
              (or (not ,location)
                  (stringp ,location)))
         (if (featurep ,feature)
             ,feature
           (unwind-protect
               (if (< 3 (cl-count ,feature exordium--require-nesting-list))
                   (error
                    "Recursive `exordium-require' for feature `%s', require-nesting-list: %s"
                    ,feature exordium--require-nesting-list)
                 (push ,feature exordium--require-nesting-list)
                 (exordium--require-load ,feature ,location)
                 (if (featurep ,feature)
                     ,feature
                   (if-let* ((file (caar load-history)))
                       (error "Loading file %s failed to provide feature `%s'"
                              file ,feature)
                     (error "Required feature `%s' was not provided"
                            ,feature))))
             (when (eq ,feature (car exordium--require-nesting-list))
               (pop exordium--require-nesting-list))))
       (if (symbolp ,feature)
           (error "Wrong type argument: symbolp, %S" ,feature)
         (error "Wrong type argument: stringp, %S" ,location)))))

(provide 'init-require)

;;; init-require.el ends here
