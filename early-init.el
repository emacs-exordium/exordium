;;; early-init.el --- Early Init -*- no-byte-compile: t; lexical-binding: t; -*-

(let ((min-version "27.1"))
  (when (version< emacs-version min-version)
    (error "This config requires at least Emacs %s, but you're running %s"
           min-version emacs-version)))

(setq initial-scratch-message
      "EXORDIUM DID NOT LOAD CORRECTLY.
Check the warnings and messages buffers, or restart with --debug-init")

(defconst exordium-before-init "before-init.el"
  "name of the before init file")

(defconst exordium-prefs "prefs.el"
  "name of the prefs file")

(defconst exordium-after-init "after-init.el"
  "name of the after init file")

(defconst exordium-custom "emacs-custom.el"
  "name of the customization file")

;; Use this file for HTTP proxy settings if needed for packages.  Also add
;; additional packages to exordium-extra-packages for packages to be
;; automatically pulled from the elpa archives

(defconst exordium-before-init-file (locate-user-emacs-file exordium-before-init)
  "location of the master before init file")

(defconst exordium-modules-dir (locate-user-emacs-file "modules")
  "location of the modules directory")
(defconst exordium-themes-dir (locate-user-emacs-file "themes")
  "location of the themes directory")
(defconst exordium-extensions-dir (locate-user-emacs-file "extensions")
  "location of the extensions directory")
(defconst exordium-local-dir (locate-user-emacs-file "local")
  "location of the local directory")

(defconst exordium-prefs-file (locate-user-emacs-file exordium-prefs)
  "location of the master prefs file")

(defconst exordium-after-init-file (locate-user-emacs-file exordium-after-init)
  "location of the master after init file")

(defconst exordium-custom-file (locate-user-emacs-file exordium-custom)
  "location of the customization file")

;; Save any custom set variable in exordium-custom-file rather than at the end of init.el:
(setq custom-file exordium-custom-file)

(defcustom exordium-extra-packages ()
  "A list of additional packages to auto load from elpa repositories."
    :group 'exordium
    :type  'list)

(defcustom exordium-extra-pinned ()
  "An alist of additional packages locations to pin to.

Each element of the list is in the same form as in `package-pinned-packages'."
  :group 'exordium
  :type  'alist)

;; Taps definition of before and after files. These are loaded
;; after master 'before', 'after', and 'prefs' files

(defconst exordium-taps-root (locate-user-emacs-file "taps")
  "location of the tapped directories")

(defconst exordium-tapped-before-init-files ()
  "all tapped before init files, including master")

(defconst exordium-tapped-prefs-files ()
  "all tapped prefs files, including master")

(defconst exordium-tapped-after-init-files ()
  "all tapped after init files, including master")

(defconst exordium-melpa-package-repo "https://melpa.org/packages/"
  "URL for packages repository")

(defconst exordium-pinned-melpa-package-repo "https://melpa.org/packages/"
  "URL for pinned default packages. Set to stable melpa.org if you want stable")

(defconst exordium-gnu-package-repo "https://elpa.gnu.org/packages/"
  "URL for the GNU package repository")

(defvar exordium-early-debug nil
  "Non-nil to enable debug.")

(defvar exordium-gc-threshold (* 16 1024 1024)
  "The value of `gc-cons-threshold' after Emacs startup.")

(defvar exordium-early-user-directory user-emacs-directory
  "The default value of the `user-emacs-directory' variable.")

;;; Misc

(set-language-environment "UTF-8")

;; Set-language-environment sets default-input-method, which is unwanted.
(setq default-input-method nil)

;;; Garbage collection
;; Garbage collection significantly affects startup times. This setting delays
;; garbage collection during startup but will be reset later.

(setq gc-cons-threshold most-positive-fixnum)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold exordium-gc-threshold)))

;; Prefer loading newer compiled files
(setq load-prefer-newer t)

;; Increase how much is read from processes in a single chunk (default is 4kb).
(setq read-process-output-max (* 512 1024))  ; 512kb

;; Reduce rendering/line scan work by not rendering cursors or regions in
;; non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; Disable warnings from the legacy advice API. They aren't useful.
(setq ad-redefinition-action 'accept)

(setq warning-suppress-types '((lexical-binding)))

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; By default, Emacs "updates" its ui more often than it needs to
(setq idle-update-delay 1.0)

;; Font compacting can be very resource-intensive, especially when rendering
;; icon fonts on Windows. This will increase memory usage.
(setq inhibit-compacting-font-caches t)


;; Allow for shorter responses: "y" for yes and "n" for no.
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (advice-add #'yes-or-no-p :override #'y-or-n-p))
(defalias #'view-hello-file #'ignore)  ; Never show the hello file


(require 'seq)
(require 'package)

;;; package.el
(setq package-enable-at-startup nil)
(setq package-quickstart nil)
(setq use-package-always-ensure t)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(setq package-user-dir (concat "~/.emacs.d/elpa-" emacs-version))

(when (fboundp 'native-comp-available-p)
 (setq package-native-compile (native-comp-available-p)))

(package-initialize)

;; Load the packages we need if they are not installed already
(let ((package-pinned-packages exordium-extra-pinned)
      (has-refreshed nil))

  (defun update-package (p  has-refreshed)
    (unless (package-installed-p p)
      (unless has-refreshed
        (message "Refreshing package database...")
        (package-refresh-contents)
        (setq has-refreshed t)
        (message "Done."))
      (package-install p)))

  (dolist (pkg package-pinned-packages)
    (let ((p (car pkg)))
      (update-package p has-refreshed)))

  (dolist (pkg exordium-extra-packages)
    (update-package pkg has-refreshed)))


(customize-set-variable 'package-archive-priorities '(("gnu"    . 99)
                                                      ("nongnu" . 80)
                                                      ("stable" . 70)
                                                      ("melpa"  . 0)))


(when (file-accessible-directory-p exordium-taps-root)
  (dolist (tap (nreverse (directory-files exordium-taps-root t "^[^\.][^\.]?*+")))
    (when (file-accessible-directory-p tap)
      (let ((tapped (concat (file-name-as-directory tap) exordium-before-init)))
        (when (file-readable-p tapped)
          (add-to-list 'exordium-tapped-before-init-files tapped))
        (setq tapped (concat (file-name-as-directory tap) exordium-prefs))
        (when (file-readable-p tapped)
          (add-to-list 'exordium-tapped-prefs-files tapped))
        (setq tapped (concat (file-name-as-directory tap) exordium-after-init))
        (when (file-readable-p tapped)
          (add-to-list 'exordium-tapped-after-init-files tapped))))))

(when (file-readable-p exordium-before-init-file)
  (add-to-list 'exordium-tapped-before-init-files exordium-before-init-file))

(when (file-readable-p exordium-prefs-file)
  (add-to-list 'exordium-tapped-prefs-files exordium-prefs-file))

(when (file-readable-p exordium-after-init-file)
  (add-to-list 'exordium-tapped-after-init-files exordium-after-init-file))

(when (file-readable-p exordium-custom-file)
  (add-to-list 'exordium-tapped-after-init-files exordium-custom-file))

;; Load before init files
(dolist (tapped-file exordium-tapped-before-init-files)
  (load tapped-file))



;; - Some packages (i.e., magit, forge) require seq-2.24.
;; - Emacs-29.1 is delivered with seq-2.23.
;; - Other packages (i.e., compat) require seq-2.23.
;; - When only magit is installed it requires compat which requires seq-2.23 -> seq is not upgraded
;; - When only forge is installed is requires magit and compat which requires seq-2.23 -> seq is not upgraded
;; - When magit is installed followed by installation of forge seq is upgraded to seq-2.24 -> this fails
;; Force installing the freshest version of seq with errors suppressed:
(when (version< emacs-version "29.2")
  (let (debug-on-error)
    ;; this assumes `package-refresh-contents has been called'
    (package-install (car (alist-get 'seq package-archive-contents)))))

;;; Path for "require"

(add-to-list 'load-path exordium-modules-dir)

(defun add-directory-tree-to-load-path (dir &optional ignore-if-absent)
  "Add DIR and all its subdirs to the load path."
  (cond ((file-directory-p dir)
         (add-to-list 'load-path dir)
         (let ((default-directory dir))
           (normal-top-level-add-subdirs-to-load-path)))
        ((not ignore-if-absent)
         (warn "Missing directory: %s" dir))))

(add-directory-tree-to-load-path exordium-extensions-dir)
(add-directory-tree-to-load-path exordium-themes-dir)
(add-directory-tree-to-load-path exordium-local-dir t)

(add-directory-tree-to-load-path exordium-taps-root)

(setq custom-theme-directory exordium-themes-dir)


;;; remove a package from the builtin list so it can be upgraded
(defun exordium-ignore-builtin (pkg)
  (assq-delete-all pkg package--builtins)
  (assq-delete-all pkg package--builtin-versions))


;;; Load Modules
(use-package bytecomp :ensure nil)
(defun recompile-modules ()
  "Recompile modules for which the .elc is older than the .el, if
the .elc exists. Also discard .elc without corresponding .el"
  (interactive)
  (dolist (dir (list exordium-modules-dir
                     exordium-themes-dir
                     exordium-extensions-dir
                     exordium-local-dir))
    (when (file-directory-p dir)
      ;; Recompile
      (dolist (el (directory-files dir t "\\.el$"))
        (let ((elc (byte-compile-dest-file el)))
          (when (and (file-exists-p elc)
                     (file-newer-than-file-p el elc))
            (byte-compile-file el))))
      ;; Discard .elc singletons
      (dolist (elc (directory-files dir t "\\.elc$"))
        (let ((el (concat (concat (file-name-sans-extension elc) ".el"))))
          (unless (file-exists-p el)
            (warn "Removing singleton .elc file: %s" elc)
            (delete-file elc)))))))
(recompile-modules)

(use-package init-lib :ensure nil)         ; utility functions - load this first
(use-package init-environment :ensure nil) ; environment variables

;;; Local preferences (fonts, frame size etc.)
(use-package init-prefs :ensure nil)       ; defines variables that prefs.el can override
(dolist (tapped-file exordium-tapped-prefs-files)
  (load tapped-file))

(provide 'early-init)

;;; early-init.el ends here
