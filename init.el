;;; init.el --- Exordium init.el                     -*- lexical-binding: t -*-
;;; Commentary:
;;;;  ___      __   __   __
;;;; |__  \_/ /  \ |__) |  \ | |  |  |\/|
;;;; |___ / \ \__/ |  \ |__/ | \__/  |  |
;;;;
;;;; Emacs Makes All Computing Simple.

;;; Code:

;; Reduce the frequency of garbage collection by making it happen on
;; each 100MB of allocated data (the default is on every 0.76MB). This reduces
;; the startup time.
(setq gc-cons-threshold 100000000)

(let ((min-version "27.1"))
  (when (version< emacs-version min-version)
    (error "This config requires at least Emacs %s, but you're running %s"
           min-version emacs-version)))

(setq initial-scratch-message
      "EXORDIUM DID NOT LOAD CORRECTLY.
Check the warnings and messages buffers, or restart with --debug-init")

(defconst exordium-before-init "before-init.el"
  "Name of the before init file.")

(defconst exordium-prefs "prefs.el"
  "Name of the prefs file.")

(defconst exordium-after-init "after-init.el"
  "Name of the after init file.")

(defconst exordium-custom "emacs-custom.el"
  "Name of the customization file.")

;; Use this file for HTTP proxy settings if needed for packages.  Also add
;; additional packages to exordium-extra-packages for packages to be
;; automatically pulled from the elpa archives

(defconst exordium-before-init-file
  (expand-file-name (locate-user-emacs-file exordium-before-init))
  "Location of the master before init file.")

(defconst exordium-modules-dir
  (expand-file-name (locate-user-emacs-file "modules"))
  "Location of the modules directory.")

(defconst exordium-themes-dir
  (expand-file-name (locate-user-emacs-file "themes"))
  "Location of the themes directory.")

(defconst exordium-extensions-dir
  (expand-file-name (locate-user-emacs-file "extensions"))
  "Location of the extensions directory.")

(defconst exordium-local-dir
  (expand-file-name (locate-user-emacs-file "local"))
  "Location of the local directory.")

(defconst exordium-prefs-file
  (expand-file-name (locate-user-emacs-file exordium-prefs))
  "Location of the master prefs file.")

(defconst exordium-after-init-file
  (expand-file-name (locate-user-emacs-file exordium-after-init))
  "Location of the master after init file.")

(defconst exordium-custom-file
  (expand-file-name (locate-user-emacs-file exordium-custom))
  "Location of the customization file.")

;; Save any custom set variable in exordium-custom-file rather than at the end of init.el:
(setq custom-file exordium-custom-file)

(defcustom exordium-extra-packages ()
  "A list of additional packages to auto load from elpa repositories."
  :group 'exordium
  :type  '(repeat (symbol :tag "Package")))

(defcustom exordium-extra-pinned ()
  "An alist of additional packages locations to pin to.

Each element of the list is in the same form as in `package-pinned-packages'."
  :group 'exordium
  :type  '(alist :key-type (symbol :tag "Package")
                 :value-type (string :tag "Archive name")))

(defconst exordium-melpa-package-repo "https://melpa.org/packages/"
  "URL for packages repository.")

(defconst exordium-pinned-melpa-package-repo "https://melpa.org/packages/"
  "URL for pinned default packages.
Set to stable melpa.org if you want stable.")

(defconst exordium-gnu-package-repo "https://elpa.gnu.org/packages/"
  "URL for the GNU package repository.")

;; Taps definition of before and after files. These are loaded
;; after master 'before', 'after', and 'prefs' files

(defconst exordium-taps-root
  (expand-file-name (locate-user-emacs-file "taps"))
  "Location of the tapped directories.")

(defvar exordium-tapped-before-init-files ()
  "All tapped before init files, including master one.")

(defvar exordium-tapped-prefs-files ()
  "All tapped prefs files, including master one.")

(defvar exordium-tapped-after-init-files ()
  "All tapped after init files, including master one.")

(defun exordium--add-directory-tree-to-load-path (dir &optional ignore-if-absent)
  "Add DIR and all its subdirs to the load path.
Warn if DIR is not a directory and IGNORE-IF-ABSENT is nil."
  (cond ((file-directory-p dir)
         (add-to-list 'load-path dir)
         (let ((default-directory dir))
           (normal-top-level-add-subdirs-to-load-path)))
        ((not ignore-if-absent)
         (warn "Missing directory: %s" dir))))


(when (file-accessible-directory-p exordium-taps-root)
  (dolist (tap (nreverse (directory-files exordium-taps-root t "^[^\.].*")))
    (when (file-accessible-directory-p tap)
      (exordium--add-directory-tree-to-load-path tap) ;; @todo: remove?
      (when-let* ((tapped (file-name-concat tap exordium-before-init))
                  ((file-readable-p tapped)))
        (add-to-list 'exordium-tapped-before-init-files tapped))
      (when-let* ((tapped (file-name-concat tap exordium-prefs))
                  ((file-readable-p tapped)))
        (add-to-list 'exordium-tapped-prefs-files tapped))
      (when-let* ((tapped (file-name-concat tap exordium-after-init))
                  ((file-readable-p tapped)))
        (add-to-list 'exordium-tapped-after-init-files tapped)))))

(when (file-readable-p exordium-before-init-file)
  (add-to-list 'exordium-tapped-before-init-files exordium-before-init-file))

(when (file-readable-p exordium-prefs-file)
  (add-to-list 'exordium-tapped-prefs-files exordium-prefs-file))

(when (file-readable-p exordium-after-init-file)
  (add-to-list 'exordium-tapped-after-init-files exordium-after-init-file))

(when (file-readable-p exordium-custom-file)
  (add-to-list 'exordium-tapped-after-init-files exordium-custom-file))

;; @todo: remove?
;;; Path for "require"
(add-to-list 'load-path exordium-modules-dir)
(exordium--add-directory-tree-to-load-path exordium-extensions-dir) ;; @todo third party
(exordium--add-directory-tree-to-load-path exordium-themes-dir)
(exordium--add-directory-tree-to-load-path exordium-local-dir t)


;; Load before init files
(dolist (tapped-file exordium-tapped-before-init-files)
  (message "Loadding tapped before-init file: %s" tapped-file)
  (load (file-name-sans-extension tapped-file)))


;;; Packages from Melpa
;; Use M-x `package-refresh-contents' to update the cache.
;; Use M-x `package-list-package' to load and display the list of packages,
;; then press I to mark for installation and X to execute (it's like `dired').

;; Initialize the package system
(require 'seq)
(require 'package)
(setq package-install-upgrade-built-in t)
(when (or (not (string= exordium-melpa-package-repo
                        exordium-pinned-melpa-package-repo))
          (seq-filter (lambda (pkg)
                        (string= "melpa" (cdr pkg)))
                      exordium-extra-pinned))
  (add-to-list 'package-archives
               (cons "melpa" exordium-melpa-package-repo) t))
(add-to-list 'package-archives
             (cons "melpa-pinned" exordium-pinned-melpa-package-repo) t)
(unless (seq-filter (lambda (repo)
                      (string= exordium-gnu-package-repo (cdr repo)))
                    package-archives)
  (add-to-list 'package-archives
               (cons "gnu" exordium-gnu-package-repo) t))

(setq package-user-dir
      (locate-user-emacs-file (concat "elpa-" emacs-version)))

(when (fboundp 'native-comp-available-p)
  (setq package-native-compile (native-comp-available-p)))

(package-initialize)

;; Load the packages we need if they are not installed already
(let ((package-pinned-packages (append
                                '((use-package             . "gnu")
                                  (diminish                . "gnu")
                                  (bind-key                . "melpa-pinned"))
                                exordium-extra-pinned))
      has-refreshed)
  (mapc (lambda (pkg)
          (unless (package-installed-p pkg)
            (unless has-refreshed
              (message "Refreshing package database...")
              (package-refresh-contents)
              (setq has-refreshed t)
              (message "Done."))
            (package-install pkg)))
        (append (mapcar #'car package-pinned-packages)
                exordium-extra-packages)))

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

;; This is only needed once, near the top of the file
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (require 'use-package))
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant

;; Ensure use-package and diminish are from gnu
(use-package-pin-package 'use-package 'gnu)
(use-package-pin-package 'diminish 'gnu)

(require 'use-package-ensure)
(setq use-package-always-ensure t)
(setq use-package-compute-statistics t)

(defun exordium--ignore-builtin (pkg)
  "Remove the PKG from the builtin list so it can be upgraded."
  (assq-delete-all pkg package--builtins)
  (assq-delete-all pkg package--builtin-versions))


;;; Load Modules
(require 'bytecomp)
(defun exordium-recompile-modules ()
  "Recompile modules.
For which the .elc is older than the .el, if the .elc exists.
Also discard .elc without corresponding .el."
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
(exordium-recompile-modules)

(eval-and-compile
  (load (file-name-concat (locate-user-emacs-file "modules") "init-require")))

(exordium-require 'init-lib)              ; utility functions - load this first

(exordium-require 'init-environment)      ; environment variables

;;; Local preferences (fonts, frame size etc.)
(exordium-require 'init-prefs)            ; defines variables that prefs.el can override

(dolist (tapped-file exordium-tapped-prefs-files)
  (message "Loadding tapped prefs file: %s" tapped-file)
  (load (file-name-sans-extension tapped-file)))

;;; Themes
;;; Note: use "export TERM=xterm-256color" for emacs -nw
(exordium-require 'init-progress-bar nil)

(when exordium-nw
  (set-face-background 'highlight nil))
(when exordium-theme
  (exordium-require 'init-themes))

;;; Look and feel
(exordium-require 'init-look-and-feel)    ; fonts, UI, keybindings, saving files etc.
(exordium-require 'init-font-lock)        ; enables/disables font-lock globally.
(exordium-require 'init-linum)            ; line numbers
(when exordium-smooth-scroll
  (exordium-require 'init-smooth-scroll nil)
  (smooth-scroll-mode 1)) ; smooth scroll

(update-progress-bar)

;;; Usability
(exordium-require 'init-window-manager)  ; navigate between windows
(exordium-require 'init-util)            ; utilities like match paren, bookmarks...
(unless exordium-helm-everywhere
  (exordium-require 'init-ido))          ; supercharged completion engine
(exordium-require 'init-highlight)       ; highlighting current line, symbol under point

(pcase exordium-complete-mode
  (:auto-complete
   (exordium-require 'init-autocomplete))
  (:company
   (exordium-require 'init-company)))    ; completion

(when exordium-helm-projectile
  (exordium-require 'init-helm-projectile))

(exordium-require 'init-helm)             ; setup helm

(when exordium-help-extensions
  (exordium-require 'init-help))           ; extra help

(update-progress-bar)

(exordium-require 'init-dired)            ; enable dired+ and wdired permission editing
(exordium-require 'init-git)              ; Magit and git gutter
(exordium-require 'init-git-visit-diffs)  ; visit diffs in successive narrowed buffers
(exordium-require 'init-forge)            ; Forge
(exordium-require 'init-flb-mode)         ; frame-local buffers

(update-progress-bar)

;;; Prog mode
(exordium-require 'init-prog-mode )

;;; Shell mode
(exordium-require 'init-prog-mode)

;;; Major modes
(exordium-require 'init-markdown)
(exordium-require 'init-org)
(exordium-require 'init-xml)

;;; OS-specific things
(when exordium-osx
  (exordium-require 'init-osx))

;;; C++
(exordium-require 'init-cpp)
(exordium-require 'init-bde-style)
(when exordium-yasnippet
  (exordium-require 'init-yasnippet))
(exordium-require 'init-gdb)

;;; RTags
(exordium-require 'init-rtags nil
                  :functions (rtags-auto-complete))
(when (and (eq exordium-complete-mode :auto-complete)
           exordium-rtags-auto-complete)
  (rtags-auto-complete))
(exordium-require 'init-rtags-helm)
(exordium-require 'init-rtags-cmake)
(exordium-require 'init-rtags-cdb)

(update-progress-bar)

;;; JS
(exordium-require 'init-javascript)

;;; Python
(exordium-require 'init-python)

;;; Ruby
(exordium-require 'init-ruby)

;;; Lisp
(exordium-require 'init-elisp)

;;; Clojure
(when exordium-clojure
  (exordium-require 'init-clojure))

;;; Groovy
(exordium-require 'init-groovy)

;;; include-what-you-use
(exordium-require 'init-iwyu)

(update-progress-bar)

;;; Desktop - close to the end so customisations had a chance to kick in
(when exordium-desktop
  (exordium-require 'init-desktop))

(when (and exordium-theme exordium-enable-powerline)
  (exordium-require 'init-powerline))

;; Docker
(exordium-require 'init-docker)

;; Flycheck
(exordium-require 'init-flycheck)

;;; Treesit
(exordium-require 'init-treesit)

;;; LSP
(when exordium-lsp-mode-enable
  (exordium-require 'init-lsp))

;;; Local extensions
(dolist (tapped-file exordium-tapped-after-init-files)
  (message "Loadding tapped after-init file: %s" tapped-file)
  (load (file-name-sans-extension tapped-file)))

(update-progress-bar)

;;; Greetings
(setq initial-scratch-message
      (let ((current-user (split-string (user-full-name) " ")))
        (format ";; -*- lexical-binding: t -*-

;; Happy hacking %s!

" (if current-user (car current-user) exordium-current-user))))


(provide 'init)

;;; init.el ends here
