;;;;
;;;; ███████╗ ██╗  ██╗  ██████╗  ██████╗  ██████╗  ██╗ ██╗   ██╗ ███╗   ███╗
;;;; ██╔════╝ ╚██╗██╔╝ ██╔═══██╗ ██╔══██╗ ██╔══██╗ ██║ ██║   ██║ ████╗ ████║
;;;; █████╗    ╚███╔╝  ██║   ██║ ██████╔╝ ██║  ██║ ██║ ██║   ██║ ██╔████╔██║
;;;; ██╔══╝    ██╔██╗  ██║   ██║ ██╔══██╗ ██║  ██║ ██║ ██║   ██║ ██║╚██╔╝██║
;;;; ███████╗ ██╔╝ ██╗ ╚██████╔╝ ██║  ██║ ██████╔╝ ██║ ╚██████╔╝ ██║ ╚═╝ ██║
;;;; ╚══════╝ ╚═╝  ╚═╝  ╚═════╝  ╚═╝  ╚═╝ ╚═════╝  ╚═╝  ╚═════╝  ╚═╝     ╚═╝
;;;;
;;;; Emacs Makes All Computing Simple.

(let ((min-version "24.1"))
  (when (version< emacs-version min-version)
    (error "This config requires at least Emacs %s, but you're running %s"
           min-version emacs-version)))

(defconst exordium-before-init "before-init.el"
  "name of the before init file")

(defconst exordium-prefs "prefs.el"
  "name of the prefs file")

(defconst exordium-after-init "after-init.el"
  "name of the after init file")

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

(defcustom exordium-extra-packages '()
  "Additional packages to auto load from elpa repositories"
    :group 'init
    :type 'list)

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

(when (file-accessible-directory-p exordium-taps-root)
  (dolist (tap (nreverse (directory-files exordium-taps-root t "^[^\.][^\.]?*+")))
    (when (file-accessible-directory-p tap)
      (setq tapped (concat (file-name-as-directory tap) exordium-before-init))
      (when (file-readable-p tapped)
        (add-to-list 'exordium-tapped-before-init-files tapped))
      (setq tapped (concat (file-name-as-directory tap) exordium-prefs))
      (when (file-readable-p tapped)
        (add-to-list 'exordium-tapped-prefs-files tapped))
      (setq tapped (concat (file-name-as-directory tap) exordium-after-init))
      (when (file-readable-p tapped)
        (add-to-list 'exordium-tapped-after-init-files tapped)))))
(when (file-readable-p exordium-before-init-file)
  (add-to-list 'exordium-tapped-before-init-files exordium-before-init-file))
(when (file-readable-p exordium-prefs-file)
  (add-to-list 'exordium-tapped-prefs-files exordium-prefs-file))
(when (file-readable-p exordium-after-init-file)
  (add-to-list 'exordium-tapped-after-init-files exordium-after-init-file))

;; Load before init files
(dolist (tapped-file exordium-tapped-before-init-files)
  (load tapped-file))


;;; Packages from Melpa
;; Use M-x `package-refresh-contents' to update the cache.
;; Use M-x `package-list-package' to load and display the list of packages,
;; then press I to mark for installation and X to execute (it's like dired).

;; Initialize the package system
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; Load the packages we need if they are not installed already
(let ((required-packages (append '(diminish
                                   highlight-symbol
                                   magit
                                   git-timemachine
                                   git-gutter
                                   git-gutter-fringe
                                   expand-region
                                   auto-complete
                                   company
                                   rtags
                                   auto-complete-c-headers
                                   yasnippet
                                   js2-mode
                                   ac-js2
                                   iedit
                                   cider
                                   clojure-mode
                                   paredit
                                   rainbow-delimiters
                                   helm
                                   helm-descbinds
                                   helm-swoop
                                   ido-ubiquitous
                                   projectile
                                   helm-projectile
                                   ;; ack-and-a-half
                                   cmake-mode
                                   markdown-mode
                                   enh-ruby-mode
                                   fill-column-indicator
                                   exec-path-from-shell
                                   goto-chg
                                   project-explorer
                                   page-break-lines)
                                 exordium-extra-packages))
      (has-refreshed nil))
  (dolist (p required-packages)
    (unless (package-installed-p p)
      (unless has-refreshed
        (message "Refreshing package database...")
        (package-refresh-contents)
        (setq has-refreshed t)
        (message "Done."))
      (package-install p))))


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

(setq custom-theme-directory exordium-themes-dir)


;;; Load Modules
(require 'bytecomp)
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

(require 'init-lib)         ; utility functions - load this first
(require 'init-environment) ; environment variables

;;; Local preferences (fonts, frame size etc.)
(require 'init-prefs)       ; defines variables that prefs.el can override
(dolist (tapped-file exordium-tapped-prefs-files)
  (load tapped-file))

;;; Desktop
(when exordium-desktop
  (require 'init-desktop))

;;; Look and feel
(require 'init-look-and-feel)   ; fonts, UI, keybindings, saving files etc.
(require 'init-font-lock)       ; enables/disables font-lock globally.
(require 'init-linum)           ; line numbers

;;; Usability
(require 'init-window-manager)  ; navigate between windows
(require 'init-util)            ; utilities like match paren, bookmarks...
(require 'init-ido)             ; supercharged completion engine
(when exordium-auto-complete
  (require 'init-autocomplete)) ; auto-completion (see below for RTags AC)
(when exordium-helm-projectile  ; find files anywhere in project
  (require 'init-helm-projectile))

(require 'init-dired)           ; enable dired+ and wdired permission editing

;;; Magit and git gutter
(require 'init-git)

;;; Themes
;;; Note: use "export TERM=xterm-256color" for emacs -nw
(when exordium-nw
  (set-face-background 'highlight nil))
(when exordium-theme
  (require 'init-themes)
  (when exordium-enable-powerline
    (require 'init-powerline)))

;;; Shell mode
(require 'init-shell)

;;; Major modes
(require 'init-markdown)
(require 'init-org)
(require 'init-xml)

;;; OS-specific things
(when exordium-osx
  (require 'init-osx))

;;; C++
(require 'init-cpp)
(require 'init-bde-style)
(when exordium-yasnippet
  (require 'init-yasnippet))
(require 'init-rtags)
(when exordium-rtags-auto-complete
  (rtags-auto-complete))
(require 'init-rtags-helm)

;;; JS
(require 'init-javascript)

;;; Python
(require 'init-python)

;;; Ruby
(require 'init-ruby)

;;; Lisp
(require 'init-elisp)

;;; Clojure
(when exordium-clojure
  (require 'init-clojure))

;;; Local extensions
(dolist (tapped-file exordium-tapped-after-init-files)
  (load tapped-file))

;;; Greetings
(setq initial-scratch-message
      (let ((current-user (split-string (user-full-name) " ")))
        (format ";; Happy hacking %s!

" (if current-user (car current-user) exordium-current-user))))

;;; End of file
