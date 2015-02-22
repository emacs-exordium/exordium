;;;; Philippe's
;;;;    ___ _ __ ___   __ _  ___ ___
;;;;   / _ \ '_ ` _ \ / _` |/ __/ __|
;;;;  |  __/ | | | | | (_| | (__\__ \
;;;; (_)___|_| |_| |_|\__,_|\___|___/
;;;;
;;;; Emacs Makes All Computing Simple.

(let ((min-version "24.1"))
  (when (version< emacs-version min-version)
    (error "This config requires at least Emacs %s, but you're running %s"
           min-version emacs-version)))

;; Use this file for HTTP proxy settings if needed, for packages.
(when (file-exists-p "~/.emacs.d/init-local-prolog.el")
  (warn "init-local-prolog.el is deprecated, use before-init.el")
  (load "~/.emacs.d/init-local-prolog.el"))
(when (file-exists-p "~/.emacs.d/before-init.el")
  (load "~/.emacs.d/before-init.el"))


;;; Packages from Melpa
;; Use M-x `package-refresh-contents' to update the cache.
;; Use M-x `package-list-package' to load and display the list of packages,
;; then press I to mark for installation and X to execute (it's like dired).

;; Initialize the package system
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; Load the packages we need if they are not installed already
(let ((required-packages '(diminish
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
                           ido-ubiquitous
                           projectile
                           helm-projectile
                           ack-and-a-half
                           cmake-mode
                           markdown-mode
                           enh-ruby-mode
                           fill-column-indicator
                           exec-path-from-shell
                           goto-chg
                           project-explorer
                           page-break-lines))
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

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(defun add-directory-tree-to-load-path (dir)
  "Add 'dir' and all its subdirs to the load path"
  (add-to-list 'load-path dir)
  (let ((default-directory dir))
    (normal-top-level-add-subdirs-to-load-path)))

(add-directory-tree-to-load-path "~/.emacs.d/extensions/")
(add-directory-tree-to-load-path "~/.emacs.d/themes/")

(setq custom-theme-directory "~/.emacs.d/themes/")


;;; Load Modules
(require 'bytecomp)
(defun recompile-modules ()
  "Recompile modules for which the .elc is older than the .el, if
the .elc exists."
  (interactive)
  (dolist (dir '("~/.emacs.d/modules" "~/.emacs.d/themes" "~/.emacs.d/extensions"))
    (when (file-directory-p dir)
      (dolist (el (directory-files dir t "\\.el$"))
        (let ((elc (byte-compile-dest-file el)))
          (when (and (file-exists-p elc)
                     (file-newer-than-file-p el elc))
            (byte-compile-file el)))))))
(recompile-modules)

(require 'init-lib)         ; utility functions - load this first
(require 'init-environment) ; environment variables

;;; Local preferences (fonts, frame size etc.)
(require 'init-prefs)       ; defines variables that prefs.el can override
(when (file-exists-p "~/.emacs.d/init-local-prefs.el")
  (warn "init-local-prefs.el is deprecated, use prefs.el instead")
  (load "~/.emacs.d/init-local-prefs.el"))
(when (file-exists-p "~/.emacs.d/prefs.el")
  (load "~/.emacs.d/prefs.el"))

;;; Look and feel
(require 'init-ui)          ; fonts, menubar, syntax highlighting etc.
(require 'init-linum)       ; line numbers
(require 'init-behavior)    ; save behavior: backup files, trailing spaces...
(require 'init-keyboard)    ; key bindings
(require 'init-util)        ; utilities like match paren, bookmarks...

;;; Usability
(require 'init-ido)
(require 'init-autocomplete)
(when *init-helm-projectile*
  (require 'init-helm-projectile))
(require 'init-git)

;;; Themes
(when *environment-nw*
  (set-face-background 'highlight nil))
(when (and (not *environment-nw*) *init-theme*)
  (require 'init-themes)
  (when *init-enable-powerline*
    (require 'init-powerline)))

;;; Shell mode
(require 'init-shell)

;;; Major modes
(require 'init-markdown)
(require 'init-org)
(require 'init-xml)

;;; OS-specific things
(when *environment-osx*
  (require 'init-osx))

;;; C++
(require 'init-cpp)
(require 'init-bde-style)

(when *init-yasnippet*
  (require 'init-yasnippet))

(require 'init-rtags)
(when *init-rtags-auto-complete*
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
(when *init-clojure*
  (require 'init-clojure))

;;; Local extensions
(when (file-exists-p "~/.emacs.d/init-local.el")
  (warn "init-local.el is deprecated, use after-init.el")
  (load "~/.emacs.d/init-local.el"))
(when (file-exists-p "~/.emacs.d/after-init.el")
  (load "~/.emacs.d/after-init.el"))

;;; Greetings
(setq initial-scratch-message
      (format ";; Happy hacking %s!

" *environment-current-user*))
