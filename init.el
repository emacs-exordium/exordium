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

(setq init-local-prolog-file (locate-user-emacs-file "init-local-prolog.el"))
(setq init-before-init-file (locate-user-emacs-file "before-init.el"))
(setq init-modules-dir (locate-user-emacs-file "modules"))
(setq init-themes-dir (locate-user-emacs-file "themes"))
(setq init-extensions-dir (locate-user-emacs-file "extensions"))
(setq init-local-dir (locate-user-emacs-file "local"))

(setq init-local-prefs-file (locate-user-emacs-file "init-local-prefs.el"))
(setq init-prefs-file (locate-user-emacs-file "prefs.el"))

(setq init-local-file (locate-user-emacs-file "init-local.el"))
(setq init-after-init-file (locate-user-emacs-file "after-init.el"))

;; Use this file for HTTP proxy settings if needed, for packages.
(when (file-exists-p init-local-prolog-file)
  (warn "init-local-prolog.el is deprecated, use before-init.el")
  (load init-local-prolog-file))
(when (file-exists-p init-before-init-file)
  (load init-before-init-file))


;;; Packages from Melpa
;; Use M-x `package-refresh-contents' to update the cache.
;; Use M-x `package-list-package' to load and display the list of packages,
;; then press I to mark for installation and X to execute (it's like dired).


(setq url-proxy-services
      '(("no_proxy" . "^.*\\(bloomberg\\)\.\\(com\\|net\\)")
        ("http"     . "devproxy.bloomberg.com:82")
        ("https"    . "devproxy.bloomberg.com:82")))


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
(add-to-list 'load-path (expand-file-name "local" user-emacs-directory))

(defun add-directory-tree-to-load-path (dir)
  "Add 'dir' and all its subdirs to the load path"
  (add-to-list 'load-path dir)
  (let ((default-directory dir))
    (normal-top-level-add-subdirs-to-load-path)))

(add-directory-tree-to-load-path init-extensions-dir)
(add-directory-tree-to-load-path init-themes-dir)

(setq custom-theme-directory init-themes-dir)


;;; Load Modules
(require 'bytecomp)
(defun recompile-modules ()
  "Recompile modules for which the .elc is older than the .el, if
the .elc exists. Also discard .elc without corresponding .el"
  (interactive)
  (dolist (dir (list init-modules-dir
                     init-themes-dir
                     init-extensions-dir
                     init-local-dir))
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
(when (file-exists-p init-local-prefs-file)
  (warn "init-local-prefs.el is deprecated, use prefs.el instead")
  (load init-local-prefs-file))
(when (file-exists-p init-prefs-file)
  (load init-prefs-file))

;;; Look and feel
(require 'init-look-and-feel)   ; fonts, UI, keybindings, saving files etc.
(require 'init-font-lock)       ; enables/disables font-lock globally.
(require 'init-linum)           ; line numbers

;;; Usability
(require 'init-window-manager)  ; navigate between windows
(require 'init-util)            ; utilities like match paren, bookmarks...
(require 'init-ido)             ; supercharged completion engine
(when *init-auto-complete*
  (require 'init-autocomplete)) ; auto-completion (see below for RTags AC)
(when *init-helm-projectile*    ; find files anywhere in project
  (require 'init-helm-projectile))

;;; Magit and git gutter
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
(when (file-exists-p init-local-file)
  (warn "init-local.el is deprecated, use after-init.el")
  (load init-local-file))
(when (file-exists-p init-after-init-file)
  (load init-after-init-file))

;;; Greetings
(setq initial-scratch-message
      (let ((current-user (split-string (user-full-name) " ")))
        (format ";; Happy hacking %s!

" (if current-user (car current-user) *environment-current-user*))))
;;; End of file
