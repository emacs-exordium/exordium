;;;; Philippe's
;;;;    ___ _ __ ___   __ _  ___ ___
;;;;   / _ \ '_ ` _ \ / _` |/ __/ __|
;;;;  |  __/ | | | | | (_| | (__\__ \
;;;; (_)___|_| |_| |_|\__,_|\___|___/
;;;;
;;;; Emacs Makes All Computing Simple.

(let ((min-version "24.0"))
  (when (version< emacs-version min-version)
    (error "This config requires at least Emacs %s, but you're running %s"
           min-version emacs-version)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Packages from Melpa and Marmelade
;; Use M-x `package-refresh-contents' to update the cache.
;; Use M-x `package-list-package' to load and display the list of packges,
;; then press I to mark for installation and X to execute (it's like dired).

(require 'package)
;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Path for "require"

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(defun add-directory-tree-to-load-path (dir)
  "Add 'dir' and all its subdirs to the load path"
  (add-to-list 'load-path dir)
  (let ((default-directory dir))
    (normal-top-level-add-subdirs-to-load-path)))

(add-directory-tree-to-load-path "~/.emacs.d/extensions/")
(add-directory-tree-to-load-path "~/.emacs.d/themes/")

(setq custom-theme-directory "~/emacs.d/themes/")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Load Modules

(require 'init-prolog)      ; environment; must be loaded first
(require 'init-ui)          ; fonts, menubar, syntax highlighting etc.
(require 'init-user-prefs)  ; backup files, trailing spaces...
(require 'init-keyboard)    ; key bindings
(require 'init-util)        ; utilities like match paren, bookmarks...
(require 'init-extensions)  ; minor modes like CUA, 80 col, FIXME etc.
(require 'init-ido)
(require 'init-markdown)
(require 'init-org)

;;; Themes
(if *environment-nw*
    (set-face-background 'highlight nil)
  (require 'init-themes)
  (unless *environment-bloomberg*
    (require 'init-powerline)))

(when *environment-osx*
  (require 'init-osx))

;;; C++
(require 'init-cpp)
(require 'init-bde-style)
(require 'init-autocomplete)
(require 'init-yasnippet)
;; (require 'init-cedet.el)
(require 'init-rtags)

;;; Other programming languages
(require 'init-javascript)
(unless *environment-bloomberg*
  ;; Save a little bit of time
  (require 'init-clojure))

;;; Local extensions
(when (file-exists-p "~/.emacs.d/init_local.el")
  (load "~/.emacs.d/init_local.el"))

;;; End of file
