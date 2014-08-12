;;;; Philippe's
;;;;    ___ _ __ ___   __ _  ___ ___
;;;;   / _ \ '_ ` _ \ / _` |/ __/ __|
;;;;  |  __/ | | | | | (_| | (__\__ \
;;;; (_)___|_| |_| |_|\__,_|\___|___/
;;;;
;;;; Emacs Makes All Computing Simple.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Packages from Melpa and Marmelade
;; Use M-x `package-refresh-contents' to update the cache.
;; Use M-x `package-list-package' to load and display the list of packges,
;; then press I to mark for installation and X to execute (it's like dired).

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Path for "require"

(setq custom-theme-directory "~/emacs.d/themes/")

(defun add-tree-to-load-path (dir)
  "Add 'dir' and all its subdirs to the load path"
  (add-to-list 'load-path dir)
  (let ((default-directory dir))
    (normal-top-level-add-subdirs-to-load-path)))

(add-tree-to-load-path "~/.emacs.d/extensions/")
(add-tree-to-load-path "~/.emacs.d/themes/")
(add-tree-to-load-path "~/.emacs.d/bde/")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Components

(load "~/.emacs.d/init_prolog.el") ; must be loaded first
(load "~/.emacs.d/init_user.el")
(load "~/.emacs.d/init_util.el")
(load "~/.emacs.d/init_extensions.el")

;;; Themes
(if (not (emacs-nw-p))
    (progn
      (load "~/.emacs.d/init_themes.el")
      (unless (emacs-bloomberg-p)
        (load "~/.emacs.d/init_powerline.el")))
  (set-face-background 'highlight nil))

;;; C++
(load "~/.emacs.d/init_cpp.el")
(load "~/.emacs.d/init_autocomplete.el")
;;(load "~/.emacs.d/init_cedet.el")
(load "~/.emacs.d/init_rtags.el")

;;; Javascript
(load "~/.emacs.d/init_javascript.el")

;;; Clojure
(load "~/.emacs.d/init_clojure.el")

;;; Local extensions
(when (file-exists-p "~/.emacs.d/init_local.el")
  (load "~/.emacs.d/init_local.el"))

;;; End of file
