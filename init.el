;;;; Philippe's
;;;;    ___ _ __ ___   __ _  ___ ___
;;;;   / _ \ '_ ` _ \ / _` |/ __/ __|
;;;;  |  __/ | | | | | (_| | (__\__ \
;;;; (_)___|_| |_| |_|\__,_|\___|___/
;;;;
;;;; Any editor can save your files. Only Emacs can save your soul.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Environment functions

(defun emacs-nw-p ()
  (not window-system))

(defun emacs-osx-p ()
  (eq window-system 'ns))

(defun emacs-x-p ()
  (eq window-system 'x))

(defun gnu-emacs-p ()
  (string-match "GNU Emacs" (version)))

(defun emacs-24-p ()
  (>= emacs-major-version 24))

(defun emacs-23-p ()
  (>= emacs-major-version 23))

(defun emacs-22-p ()
  (string-match "GNU Emacs 22" (version)))

(defmacro emacs-linux ()
  (string-match "linux" (prin1-to-string system-type)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Packages from Melpa and Marmelade

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; Do package-refresh-contents to update the cache


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

(load "~/.emacs.d/init_user.el")
(load "~/.emacs.d/init_util.el")
(load "~/\.emacs.d/init_extensions.el")

(if (not (emacs-nw-p))
    (load "~/.emacs.d/init_themes.el")
  (set-face-background 'highlight nil))

(load "~/.emacs.d/init_cpp.el")
