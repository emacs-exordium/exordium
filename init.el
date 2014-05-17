;;;; Philippe's
;;;;
;;;;    ___ _ __ ___   __ _  ___ ___
;;;;   / _ \ '_ ` _ \ / _` |/ __/ __|
;;;;  |  __/ | | | | | (_| | (__\__ \
;;;; (_)___|_| |_| |_|\__,_|\___|___/
;;;;


;;; Macros

(defmacro gnu-emacs-only (&rest x)
  (list 'if (string-match "GNU Emacs" (version)) (cons 'progn x)))

(defmacro xemacs-only (&rest x)
  (list 'if (string-match "XEmacs" (version)) (cons 'progn x)))

(defmacro x-windows-only (&rest x)
  (list 'if (eq window-system 'x) (cons 'progn x)))

(defmacro mac-osx-only (&rest x)
  (list 'if (eq window-system 'ns) (cons 'progn x)))

(defmacro mac-osx-or-x-windows-only (&rest x)
  (list 'if (or (eq window-system 'ns) (eq window-system 'x)) (cons 'progn x)))

(defmacro non-x-windows-only (&rest x)
  (list 'if (not (eq window-system 'x)) (cons 'progn x)))


;;; Path for "require"

(add-to-list 'load-path "~/.emacs.d/site-lisp/")


;;; Components

(load "~/.emacs.d/init_user.el")
(load "~/.emacs.d/init_extensions.el")
(load "~/.emacs.d/init_themes.el")
