;;;; Philippe's
;;;;
;;;;    ___ _ __ ___   __ _  ___ ___
;;;;   / _ \ '_ ` _ \ / _` |/ __/ __|
;;;;  |  __/ | | | | | (_| | (__\__ \
;;;; (_)___|_| |_| |_|\__,_|\___|___/
;;;;


;;; Environment macros

(defmacro gnu-emacs-only (&rest x)
  (list 'if (string-match "GNU Emacs" (version))
        (cons 'progn x)))

(defmacro gnu-emacs24-only (&rest body)
  (list 'if (string-match "GNU Emacs 24" (version))
        (cons 'progn body)))

(defmacro x-windows-only (&rest x)
  (list 'if (eq window-system 'x)
        (cons 'progn x)))

(defmacro osx-only (&rest x)
  (list 'if (eq window-system 'ns)
        (cons 'progn x)))

(defmacro linux-only (&rest body)
  (list 'if (string-match "linux" (prin1-to-string system-type))
        (cons 'progn body)))


;;; Path for "require"

(add-to-list 'load-path "~/.emacs.d/extensions/")


;;; Components

(load "~/.emacs.d/init_user.el")
(load "~/.emacs.d/init_extensions.el")
(load "~/.emacs.d/init_themes.el")
