;;; init-environment.el --- Environment variables ;; -*- lexical-binding: t; -*-
;;; Commentary:
;;

;;; Code:

(defconst exordium-osx (eq system-type 'darwin)
  "Non-nil if we are on a Mac.")

(defconst exordium-linux (string-match "linux" (prin1-to-string system-type))
  "Non-nil if we are on Linux.")

(defconst exordium-nw (not window-system)
  "Non-nil if Emacs is started in -nw mode.")

(defconst exordium-xwindow (eq window-system 'x)
  "Non-nil if we are in X-Window.")

(defconst exordium-bloomberg (and exordium-xwindow
                                  (getenv "MBIG_NUMBER"))
  "Non-nil if we are at Bloomberg.")

(defconst exordium-current-user
  (getenv
   (if (equal system-type 'windows-nt) "USERNAME" "USER")))

(provide 'init-environment)

;;; init-environment.el ends here
