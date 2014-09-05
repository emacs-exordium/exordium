;;;; Environment variables

(defconst *environment-osx* (eq system-type 'darwin)
  "Non-nil if we are on a Mac")

(defconst *environment-linux* (string-match "linux" (prin1-to-string system-type))
  "Non-nil if we are on Linux")

(defconst *environment-nw*  (not window-system)
  "Non-nil if emacs is started in -nw mode")

(defconst *environment-xwindow* (eq window-system 'x)
  "Non-nil if we are in X-Window")

(defconst *environment-bloomberg* (and *environment-xwindow*
                                       (getenv "MBIG_NUMBER"))
  "Non-nil if we are at Bloomberg")

(defconst *environment-current-user*
  (getenv
   (if (equal system-type 'windows-nt) "USERNAME" "USER")))

(provide 'init-environment)
