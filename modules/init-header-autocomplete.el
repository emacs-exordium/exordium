;;;; Init C++ headers Auto-complete
;;;
;;; This module sets up auto-complete of #include header files in C++ mode. It
;;; reuses the file `compile_includes' to find what include directories to use
;;; (see `init_rtags.el' for details about that file).
;;;
;;; Header auto-complete currently reuses the same variables that store the
;;; content of `compile_includes'. These variables are loaded with either:
;;; M-x `rtags-create-compilation-database'
;;; Or M-x `rtags-load-compile-includes-file' which is faster and is in fact
;;; called by the former. After calling one of these functions, header auto-
;;; -complete should work for any C++ file you open.

(require 'init-prolog)
(require 'init-rtags)
(require 'auto-complete-c-headers)

(defun header-ac-c-hook ()
  "C++ mode hook for autocompleting system headers"
  ;; Standard OS headers
  (add-to-list 'ac-sources 'ac-source-c-headers)
  ;; Project specific headers
  (dolist (dir *rtags-project-source-dirs*)
    (add-to-list 'achead:include-directories dir))
  (dolist (dir *rtags-project-include-dirs*)
    (add-to-list 'achead:include-directories dir)))

(add-hook 'c++-mode-hook 'header-ac-c-hook)
(add-hook 'c-mode-hook 'header-ac-c-hook)

(provide 'init-header-autocomplete)
