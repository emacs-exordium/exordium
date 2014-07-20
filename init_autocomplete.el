;;;; Autocomplete extensions
;;;
;;; This file load extensions:
;;; auto-complete : auto completion backend
;;; yasnippet ....: it completes auto complete
;;;
;;; To auto-complete C++ headers, set variable `local-ac-c-header-trees' to the
;;; list of directories containing system include directory (or BDE package
;;; groups). For example:
;;;   (setq system-ac-c-header-trees
;;;        '("/Users/phil/Code/cpp/bde/groups/bsl"
;;;          "/Users/phil/Code/cpp/bde/groups/bdl"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Auto-complete

(require 'auto-complete)

;; Default config for auto-complete
(require 'auto-complete-config)
(ac-config-default)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Yasnippet

(require 'yasnippet)
(yas-global-mode 1) ; always on

;; Auto-complete for C/C++ files
(defvar system-ac-c-header-trees ()
  "List of directory containing package groups to use for include
  file autocomplete")

(defun my-ac-c-header-init ()
  "C++ mode hook for autocompleting system headers"
  (require 'auto-complete-c-headers)
  ;; Standard OS headers
  (add-to-list 'ac-sources 'ac-source-c-headers)
  ;; Custom headers for BDE
  (dolist (tree system-ac-c-header-trees)
    (dolist (dir (directory-tree tree))
      (add-to-list 'achead:include-directories dir))))

(add-hook 'c++-mode-hook 'my-ac-c-header-init)
(add-hook 'c-mode-hook 'my-ac-c-header-init)
