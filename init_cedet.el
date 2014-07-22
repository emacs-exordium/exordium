;;;; Configuration for CEDET
;;;
;;; Usage:
;;; * Each file is indexed when visited. The content of the index is stored in
;;;   ~/.emacs.d/semantics when Emacs exits.
;;;
;;; * Add pre-indexing of useful package group headers in your init_local.el:
;;;   (semantic-add-system-include-tree "/path/to/groups/bsl")
;;;
;;; * Force pre-indexing of a full project (source and headers):
;;;   M-x semantic-index-source-tree
;;;   It will ask for the root directory (e.g. "mbs"). Indexing may take several
;;;   minutes.
;;;
;;; Functions:
;;; * `semantic-mode' - turn indexing on or off
;;; * `semantic-add-system-include-tree' - (non-interactive) add a package
;;;   group directory for indexing its headers (not sources)
;;; * `semantic-index-source-tree' - ask for a directory name, then indexes
;;;   all headers and sources in it.
;;; * `ecb-toggle' - toggle Emacs code browser.
;;;
;;; Note: to debug, try `semantic-c-describe-environment'.
;;;
;;; ------------ ---------------------------------------------------------
;;; Key          Definition
;;; ------------ ---------------------------------------------------------
;;; Meta-Space   autocomplete (menu)
;;; F3           jump to definition
;;; F4           open include file
;;; F5           toggle ecb
;;; Ctrl-C r     find references
;;; Ctrl-C s     show signature
;;; Ctrl-C d     show documentation (doesn't understand BDE style though)
;;; ------------ ---------------------------------------------------------
;;;
;;; Resources:
;;; http://www.logilab.org/173886
;;; http://alexott.net/en/writings/emacs-devenv/EmacsCedet.html#sec6
;;; http://www.gnu.org/software/emacs/manual/html_mono/semantic.html
;;; http://cxwangyi.wordpress.com/2010/08/21/using-cedet-with-emacs/

(add-to-list 'load-path "~/.emacs.d/cedet/")
(add-to-list 'load-path "~/.emacs.d/cedet/contrib")

(load-file "~/.emacs.d/cedet/cedet-devel-load.el")

(add-to-list 'Info-directory-list "~/.emacs.d/cedet/doc/info")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Semantics
;;;
;;; Semantics is a collection of language parsers (including C++) and an index
;;; stored in a persistent database (~/.emacs.d/semanticdb). It parses every
;;; file you visit.

;;; List of modes and associated parsing functions
(setq semantic-new-buffer-setup-functions
      '((c-mode . semantic-default-c-setup)
        (c++-mode . semantic-default-c-setup)
        ;;(emacs-lisp-mode . semantic-default-elisp-setup)
        ;;(html-mode . semantic-default-html-setup)
        ;;(java-mode . wisent-java-default-setup)
        ;;(js-mode . wisent-javascript-setup-parser)
        ;;(python-mode . wisent-python-default-setup)
        ;;(scheme-mode . semantic-default-scheme-setup)
        ;;(f90-mode . semantic-default-f90-setup)
        ;;(srecode-template-mode . srecode-template-setup-parser)
        ;;(texinfo-mode . semantic-default-texi-setup)
        (makefile-automake-mode . semantic-default-make-setup)
        (makefile-gmake-mode . semantic-default-make-setup)
        ;;(makefile-makepp-mode . semantic-default-make-setup)
        ;;(makefile-bsdmake-mode . semantic-default-make-setup)
        ;;(makefile-imake-mode . semantic-default-make-setup)
        (makefile-mode . semantic-default-make-setup)))

;;; Select Semantic submodes
(add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
(add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)

;;; Activate Semantic
(semantic-mode 1)

;; No sticky line on top
(global-semantic-stickyfunc-mode 0)

;; Re-parse current buffer when emacs is idle
;;(global-semantic-idle-scheduler-mode 1)

;;; load contrib library
(require 'eassist)

;;; Keys
(global-unset-key (kbd "M-SPC"))
(defun my-cedet-hook ()
  (add-to-list 'ac-sources 'ac-source-semantic)
  ;; Keys
  (local-set-key (kbd "M-SPC") 'semantic-ia-complete-symbol-menu)
  (local-set-key (kbd "<f3>") 'semantic-ia-fast-jump)
  (local-set-key "\C-cr" 'semantic-symref)
  (local-set-key "\C-cs" 'semantic-ia-show-summary)
  (local-set-key "\C-cd" 'semantic-ia-show-doc)
  (local-set-key "\C-ck" 'semantic-ia-describe-class)
  (local-set-key (kbd "<f4>") 'semantic-decoration-include-visit)
  ;; TODO change or remove
  (local-set-key "\C-c?" 'semantic-ia-complete-symbol)
  (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
  (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle))

(add-hook 'c-mode-common-hook 'my-cedet-hook)
(add-hook 'lisp-mode-hook 'my-cedet-hook)
(add-hook 'emacs-lisp-mode-hook 'my-cedet-hook)

(defun my-c-mode-cedet-hook ()
  ;;(local-set-key "\C-ct" 'eassist-switch-h-cpp)
  ;;(local-set-key "\C-xt" 'eassist-switch-h-cpp)
  (local-set-key "\C-ce" 'eassist-list-methods)
  (local-set-key "\C-c\C-r" 'semantic-symref))

(add-hook 'c-mode-common-hook 'my-c-mode-cedet-hook)

;; Use gnu-global
;;(semanticdb-enable-gnu-global-databases 'c-mode t)
;;(semanticdb-enable-gnu-global-databases 'c++-mode t)

;; Use C-tags
;; (when (cedet-ectag-version-check t)
;;   (semantic-load-enable-primary-ectags-support))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Include directories to index

(defun semantic-add-system-include-tree (dir)
  "Add any subdir as a semantic system include for the C++ mode"
  (dolist (d (directory-tree dir))
    (semantic-add-system-include d 'c++-mode)))

;;; Add the package groups to index in init_local.el like so:
;;(semantic-add-system-include-tree "/Users/phil/Code/cpp/bde/groups/bsl")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Source trees to index

(defun semantic-index-source-tree (dir)
  "Index all source files in the specified dir"
  (interactive "DIndex source directory: ")
  (dolist (d (cons dir (directory-tree dir)))
    (let ((files (directory-files d t ".*\\.\\(cpp\\|h\\)")))
      (dolist (f files)
        (semanticdb-file-table-object f)))))

;;; Add project source trees in init_local.el like so:
;;(semantic-index-source-tree "/Users/phil/Code/cpp/foo")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ECB - Emacs Code Browser

(add-to-list 'load-path (expand-file-name "~/.emacs.d/cedet-ecb/"))
(require 'ecb)

(defvar ecb-toggle-status nil)
(defun ecb-toggle ()
  "Toggle the Emacs Code Browser"
  (interactive)
  (if ecb-toggle-status
      (progn
        (ecb-deactivate)
        (setq ecb-toggle-status nil))
    (ecb-activate)
    (setq ecb-toggle-status t)))

(unless (emacs-bloomberg-p)
  ;; It either loops or crashes our build of emacs :'(
  (define-key global-map [f5] 'ecb-toggle))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EDE - Emacs Development Environment
;;;
;;; EDE adds the notion of project to Emacs. It can in particular define the
;;; root dir of the project and the directories to index. It can also build.

;;(global-ede-mode 1)
;;(ede-enable-generic-projects)

;; (ede-cpp-root-project "foo"
;;                 :name "Project foo for testing"
;;                 :file "~/Code/cpp/foo/foo.cpp" ;; not parsed, used as anchor
;;                 :include-path '("/"
;;                                )
;;                 :system-include-path '("~/Code/cpp/bde/groups/bsl")
;;                 :spp-table '(("isUnix" . "")
;;                              ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SRecode
;;;
;;; Source code generation using Semantics information, with templates.

;;(global-srecode-minor-mode 1)
