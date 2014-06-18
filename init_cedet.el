;;;; Configuration for CEDET
;;;;
;;;; Resources:
;;;; http://www.logilab.org/173886
;;;; http://alexott.net/en/writings/emacs-devenv/EmacsCedet.html#sec6
;;;; http://www.gnu.org/software/emacs/manual/html_mono/semantic.html
;;;; http://cxwangyi.wordpress.com/2010/08/21/using-cedet-with-emacs/

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

;;; load contrib library
(require 'eassist)

;;; ------------ ---------------------------------------------------------
;;; Key          Definition
;;; ------------ ---------------------------------------------------------
;;; Meta-Space   autocomplete (menu)
;;; F3           jump to definition
;;; F4           open include file
;;; Ctrl-C r     find references
;;; Ctrl-C s     show signature
;;; Ctrl-C d     show documentation (doesn't understand BDE though)
;;; ------------ ---------------------------------------------------------
(global-unset-key (kbd "M-SPC"))
(defun my-cedet-hook ()
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

(semanticdb-enable-gnu-global-databases 'c-mode t)
(semanticdb-enable-gnu-global-databases 'c++-mode t)

(when (cedet-ectag-version-check t)
  (semantic-load-enable-primary-ectags-support))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Include directories to index

(defun directory-tree (dir)
  "Returns the list of subdirs excluding any dot files"
  (let* ((dir   (directory-file-name dir))
         (dirs  '())
         (files (directory-files dir nil nil t)))
    (dolist (f files)
      (unless (string-equal "." (substring f 0 1))
        (let ((f (concat dir "/" f)))
          (when (file-directory-p f)
            (setq dirs (append (cons f (directory-tree f))
                               dirs))))))
    dirs))

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
  (dolist (d (cons dir (directory-tree dir)))
    (let ((files (directory-files d t ".*\\.\\(cpp\\|h\\)")))
      (dolist (f files)
        (semanticdb-file-table-object f)))))

;;; Add project source trees in init_local.el like so:
;;(semantic-index-source-tree "/Users/phil/Code/cpp/foo")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ECB - Emacs Code Browser

;;; TODO first create a branch and commit without ecb
;;; Then add ecb after testing.

;;(add-to-list 'load-path (expand-file-name "~/Code/ecb/"))
;;(require 'ecb)
;; then ecb-activate

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
