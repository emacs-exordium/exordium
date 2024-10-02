;;;;  ___      __   __   __
;;;; |__  \_/ /  \ |__) |  \ | |  |  |\/|
;;;; |___ / \ \__/ |  \ |__/ | \__/  |  |
;;;;
;;;; Emacs Makes All Computing Simple.

;; Reduce the frequency of garbage collection by making it happen on
;; each 100MB of allocated data (the default is on every 0.76MB). This reduces
;; the startup time.
(setq gc-cons-threshold 100000000)

(let ((min-version "27.1"))
  (when (version< emacs-version min-version)
    (error "This config requires at least Emacs %s, but you're running %s"
           min-version emacs-version)))

(setq initial-scratch-message
      "EXORDIUM DID NOT LOAD CORRECTLY.
Check the warnings and messages buffers, or restart with --debug-init")

(defconst exordium-before-init "before-init.el"
  "name of the before init file")

(defconst exordium-prefs "prefs.el"
  "name of the prefs file")

(defconst exordium-after-init "after-init.el"
  "name of the after init file")

(defconst exordium-custom "emacs-custom.el"
  "name of the customization file")

;; Use this file for HTTP proxy settings if needed for packages.  Also add
;; additional packages to exordium-extra-packages for packages to be
;; automatically pulled from the elpa archives

(defconst exordium-before-init-file (locate-user-emacs-file exordium-before-init)
  "location of the master before init file")

(defconst exordium-modules-dir (locate-user-emacs-file "modules")
  "location of the modules directory")
(defconst exordium-themes-dir (locate-user-emacs-file "themes")
  "location of the themes directory")
(defconst exordium-extensions-dir (locate-user-emacs-file "extensions")
  "location of the extensions directory")
(defconst exordium-local-dir (locate-user-emacs-file "local")
  "location of the local directory")

(defconst exordium-prefs-file (locate-user-emacs-file exordium-prefs)
  "location of the master prefs file")

(defconst exordium-after-init-file (locate-user-emacs-file exordium-after-init)
  "location of the master after init file")

(defconst exordium-custom-file (locate-user-emacs-file exordium-custom)
  "location of the customization file")

;; Save any custom set variable in exordium-custom-file rather than at the end of init.el:
(setq custom-file exordium-custom-file)

(defcustom exordium-extra-packages ()
  "A list of additional packages to auto load from elpa repositories."
    :group 'exordium
    :type  'list)

(defcustom exordium-extra-pinned ()
  "An alist of additional packages locations to pin to.

Each element of the list is in the same form as in `package-pinned-packages'."
  :group 'exordium
  :type  'alist)

;; Taps definition of before and after files. These are loaded
;; after master 'before', 'after', and 'prefs' files

(defconst exordium-taps-root (locate-user-emacs-file "taps")
  "location of the tapped directories")

(defconst exordium-tapped-before-init-files ()
  "all tapped before init files, including master")

(defconst exordium-tapped-prefs-files ()
  "all tapped prefs files, including master")

(defconst exordium-tapped-after-init-files ()
  "all tapped after init files, including master")

(defconst exordium-melpa-package-repo "https://melpa.org/packages/"
  "URL for packages repository")

(defconst exordium-pinned-melpa-package-repo "https://melpa.org/packages/"
  "URL for pinned default packages. Set to stable melpa.org if you want stable")

(defconst exordium-gnu-package-repo "https://elpa.gnu.org/packages/"
  "URL for the GNU package repository")

(when (file-accessible-directory-p exordium-taps-root)
  (dolist (tap (nreverse (directory-files exordium-taps-root t "^[^\.][^\.]?*+")))
    (when (file-accessible-directory-p tap)
      (let ((tapped (concat (file-name-as-directory tap) exordium-before-init)))
        (when (file-readable-p tapped)
          (add-to-list 'exordium-tapped-before-init-files tapped))
        (setq tapped (concat (file-name-as-directory tap) exordium-prefs))
        (when (file-readable-p tapped)
          (add-to-list 'exordium-tapped-prefs-files tapped))
        (setq tapped (concat (file-name-as-directory tap) exordium-after-init))
        (when (file-readable-p tapped)
          (add-to-list 'exordium-tapped-after-init-files tapped))))))

(when (file-readable-p exordium-before-init-file)
  (add-to-list 'exordium-tapped-before-init-files exordium-before-init-file))

(when (file-readable-p exordium-prefs-file)
  (add-to-list 'exordium-tapped-prefs-files exordium-prefs-file))

(when (file-readable-p exordium-after-init-file)
  (add-to-list 'exordium-tapped-after-init-files exordium-after-init-file))

(when (file-readable-p exordium-custom-file)
  (add-to-list 'exordium-tapped-after-init-files exordium-custom-file))

;; Load before init files
(dolist (tapped-file exordium-tapped-before-init-files)
  (load tapped-file))


;;; Packages from Melpa
;; Use M-x `package-refresh-contents' to update the cache.
;; Use M-x `package-list-package' to load and display the list of packages,
;; then press I to mark for installation and X to execute (it's like dired).

;; Initialize the package system
(require 'seq)
(require 'package)
(when (or (not (string= exordium-melpa-package-repo
                        exordium-pinned-melpa-package-repo))
          (seq-filter (lambda (pkg)
                        (string= "melpa" (cdr pkg)))
                      exordium-extra-pinned))
  (add-to-list 'package-archives
               (cons "melpa" exordium-melpa-package-repo) t))
(add-to-list 'package-archives
             (cons "melpa-pinned" exordium-pinned-melpa-package-repo) t)
(unless (seq-filter (lambda (repo)
                      (string= exordium-gnu-package-repo (cdr repo)))
                    package-archives)
  (add-to-list 'package-archives
               (cons "gnu" exordium-gnu-package-repo) t))

(setq package-user-dir
      (locate-user-emacs-file (concat "elpa-" emacs-version)))

(when (fboundp 'native-comp-available-p)
 (setq package-native-compile (native-comp-available-p)))

(package-initialize)

;; Load the packages we need if they are not installed already
(let ((package-pinned-packages (append
                                '((use-package             . "melpa-pinned")
                                  (diminish                . "melpa-pinned")
                                  (bind-key                . "melpa-pinned"))
                                exordium-extra-pinned))
      (has-refreshed nil))

  (defun update-package (p  has-refreshed)
    (unless (package-installed-p p)
      (unless has-refreshed
        (message "Refreshing package database...")
        (package-refresh-contents)
        (setq has-refreshed t)
        (message "Done."))
      (package-install p)))

  (dolist (pkg package-pinned-packages)
    (let ((p (car pkg)))
      (update-package p has-refreshed)))

  (dolist (pkg exordium-extra-packages)
    (update-package pkg has-refreshed)))

;; - Some packages (i.e., magit, forge) require seq-2.24.
;; - Emacs-29.1 is delivered with seq-2.23.
;; - Other packages (i.e., compat) require seq-2.23.
;; - When only magit is installed it requires compat which requires seq-2.23 -> seq is not upgraded
;; - When only forge is installed is requires magit and compat which requires seq-2.23 -> seq is not upgraded
;; - When magit is installed followed by installation of forge seq is upgraded to seq-2.24 -> this fails
;; Force installing the freshest version of seq with errors suppressed:
(when (version< emacs-version "29.2")
  (let (debug-on-error)
    ;; this assumes `package-refresh-contents has been called'
    (package-install (car (alist-get 'seq package-archive-contents)))))

;;; Path for "require"

(add-to-list 'load-path exordium-modules-dir)

(defun add-directory-tree-to-load-path (dir &optional ignore-if-absent)
  "Add DIR and all its subdirs to the load path."
  (cond ((file-directory-p dir)
         (add-to-list 'load-path dir)
         (let ((default-directory dir))
           (normal-top-level-add-subdirs-to-load-path)))
        ((not ignore-if-absent)
         (warn "Missing directory: %s" dir))))

(add-directory-tree-to-load-path exordium-extensions-dir)
(add-directory-tree-to-load-path exordium-themes-dir)
(add-directory-tree-to-load-path exordium-local-dir t)

(add-directory-tree-to-load-path exordium-taps-root)

(setq custom-theme-directory exordium-themes-dir)


;; This is only needed once, near the top of the file
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (require 'use-package))
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant

(require 'use-package-ensure)
(setq use-package-always-ensure t)
(setq use-package-compute-statistics t)

;;; remove a package from the builtin list so it can be upgraded
(defun exordium-ignore-builtin (pkg)
  (assq-delete-all pkg package--builtins)
  (assq-delete-all pkg package--builtin-versions))


;;; Load Modules
(use-package bytecomp :ensure nil)
(defun recompile-modules ()
  "Recompile modules for which the .elc is older than the .el, if
the .elc exists. Also discard .elc without corresponding .el"
  (interactive)
  (dolist (dir (list exordium-modules-dir
                     exordium-themes-dir
                     exordium-extensions-dir
                     exordium-local-dir))
    (when (file-directory-p dir)
      ;; Recompile
      (dolist (el (directory-files dir t "\\.el$"))
        (let ((elc (byte-compile-dest-file el)))
          (when (and (file-exists-p elc)
                     (file-newer-than-file-p el elc))
            (byte-compile-file el))))
      ;; Discard .elc singletons
      (dolist (elc (directory-files dir t "\\.elc$"))
        (let ((el (concat (concat (file-name-sans-extension elc) ".el"))))
          (unless (file-exists-p el)
            (warn "Removing singleton .elc file: %s" elc)
            (delete-file elc)))))))
(recompile-modules)

(use-package init-lib :ensure nil)         ; utility functions - load this first
(use-package init-environment :ensure nil) ; environment variables

;;; Local preferences (fonts, frame size etc.)
(use-package init-prefs :ensure nil)       ; defines variables that prefs.el can override
(dolist (tapped-file exordium-tapped-prefs-files)
  (load tapped-file))

;;; Themes
;;; Note: use "export TERM=xterm-256color" for emacs -nw
(use-package init-progress-bar :ensure nil)
(when exordium-nw
  (set-face-background 'highlight nil))
(use-package init-themes :ensure nil :if exordium-theme)

;;; Look and feel
(use-package init-look-and-feel :ensure nil)   ; fonts, UI, keybindings, saving files etc.
(use-package init-font-lock :ensure nil)       ; enables/disables font-lock globally.
(use-package init-linum :ensure nil)           ; line numbers
(use-package init-smooth-scroll
  :ensure nil
  :if exordium-smooth-scroll
  :config (smooth-scroll-mode 1)) ; smooth
                                                                                                       ; scroll

(update-progress-bar)

;;; Usability
(use-package init-window-manager :ensure nil)  ; navigate between windows
(use-package init-util :ensure nil)            ; utilities like match paren, bookmarks...
(use-package init-ido :ensure nil)             ; supercharged completion engine
(use-package init-highlight :ensure nil)       ; highlighting current line, symbol under point
(use-package init-autocomplete :ensure nil
  :if (eq exordium-complete-mode :auto-complete))
(use-package init-company :ensure nil
  :if (eq exordium-complete-mode :company))

(use-package init-helm-projectile :ensure nil
  :if exordium-helm-projectile)
(use-package init-helm :ensure nil)            ; setup helm

(use-package init-help :ensure nil
  :if exordium-help-extensions)

(update-progress-bar)

(use-package init-dired :ensure nil)           ; enable dired+ and wdired permission editing
(use-package init-git :ensure nil)             ; Magit and git gutter
(use-package init-git-visit-diffs :ensure nil) ; visit diffs in successive narrowed buffers
(use-package init-forge :ensure nil)           ; Forge
(use-package init-flb-mode :ensure nil)        ; frame-local buffers

(update-progress-bar)

;;; Prog mode
(use-package init-prog-mode :ensure nil)

;;; Shell mode
(use-package init-shell :ensure nil)

;;; Major modes
(use-package init-markdown :ensure nil)
(use-package init-org :ensure nil)
(use-package init-xml :ensure nil)

;;; OS-specific things
(use-package init-osx :ensure nil :if exordium-osx)

;;; C++
(use-package init-cpp :ensure nil)
(use-package init-bde-style :ensure nil)
(use-package init-yasnippet :ensure nil :if exordium-yasnippet)
(use-package init-gdb :ensure nil)

;;; RTags
(use-package init-rtags :ensure nil)
(when (and (eq exordium-complete-mode :auto-complete)
       exordium-rtags-auto-complete)
  (rtags-auto-complete))
(use-package init-rtags-helm :ensure nil)
(use-package init-rtags-cmake :ensure nil)
(use-package init-rtags-cdb :ensure nil)

(update-progress-bar)

;;; JS
(use-package init-javascript :ensure nil)

;;; Python
(use-package init-python :ensure nil)

;;; Ruby
(use-package init-ruby :ensure nil)

;;; Lisp
(use-package init-elisp :ensure nil)

;;; Clojure
(use-package init-clojure :ensure nil :if exordium-clojure)

;;; Groovy
(use-package init-groovy :ensure nil)

;;; include-what-you-use
(use-package init-iwyu :ensure nil)

(update-progress-bar)

;;; Desktop - close to the end so customisations had a chance to kick in
(when exordium-desktop
  (use-package init-desktop :ensure nil))

(use-package init-powerline :ensure nil
  :if (and exordium-theme exordium-enable-powerline))

;; Docker
(use-package init-docker :ensure nil)

;; Flycheck
(use-package init-flycheck :ensure nil)

;;; Treesit
(use-package init-treesit :ensure nil)

;;; LSP
(use-package init-lsp :ensure nil :if exordium-lsp-mode-enable)

;;; Local extensions
(dolist (tapped-file exordium-tapped-after-init-files)
  (load tapped-file))

(update-progress-bar)

;;; Greetings
(setq initial-scratch-message
      (let ((current-user (split-string (user-full-name) " ")))
        (format ";; Happy hacking %s!

" (if current-user (car current-user) exordium-current-user))))

;;; End of file
