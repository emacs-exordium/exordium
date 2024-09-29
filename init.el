;;;;  ___      __   __   __
;;;; |__  \_/ /  \ |__) |  \ | |  |  |\/|
;;;; |___ / \ \__/ |  \ |__/ | \__/  |  |
;;;;
;;;; Emacs Makes All Computing Simple.

(require 'early-init)

;;; Minibuffer
;; Keep the cursor out of the read-only portions of the.minibuffer
(setq minibuffer-prompt-properties
      '(read-only t intangible t cursor-intangible t face
                  minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)


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
