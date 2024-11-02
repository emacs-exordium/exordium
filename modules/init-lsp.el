;;; init-lsp --- Initialize LSP mode for Exordium

;;; Commentary:

;;; Code:
(require 'init-prefs)

(use-package flycheck-pos-tip
  :after flycheck
  :defines flycheck-pos-tip-timeout
  :hook (global-flycheck-mode . flycheck-pos-tip-mode)
  :config (setq flycheck-pos-tip-timeout 30))

;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
(setq lsp-keymap-prefix exordium-lsp-keymap-prefix)

(use-package lsp-mode
  :if exordium-lsp-mode-enable
  :hook ((c-mode-common  . lsp)
         (c++-ts-mode  . lsp)
         (c-ts-mode  . lsp))
  :init
  (setq-default lsp-clients-clangd-executable
                (seq-find #'executable-find exordium-lsp-clangd-executable))
  :commands (lsp lsp-deferred)

  :config
  (if exordium-enable-which-key
      (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))
  (setq lsp-clients-clangd-args exordium-lsp-clangd-args)
  (setq lsp-diagnostic-package :flycheck)
  (setq lsp-flycheck-live-reporting t)
  ;; company mode configuration for lsp-mode
  (setq lsp-completion-provider :capf)
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0)

  ;; process buffer for the LSP server needs to be larger
  (setq read-process-output-max (* 1024 1024)) ;; 1mb

  ;; semantic hilite via lsp server
  (setq lsp-enable-semantic-highlighting t)

  (setq lsp-idle-delay 0.1) ;; clangd is fast

  (setq treemacs-space-between-root-nodes nil)

  (setq lsp-log-io t)
)

(use-package lsp-ui
  :after lsp-mode
  :init
  (setq lsp-ui-doc-enable exordium-lsp-ui-doc-enable
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-show-with-cursor nil
        lsp-ui-doc-position exordium-lsp-ui-doc-position
        lsp-ui-doc-include-signature t

        lsp-ui-sideline-enable exordium-lsp-ui-sideline-enable
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-show-hover nil

        lsp-ui-flycheck-enable exordium-lsp-ui-flycheck-enable
        lsp-ui-flycheck-list-position exordium-lsp-ui-flycheck-list-position

        lsp-ui-peek-enable exordium-lsp-ui-peek-enable

        lsp-lens-enable t)
  :commands lsp-ui-mode)

(use-package helm-xref
  :ensure t
  :after helm
  :if exordium-helm-everywhere
  :commands helm-xref
  :config
  (setq xref-show-xrefs-function 'helm-xref-show-xrefs))

(use-package helm-lsp
  :after (lsp-mode helm)
  :if exordium-helm-everywhere
  :commands
  (helm-lsp-workspace-symbol
   helm-lsp-global-workspace-symbol
   helm-lsp-code-actions))

(use-package lsp-treemacs
  :after lsp-mode
  :commands lsp-treemacs-errors-list)

(use-package dap-mode
  :requires (dap-lldb dap-cpptools dap-gdb-lldb)
  :init
  (setq lsp-enable-dap-auto-configure t)
  :config
  (require 'dap-cpptools)
  (require 'dap-lldb)
  (require 'dap-gdb-lldb)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  :commands dap-mode)


;; ;; Terrible hack working around off by one error between TRAMP and lsp-mode
;; (defun lsp--make-message@override (params)
;;   "Create a LSP message from PARAMS, after encoding it to a JSON string."
;;   (let ((body (lsp--json-serialize params)))
;;     (concat "Content-Length: "
;;             (number-to-string (+ 2 (string-bytes body))) ;; dirty fix for pyls remote (https://github.com/emacs-lsp/lsp-mode/issues/1845#issuecomment-699169414)
;;             ;;(number-to-string (1+ (string-bytes body)))
;;             "\r\n\r\n"
;;             body
;;             "\n")))

;; (advice-add 'lsp--make-message :override #'lsp--make-message@override)

(provide 'init-lsp)
;;; init-lsp.el ends here
