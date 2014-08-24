;;; rtags-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (rtags-diagnostics rtags-clear-diagnostics rtags-stop-diagnostics
;;;;;;  rtags-post-command-hook rtags-restart-tracking-timer rtags-update-current-project
;;;;;;  rtags-fix-fixit-at-point rtags-cycle-overlays-on-screen rtags-is-running
;;;;;;  rtags-clear-diagnostics-overlays rtags-apply-fixit-at-point
;;;;;;  rtags-find-references-current-dir rtags-find-symbol-current-dir
;;;;;;  rtags-find-references-current-file rtags-find-symbol-current-file
;;;;;;  rtags-find-references rtags-find-symbol rtags-rename-symbol
;;;;;;  rtags-guess-function-at-point rtags-find-all-references-at-point
;;;;;;  rtags-find-virtuals-at-point rtags-find-references-at-point
;;;;;;  rtags-find-symbol-at-point rtags-location-stack-reset rtags-location-stack-back
;;;;;;  rtags-location-stack-forward rtags-quit-rdm rtags-print-current-location
;;;;;;  rtags-enable-standard-keybindings rtags-location-stack-jump
;;;;;;  rtags-goto-offset rtags-print-enum-value-at-point rtags-print-dependencies
;;;;;;  rtags-print-cursorinfo rtags-set-current-project rtags-maybe-reparse-file
;;;;;;  rtags-reparse-file rtags-preprocess-file rtags-index-js-file
;;;;;;  rtags-previous-diag rtags-next-diag rtags-previous-match
;;;;;;  rtags-next-match rtags-bury-or-delete) "rtags" "rtags.el"
;;;;;;  (21497 57519 0 0))
;;; Generated autoloads from rtags.el

(autoload 'rtags-bury-or-delete "rtags" "\


\(fn)" t nil)

(autoload 'rtags-next-match "rtags" "\


\(fn)" t nil)

(autoload 'rtags-previous-match "rtags" "\


\(fn)" t nil)

(autoload 'rtags-next-diag "rtags" "\


\(fn)" t nil)

(autoload 'rtags-previous-diag "rtags" "\


\(fn)" t nil)

(autoload 'rtags-index-js-file "rtags" "\


\(fn)" t nil)

(autoload 'rtags-preprocess-file "rtags" "\


\(fn &optional BUFFER)" t nil)

(autoload 'rtags-reparse-file "rtags" "\
WAIT-REPARSING : t to wait for reparsing to finish, nil for async (no waiting).
:fixme: add a timeout

\(fn &optional BUFFER WAIT-REPARSING)" t nil)

(autoload 'rtags-maybe-reparse-file "rtags" "\


\(fn &optional BUFFER)" t nil)

(autoload 'rtags-set-current-project "rtags" "\


\(fn)" t nil)

(autoload 'rtags-print-cursorinfo "rtags" "\


\(fn &optional PREFIX)" t nil)

(autoload 'rtags-print-dependencies "rtags" "\


\(fn &optional BUFFER)" t nil)

(autoload 'rtags-print-enum-value-at-point "rtags" "\


\(fn &optional LOCATION)" t nil)

(autoload 'rtags-goto-offset "rtags" "\


\(fn POS)" t nil)

(autoload 'rtags-location-stack-jump "rtags" "\


\(fn BY)" t nil)

(autoload 'rtags-enable-standard-keybindings "rtags" "\


\(fn &optional MAP PREFIX)" t nil)

(autoload 'rtags-print-current-location "rtags" "\


\(fn)" t nil)

(autoload 'rtags-quit-rdm "rtags" "\


\(fn)" t nil)

(autoload 'rtags-location-stack-forward "rtags" "\


\(fn)" t nil)

(autoload 'rtags-location-stack-back "rtags" "\


\(fn)" t nil)

(autoload 'rtags-location-stack-reset "rtags" "\


\(fn)" t nil)

(autoload 'rtags-find-symbol-at-point "rtags" "\
Find the natural target for the symbol under the cursor and moves to that location.
For references this means to jump to the definition/declaration of the referenced symbol (it jumps to the definition if it is indexed).
For definitions it jumps to the declaration (if there is only one) For declarations it jumps to the definition.
If called with a prefix restrict to current buffer

\(fn &optional PREFIX)" t nil)

(autoload 'rtags-find-references-at-point "rtags" "\
Find all references to the symbol under the cursor
If there's exactly one result jump directly to it.
If there's more show a buffer with the different alternatives and jump to the first one if rtags-jump-to-first-match is true.
References to references will be treated as references to the referenced symbol

\(fn &optional PREFIX)" t nil)

(autoload 'rtags-find-virtuals-at-point "rtags" "\
List all reimplentations of function under cursor. This includes both declarations and definitions

\(fn &optional PREFIX)" t nil)

(autoload 'rtags-find-all-references-at-point "rtags" "\


\(fn &optional PREFIX)" t nil)

(autoload 'rtags-guess-function-at-point "rtags" "\


\(fn)" t nil)

(autoload 'rtags-rename-symbol "rtags" "\


\(fn)" t nil)

(autoload 'rtags-find-symbol "rtags" "\


\(fn &optional PREFIX)" t nil)

(autoload 'rtags-find-references "rtags" "\


\(fn &optional PREFIX)" t nil)

(autoload 'rtags-find-symbol-current-file "rtags" "\


\(fn)" t nil)

(autoload 'rtags-find-references-current-file "rtags" "\


\(fn)" t nil)

(autoload 'rtags-find-symbol-current-dir "rtags" "\


\(fn)" t nil)

(autoload 'rtags-find-references-current-dir "rtags" "\


\(fn)" t nil)

(autoload 'rtags-apply-fixit-at-point "rtags" "\


\(fn)" t nil)

(autoload 'rtags-clear-diagnostics-overlays "rtags" "\


\(fn)" t nil)

(autoload 'rtags-is-running "rtags" "\


\(fn)" t nil)

(autoload 'rtags-cycle-overlays-on-screen "rtags" "\


\(fn)" t nil)

(autoload 'rtags-fix-fixit-at-point "rtags" "\


\(fn)" t nil)

(autoload 'rtags-update-current-project "rtags" "\


\(fn)" t nil)

(autoload 'rtags-restart-tracking-timer "rtags" "\


\(fn)" t nil)

(autoload 'rtags-post-command-hook "rtags" "\


\(fn)" t nil)

(autoload 'rtags-stop-diagnostics "rtags" "\


\(fn)" t nil)

(autoload 'rtags-clear-diagnostics "rtags" "\


\(fn)" t nil)

(autoload 'rtags-diagnostics "rtags" "\


\(fn &optional RESTART NODIRTY)" t nil)

;;;***

;;;### (autoloads nil nil ("company-rtags.el" "rtags-ac.el" "rtags-pkg.el")
;;;;;;  (21497 57519 602349 0))

;;;***

(provide 'rtags-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; rtags-autoloads.el ends here
