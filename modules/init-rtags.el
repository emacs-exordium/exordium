;;;; Rtags - see `https://github.com/Andersbakken/rtags'
;;;
;;; Rtags keys use prefix C-c r
;;; ---------- ----------------------------------------------------------------
;;; Key        Function
;;; ---------- ----------------------------------------------------------------
;;; C-c r .    `rtags-find-symbol-at-point'
;;; M-.
;;; C-c r ,    `rtags-find-references-at-point'
;;; M-,
;;;
;;; C-c r >    `rtags-find-symbol' (prompts for symbol name)
;;; C-c r <    `rtags-find-references' (prompts for symbol name)
;;;
;;; M-C-g      List all buffer symbols with Helm
;;;
;;; ---------- ----------------------------------------------------------------
;;; C-c r v    `rtags-find-virtuals-at-point' list all impl. of function
;;; C-c r ;    `rtags-find-file' find file in project using partial name
;;;
;;; C-c r R    `rtags-rename-symbol'
;;; C-c r F    `rtags-fixit' fix the error using clang "did you mean".
;;;
;;; C-c r [    `rtags-location-stack-back' go back to previous location
;;; C-{
;;; C-c r ]    `rtags-location-stack-forward' the opposite
;;; C-}
;;;
;;; ---------- ----------------------------------------------------------------
;;;            `rtags-start': start rdm in a subprocess and start RTags
;;;            diagnostics.
;;;            `rtags-stop': kill rdm subprocess and RTags diagnostics.
;;; C-c r l    `rtags-show-rdm-buffer' show rdm log buffer.
;;;            `rtags-set-current-project' switch between projects
;;; C-c r e    `rtags-reparse-file' force recompile current buffer.
;;;
;;; ---------- ----------------------------------------------------------------
;;; C-c r D    `rtags-diagnostics' start diagnostics/force reparse
;;; C-c r Q    `rtags-stop-diagnostics' stop the diagnostic subprocess
;;; C-c r d    `rtags-show-diagnostics-buffer' toggle diag window
;;;            (without reparsing)
;;; C-c r down `rtags-next-diag' goes to the next problem.
;;; C-c r up   `rtags-previous-diag' goes to previous problem.
;;; C-c r c    `rtags-clear-diagnostics' clears any error or warning overlay.
;;;            `rtags-stop-diagnostics' stops the process.
;;;
;;; ---------- ----------------------------------------------------------------
;;; C-c r U    `rtags-print-cursorinfo' show what we know about symbol
;;; C-c r P    `rtags-print-dependencies' show all includes
;;; C-c r T    `rtags-taglist' show all tags in a window on left side
;;;
;;;
;;; Building rtags
;;; ==============
;;; $ git clone https://github.com/Andersbakken/rtags.git
;;; $ cd rtags
;;; $ git submodule init && git submodule update
;;; $ cmake .
;;; $ make
;;;
;;; Files
;;; =====
;;; Rtags uses the following files:
;;; `~/.rtags' (created automatically)
;;;     Where rdm stores its index files. They are reloaded when it restarts.
;;; `~/.rdmrc' (optional)
;;;     Config file for rdm (see rdm.cpp) containing default command line args.
;;; `.rtags-config' (optional, located in project root dir)
;;;     Project configuration file. Not needed if there is a .git or .svn at
;;;     the project root.
;;; `compile_commands.json' (optional, located in project root dir)
;;;     Compilation database for a given project, containing for each file the
;;;     clang command to build it. Not needed if you use RTags's compiler
;;;     wrapper scripts.
;;;
;;; Running rdm in a shell
;;; ======================
;;; Run `rdm' in a shell or in the background. Use -L to specify a log file.
;;; Use --help for the list of options. You can stop it gracefully with: rc -q
;;;
;;; You can control rdm with the rc client (use --help to see all options):
;;; $ rc -w
;;;     List the loaded projects and show the active one.
;;; $ rc -w proj
;;;     Make "proj" the active project ("proj" is a regex).
;;; $ rc -J
;;;     Reload the compilation DB from the current directory.
;;; $ rc -W proj
;;;     Delete project.
;;; $ rc --find-project-root /path/to/sourcefile.cpp
;;;     Print what it determines to be the correct project root.
;;; $ rc -T sourcefile.cpp
;;;     Say wether this component is indexed or not.
;;; $ rc -q
;;;     Shutdown rdm.
;;;
;;; Running rdm in Emacs
;;; ====================
;;; M-x `rtags-start'. A buffer will be created with rdm logs; you can show
;;; it with "C-c r l".
;;; M-x `rtags-stop' to kill it.
;;;
;;; Setting up a new project
;;; ========================
;;; 1. If the project root dir does not contain a .git or .svn repo, create a
;;;    file `.rtags-config' in the root dir with the specified content:
;;;    project: /path/to/project
;;;
;;; 2. The next step is to create the compilation database
;;;    `compile_commands.json'. For that, use CMake or use module
;;;     init-rtags-cdb.el.
;;;
;;; Diagnostics mode
;;; ================
;;; RTags diagnostics is a subprocess that highlight compilation errors and
;;; warnings in the code (using flymake). Click on a highlighted region to view
;;; the error message. Use "C-c r d" (lowercase d) to display the diagnostics
;;; buffer containing the error messages without forcing a reparsing of the
;;; current file.
;;;
;;; It is started by default, but you can control it with:
;;; - "C-c r D" or M-x `rtags-diagnostics' to start,
;;; - "C.c r q" or M-x `rtags-stop-diagnostics' to terminate the subprocess.

(with-no-warnings (require 'cl))
(require 'init-lib)
(require 'init-prefs)
(require 'rtags)
(require 'ac-rtags)
(require 'auto-complete-c-headers)
(require 'projectile)


;;; Key bindings

;; Enable default keys from rtags with prefix "Ctrl-C r"".
;; The default prefix is "Ctrl-x r" but almost all keys are bound;
;; "Ctrl-c r" is not defined by default, so we get the whole keyboard.
(rtags-enable-standard-keybindings c-mode-base-map "\C-cr")

(defun exordium-rtags-find-symbol-at-point (&optional prefix other-window)
  "Redefinition of `rtags-find-symbol-at-point' that returns t on
success and nil if not found. This implementation comes from
https://github.com/Andersbakken/rtags/blob/master/src/rtags.el c75467b"
  (interactive "P")
  (rtags-delete-rtags-windows)
  (rtags-location-stack-push)
  (let ((arg (rtags-current-location))
        (tagname (or (rtags-current-symbol) (rtags-current-token)))
        (fn (buffer-file-name))
        (found-it nil))
    (rtags-reparse-file-if-needed)
    (with-current-buffer (rtags-get-buffer)
      (rtags-call-rc :path fn :path-filter prefix "-f" arg)
      (cond ((or (not rtags-follow-symbol-try-harder)
                 (= (length tagname) 0))
             (setq found-it (rtags-handle-results-buffer nil nil fn other-window)))
            ((setq found-it (rtags-handle-results-buffer nil t fn other-window)))
            (t
             (erase-buffer)
             (rtags-call-rc :path fn "-F" tagname "--definition-only" "-M" "1" "--dependency-filter" fn :path-filter prefix
                            (when rtags-wildcard-symbol-names "--wildcard-symbol-names")
                            (when rtags-symbolnames-case-insensitive "-I"))
             (unless (setq found-it (rtags-handle-results-buffer nil nil fn other-window))
               (erase-buffer)
               (rtags-call-rc :path fn "-F" tagname "-M" "1" "--dependency-filter" fn :path-filter prefix
                              (when rtags-wildcard-symbol-names "--wildcard-symbol-names")
                              (when rtags-symbolnames-case-insensitive "-I"))
               (setq found-it (rtags-handle-results-buffer nil nil fn other-window)))))
      (recenter))
    found-it))

;; Alias for C-c r . This key recenters the buffer if needed.
(define-key c-mode-base-map "\M-."
  (lambda (other-window)
    (interactive "P")
    (exordium-rtags-find-symbol-at-point nil other-window)))

;; Alias for C-c r ,
(define-key c-mode-base-map "\M-," (function rtags-find-references-at-point))

;; Alias for C-c r [
(define-key c-mode-base-map [(control c) (r) (left)] (function rtags-location-stack-back))
;; Alias for C-c r [
(define-key c-mode-base-map [(control c) (r) (right)] (function rtags-location-stack-forward))

(define-key c-mode-base-map [(meta control g)] (function rtags-imenu))

(define-key c-mode-base-map [(control c) (r) (down)] (function rtags-next-diag))
(define-key c-mode-base-map [(control c) (r) (up)] (function rtags-previous-diag))
(define-key c-mode-base-map [(control c) (r) (c)] (function rtags-clear-diagnostics))

(define-key c-mode-base-map "\C-crQ" (function rtags-stop-diagnostics))


;;; Start rdm as a subprocess, with output in a buffer

(defun exordium-rtags-start-rdm-maybe ()
  "Start rdm if not already running. Return t if started and nil
otherwise."
  (unless (exordium-rtags-rdm-running-p)
    (exordium-rtags-start-rdm-impl nil)
    t))

(defun exordium-rtags-rdm-running-p ()
  "Predicate testing if rdm is running"
  (let ((process (get-process "rdm")))
    (or
     ;; Rdm runs in a process started from Emacs
     (and (processp process)
          (not (eq (process-status process) 'exit))
          (not (eq (process-status process) 'signal)))
     ;; User has started rdm outside of Emacs
     ;; Note: sadly this does not work on macOS
     (let ((uuid (user-uid)))
       (dolist (pid (reverse (list-system-processes)))
         (let* ((attrs (process-attributes pid))
                (pname (cdr (assoc 'comm attrs)))
                (puid  (cdr (assoc 'euid attrs))))
           (when (and (eq puid uuid)
                      (string= pname "rdm"))
              (return t))))))))

(defun exordium-rtags-start-rdm-impl (&optional open-buffer)
  "Start rdm in a subprocess. Open the rdm log buffer if
open-buffer is true."
  (let ((buffer (get-buffer-create "*RTags rdm*")))
    (when open-buffer
      (switch-to-buffer buffer))
    (with-current-buffer buffer
      (rtags-rdm-mode)
      (read-only-mode))
    (let ((process (if exordium-rtags-rdm-args
                       (start-process "rdm" buffer "rdm" exordium-rtags-rdm-args)
                       (start-process "rdm" buffer "rdm"))))
      (message "Started rdm - PID %d" (process-id process))))
  ;; Add RTags to company backends
  (when (and (eq exordium-complete-mode :company)
             (not (member 'company-rtags company-backends)))
    (push 'company-rtags company-backends)))

(defun rtags-start ()
  "Start the rdm deamon in a subprocess and display output in a
buffer. Also start the RTag diagostics mode."
  (interactive)
  (setq rtags-autostart-diagnostics t)
  (exordium-rtags-start-rdm-impl t))

(defun rtags-stop ()
  "Stop both RTags diagnostics and rdm, if they are running."
  (interactive)
  ;; Remove RTags from company backends
  (when (and (eq exordium-complete-mode :company)
             (member 'company-rtags company-backends))
    (setq company-backends (delete 'company-rtags company-backends)))
  ;; Stop RTags Diagnostics and kill its buffer without prompt
  (when (and rtags-diagnostics-process
             (not (eq (process-status rtags-diagnostics-process) 'exit)))
    (kill-process rtags-diagnostics-process))
  (when (get-buffer "*RTags Diagnostics*")
    (let ((kill-buffer-query-functions nil))
      (kill-buffer "*RTags Diagnostics*")))
  ;; Stop rdm and kill its buffer without prompt
  (rtags-quit-rdm)
  (when (get-buffer "*RTags rdm*")
    (let ((kill-buffer-query-functions nil))
      (kill-buffer "*RTags rdm*"))))

(defun rtags-show-rdm-buffer ()
  "Show/hide the rdm log buffer"
  (interactive)
  (let* ((buffer-name "*RTags rdm*")
         (buffer (get-buffer buffer-name))
         (window (and buffer (get-buffer-window buffer))))
    (cond (window
           (bury-buffer buffer)
           (delete-window window))
          (buffer
           (display-buffer buffer))
          (t
           (message "Rtags rdm is not running (use M-x rtags-start)")))))

(define-key c-mode-base-map [(control c)(r)(l)] 'rtags-show-rdm-buffer)


;;; Mode for rdm log output
;;; See http://ergoemacs.org/emacs/elisp_syntax_coloring.html

(defsubst rtags-rdm-record-search-forward (&optional regexp bound)
  "Search forward from point for a log line matching REGEXP.
Set point to the end of the occurrence found, and return point.
An optional second argument BOUND bounds the search: the match
found must not extend after that position. This function also
sets `match-data' to the entire match."
  (let ((org-pos (point)))
    (block while-loop
      ;; While there are more matches for REGEXP
      (while (re-search-forward regexp bound t)
        (if (re-search-backward "^" org-pos t)
            (let ((begin-pos (point)))
              ;; If we found a matching log line, set match data and return
              (if (re-search-forward "$" bound t)
                  (progn
                    (set-match-data (list begin-pos (point)))
                    (return-from while-loop (point)))
                (return-from while-loop))))))))

(defun rtags-rdm-match-record-error (bound)
  "Search forward from point to BOUND for error."
  (rtags-rdm-record-search-forward "\\(error:\\)" bound))

(defun rtags-rdm-match-record-warning (bound)
  "Search forward from point to BOUND for warning."
  (rtags-rdm-record-search-forward "\\(warning:\\)" bound))

(defun rtags-rdm-match-record-note (bound)
  "Search forward from point to BOUND for note."
  (rtags-rdm-record-search-forward "\\(note:\\)" bound))

(defun rtags-rdm-match-record-done (bound)
  "Search forward from point to BOUND for Jobs."
  (rtags-rdm-record-search-forward "\\(Jobs\\)" bound))

(defconst rtags-rdm-mode-keywords
  (list '(rtags-rdm-match-record-error 0 'compilation-error)
        '(rtags-rdm-match-record-warning 0 'compilation-warning)
        '(rtags-rdm-match-record-note 0 'compilation-info)
        '(rtags-rdm-match-record-done 0 'underline))
  "Describes how to syntax highlight keywords in rtags-rdm-mode.")

(defconst rtags-rdm-mode-syntax-table
  ;; Defines a "comment" as anything that starts with a square bracket, e.g.
  ;; [100%] /path/to/file.cpp in 437ms. (1259 syms, etc) (dirty)
  (let ((synTable (make-syntax-table)))
    (modify-syntax-entry ?\[ "< b" synTable)
    (modify-syntax-entry ?\n "> b" synTable)
    synTable))

(define-derived-mode rtags-rdm-mode fundamental-mode
  "rdm-log"
  "Mode for viewing rdm logs"
  :syntax-table rtags-rdm-mode-syntax-table
  ;; Syntax highlighting:
  (setq font-lock-defaults '(rtags-rdm-mode-keywords t t)))


;;; Using the diagnostics buffer

(defun rtags-show-diagnostics-buffer ()
  "Show/hide the diagnostics buffer in a dedicated
window (similar to `rtags-diagnostics' but without reparsing)."
  (interactive)
  (if (rtags-has-diagnostics)
      (let* ((buffer-name "*RTags Diagnostics*")
             (buffer (get-buffer buffer-name))
             (window (get-buffer-window buffer)))
        (cond (window
               (bury-buffer buffer)
               (delete-window window))
              (buffer
               (display-buffer buffer-name)
               (other-window 1)
               (goto-char (point-min))
               (fit-window-to-buffer (get-buffer-window (current-buffer)) 10 2)
               (set-window-dedicated-p (get-buffer-window (current-buffer)) t)
               (other-window -1))))
    (message "Rtags diagnostics is not running (use C-c r D)")))

(define-key c-mode-base-map [(control c)(r)(d)] 'rtags-show-diagnostics-buffer)

;; Used in powerline:
(defun rtags-diagnostics-has-errors ()
  "Return t or nil depending if RTags diagnostics displays errors"
  (let ((diag-buff (get-buffer "*RTags Diagnostics*")))
    (if (and diag-buff
             rtags-diagnostics-process
             (not (eq (process-status rtags-diagnostics-process) 'exit))
             (not (eq (process-status rtags-diagnostics-process) 'signal)))
        (> (buffer-size diag-buff) 0)
      nil)))


;;; RTags auto-complete (EXPERIMENTAL)
;;; FIXME: this is broken, need to revisit the whole thing.

;;; AC source for #include

;;; The following function fixes a bug in achead:documentation-for-candidate
(defun my-documentation-for-candidate (candidate)
  "Generate documentation for a candidate `candidate'. For now,
just returns the path and content of the header file which
`candidate' specifies."
  (let ((path
         (assoc-default candidate achead:ac-latest-results-alist 'string=)))
    (ignore-errors
      (with-temp-buffer
        (insert path)
        (unless (file-directory-p path)
          (insert "\n--------------------------\n")
          (insert-file-contents path nil 0 200)) ;; first 200 content bytes
        (buffer-string)))))

(ac-define-source my-c-headers
  `((init       . (setq achead:include-cache nil))
    (candidates . achead:ac-candidates)
    (prefix     . ,achead:ac-prefix)
    (document   . my-documentation-for-candidate)
    (requires   . 0)
    (symbol     . "h")
    (action     . ac-start)
    (limit      . nil)))

;;; AC source for RTags

(defun ac-rtags-init ()
  (unless rtags-diagnostics-process
    (rtags-diagnostics)))

(ac-define-source my-rtags
  '((init       . rtags-ac-init)
    (prefix     . rtags-ac-prefix)
    (candidates . rtags-ac-candidates)
    (action     . rtags-ac-action)
    (document   . rtags-ac-document)
    (requires   . 0)
    (symbol     . "r")))

;;; Functions to enable auto-complete

(defun rtags-auto-complete ()
  "Enables auto-complete with RTags.
Note that RTags becomes the only source for auto-complete in all
C and C++ buffers. Also note that RTags Diagostics must be turned
on."
  (interactive)
  (require 'ac-rtags)
  (setq rtags-completions-enabled t)
  (add-hook 'c++-mode-hook
            (lambda ()
              (setq ac-sources '(ac-source-my-rtags)))))

(defun rtags-diagnostics-auto-complete ()
  "Starts diagnostics and auto-complete with RTags and #includes.
Note that this function replaces all other sources of auto-complete
 for C++ files. Any previously opened C++ file needs to be reopen
for this to be effective."
  (interactive)
  ;; Require
  ;; Start RTags diagnostics
  (unless rtags-diagnostics-process
    (rtags-diagnostics))
  ;; FIXME: this is broken, should not depend on compile_includes
  ;; Create an auto-complete source for headers using compile_includes
  ;; (let ((plist (rtags-load-compile-includes-file (projectile-project-root))))
  ;;   (dolist (dir (plist-get plist :src-dirs))
  ;;     (add-to-list 'achead:include-directories dir))
  ;;   (dolist (dir (plist-get plist :include-dirs))
  ;;     (add-to-list 'achead:include-directories dir)))
  ;; Turn on RTags auto-complete
  (setq rtags-completions-enabled t)
  (add-hook 'c++-mode-hook
            (lambda ()
              (setq ac-sources '(ac-source-my-rtags
                                 ;;ac-source-my-c-headers
                                 )))))

(define-key c-mode-base-map [(control c)(r)(A)]
  'rtags-diagnostics-auto-complete)


(provide 'init-rtags)
;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:
