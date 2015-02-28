;;;; Rtags - see `https://github.com/Andersbakken/rtags'
;;;
;;; Note: we use C-c r as prefix for rtags keys in order to get more choices.
;;; --------- -----------------------------------------------------------------
;;; Keys      Function
;;; --------- -----------------------------------------------------------------
;;; C-c r .  `rtags-find-symbol-at-point'
;;; M-.
;;; C-c r ,  `rtags-find-references-at-point'
;;; M-,
;;;
;;; C-c r >  `rtags-find-symbol' (prompts for symbol name)
;;; C-c r <  `rtags-find-references' (prompts for symbol name)
;;; C-c r I  `rtags-imenu' Ido-based select a symbol in current file
;;; M-C-g
;;; C-c r v  `rtags-find-virtuals-at-point' list all impl. of function
;;; C-c r ;  `rtags-find-file' find file in project using partial name
;;;
;;; C-c r R  `rtags-rename-symbol'
;;; C-c r F  `rtags-fixit' fix the error using clang "did you mean".
;;;
;;; C-c r [  `rtags-location-stack-back' go back to previous location
;;; C-{
;;; C-c r ]  `rtags-location-stack-forward' the opposite
;;; C-}
;;;
;;;          `rtags-start-rdm' in a subprocess.
;;;          `rtags-quit-rdm' kill rdm subprocess.
;;; C-c r l  `rtags-show-rdm-buffer' show rdm log buffer.
;;;          `rtags-set-current-project' switch between projects
;;; C-c r e  `rtags-reparse-file' force recompile current buffer.
;;;
;;; C-c r D  `rtags-diagnostics' start/show diagnostics buffer
;;; C-c r Q  `rtags-stop-diagnostics' stop the diagnostic subprocess
;;; C-c r d  `rtags-show-diagnostics-buffer' (without reparsing)
;;;          `rtags-next-diag' goes to the next problem.
;;;          `rtags-clear-diagnostics' clears any error or warning overlay.
;;;          `rtags-stop-diagnostics' stops the process.
;;;
;;; C-c r U  `rtags-print-cursorinfo' show what we know about symbol
;;; C-c r P  `rtags-print-dependencies' show all includes
;;; C-c r T  `rtags-taglist' show all tags in a window on left side
;;;
;;;          `rtags-create-compilation-database' see doc below
;;; ------- -------- ----------------------------------------------------------
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
;;;     clang command to build it. Not needed if you use the compiler wrapper
;;;     scripts. Use `rtags-create-compilation-database' to generate it.
;;; `compile_includes' (optional, located in project root dir)
;;;     Directives to create a compilation database with
;;;     `rtags-create-compilation-database'.
;;;
;;; Running rdm
;;; ===========
;;; If you don't want to run rdm as an Emacs subprocess, run `rdm' in a
;;; separate window or in the background. Use -L to specify a log file. Use
;;; --help for the list of options. You can stop it gracefully with "rc -q".
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
;;; There are 2 ways to create an index:
;;;
;;; 1. Building the project using the compiler wrapper scripts.
;;;    The wrapper will tell rdm to parse and index each compilation unit
;;;    before it gets compiled.
;;;    Advantage: the easiest way; all you need to do is to build.
;;;    Inconvenient: you need to build before you can use the latest index,
;;;    and any unused header won't be indexed.
;;;
;;; 2. Create a compilation database JSON file in the project root dir.
;;;    See `http://clang.llvm.org/docs/JSONCompilationDatabase.html'.
;;;    use "rc -J" to reload it.
;;;
;;; The rest of this documentation assumes we use a compilation database with
;;; one or multiple projects.
;;;
;;; Running rdm in Emacs
;;; ====================
;;; M-x `rtags-start-rdm'. A buffer will be created with rdm logs; you can show
;;; it with "C-c r l".
;;; M-x `rtags-quit-rdm' to kill it.
;;;
;;; Setting up a new project
;;; ========================
;;; 1. If the project root dir does not contain a .git or .svn repo, create a
;;; file `.rtags-config' in the root dir with the specified content: project:
;;; /path/to/project
;;;
;;; 2. The next step is to create the compilation database
;;; `compile_commands.json', which tells rdm how to compile each individual
;;; file in your project. Each entry in the file looks like this (simplified
;;; for clarity):
;;;
;;;   { "directory": "/home/phil/workspaces/foo/",
;;;     "command":   "/usr/bin/clang++ -Irelative
;;;                   -I/home/phil/workspaces/bde/groups/bsl/bsl+stdhdrs
;;;                   -I/home/phil/workspaces/bde/groups/bsl/bslma
;;;                   -I/home/phil/workspaces/bde/groups/bsl/bsls
;;;                   -c -o bar.o bar.cpp",
;;;     "file":      "bar.cpp" },
;;;
;;; First, create a file `compile_includes' in the project root dir, which
;;; specifies how to compile your project and in particular where are all the
;;; source files and all the include files. For example:
;;;
;;;   # Compile_includes files for project foo
;;;   # Pattern to exclude in -I directives and for looking for sources:
;;;   exclude /test$
;;;   exclude /doc$
;;;   exclude /group$
;;;   exclude /package$
;;;
;;;   # Where are the source files (there could be multiple directories).
;;;   # We will scan recursively any subdirectories that do not match any
;;;   # 'exclude' regex.
;;;   src .
;;;
;;;   # What to put in -I directives (in addition to the source files above).
;;;   # We will scan recursively any subdirectories that do not match any
;;;   # 'exclude' regex.
;;;   include /Users/phil/Code/cpp/include/bsl
;;;   include /Users/phil/Code/cpp/include/bdl
;;;
;;;   # If any file name pattern must be excluded from the "src" files, use
;;;   # the "excludesrc" directive. For example this will exclude all test
;;;   # drivers:
;;;   excludesrc \.t\.cpp$
;;;
;;; In addition, the creation of a compilation database uses these variables:
;;; * `*rtags-compile-includes-base-dir*': set this to your workspace path
;;;   if you want to use relative paths in `compile_includes' (by default any
;;;   relative path in this file is relative to the project root dir).
;;; * `*rtags-clang-command-prefix*': default is "/usr/bin/clang++ -Irelative"
;;;   (Note that rtags ignores the clang++ command because it uses libclang).
;;; * `*rtags-clang-command-suffix*': default is "-c -o".
;;;
;;; Once you have created the `compile_includes' file, run the command
;;; M-x `rtags-create-compilation-database'. It will:
;;; - Prompt for the project root dir
;;; - Scan all source dirs and include dirs
;;; - Create `compilation_database.json' (it overwrites without asking)
;;; - Ask if you want to reload it (if rdm is running).
;;;
;;; Diagnostics mode
;;; ================
;;; "C-c r D" or M-x `rtags-diagnostics' starts a subprocess that highlight
;;; compilation errors and warnings in the code (using flymake). Click on a
;;; highlighted region to view the error message. Use "C-c r d" (lowercase d)
;;; to display the diagnostics buffer containing the error messages without
;;; forcing a reparsing of the current file.
;;; M-x `rtags-stop-diagnostics' to terminate the subprocess.

(require 'init-lib)
(require 'rtags)
(require 'rtags-ac)
(require 'auto-complete-c-headers)


;;; Key bindings

;; Enable default keys from rtags with prefix "Ctrl-C r"".
;; The default prefix is "Ctrl-x r" but almost all keys are bound;
;; "Ctrl-c r" is not defined by default, so we get the whole keyboard.
(rtags-enable-standard-keybindings c-mode-base-map "\C-cr")

;; Alias keys for common operations
(define-key c-mode-base-map "\M-."
  (lambda ()
    (interactive)
    (rtags-find-symbol-at-point)
    (recenter-top-bottom)))

(define-key c-mode-base-map "\M-," (function rtags-find-references-at-point))
(define-key c-mode-base-map [(meta control g)] (function rtags-imenu))
(define-key c-mode-base-map [(control {)] (function rtags-location-stack-back))
(define-key c-mode-base-map [(control })] (function rtags-location-stack-forward))
(define-key c-mode-base-map "\C-crQ" (function rtags-stop-diagnostics))


;;; Start rdm as a subprocess, with output in a buffer

(defun rtags-start-rdm ()
  "Start the rdm deamon in a subprocess and display output in a
buffer"
  (interactive)
  (let ((buffer (get-buffer-create "*RTags rdm*")))
    (switch-to-buffer buffer)
    (rtags-rdm-mode)
    (let ((process (start-process "rdm" buffer "rdm")))
      (message "Started rdm - PID %d" (process-id process)))))

(defun rtags-show-rdm-buffer ()
  "Show the rdm log buffer"
  (interactive)
  (let ((buffer-name "*RTags rdm*"))
    (if (get-buffer buffer-name)
        (display-buffer buffer-name)
      (message "Rtags rdm is not running (use M-x rtags-start-rdm)"))))

(define-key c-mode-base-map [(control c)(r)(l)] 'rtags-show-rdm-buffer)

;; Mode for rdm log output
;; See http://ergoemacs.org/emacs/elisp_syntax_coloring.html

(defconst rtags-rdm-mode-keywords
  ;; Words and associated face.
  `((,(regexp-opt '("error" "warn")    'words) . font-lock-warning-face)
    (,(regexp-opt '("Jobs" "Restored") 'words) . font-lock-string-face)))

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
  (setq font-lock-defaults '((rtags-rdm-mode-keywords))))


;;; Display the diagnostics buffer without force reparsing

(defun rtags-show-diagnostics-buffer ()
  "Show/hide the diagnostics buffer in a dedicated
window (similar to `rtags-diagnostics' but without reparsing)."
  (interactive)
  (let ((buffer-name "*RTags Diagnostics*"))
    (cond ((get-buffer-window (get-buffer buffer-name))
           (bury-buffer (get-buffer buffer-name))
           (delete-window (get-buffer-window (get-buffer buffer-name))))
          ((get-buffer buffer-name)
           (display-buffer buffer-name)
           (other-window 1)
           (beginning-of-buffer)
           (fit-window-to-buffer (get-buffer-window (current-buffer)) 10 2)
           (set-window-dedicated-p (get-buffer-window (current-buffer)) t)
           (other-window -1))
          (t
           (message "Rtags diagnostics is not running (use C-c r D)")))))

(define-key c-mode-base-map [(control c)(r)(d)] 'rtags-show-diagnostics-buffer)


;;; Create a compilation database

;; Override these variables in your .emacs as needed:

(defvar *rtags-clang-command-prefix*
  "/usr/bin/clang++ -Irelative "
  "Compilation command prefix to use for creating compilation
  databases. Override this variable for your local environment.")

(defvar *rtags-clang-command-suffix*
  " -c -o "
  "Compilation command suffix to use for creating compilation
  databases. Override this variable for you local environment.")

(defvar *rtags-compile-includes-base-dir*
  nil
  "If non-nil, base directory to use for all relative paths in
  `compile_include'. Use nil for absolute paths.")

;; Do not set these variables in your .emacs, they are generated:

(defvar *rtags-project-source-dirs* ()
  "List of directories containing the source and header files of
  the current project. We scan these directories for .cpp files
  to put into the compilation database.")

(defvar *rtags-project-include-dirs* ()
  "List of directories to include for compiling the current
  project (e.g. third party libraries). We use these directories
  to generate -I directives that the clang compilation command
  needs.")

(defvar *rtags-project-exclude-files* ()
  "List of regex to exclude while searching for .cpp files for
  the current project.")

(defun rtags-load-compile-includes-file-content (compile-includes-file)
  "Read and parse the specified compile-includes file, and return
a list of 4 sublists:
- The list of src directives,
- The list of include directives,
- The list of exclude directives,
- The list of excludesrc directives."
  (let ((line-number      1)
        (value            nil)
        (src-list         ())
        (include-list     ())
        (exclude-list     ())
        (exclude-src-list ()))
    (dolist (record (pg/read-file-lines compile-includes-file))
      (incf line-number)
      (setq value (second (split-string record " ")))
      (cond ((or (eq "" record)
                 (pg/string-starts-with record "#"))
             nil) ; comment or empty string; skip it
            ((pg/string-starts-with record "src")
             (when value
               (setq src-list (cons value src-list))))
            ((pg/string-starts-with record "include")
             (when value
               (setq include-list (cons value include-list))))
            ((pg/string-starts-with record "excludesrc")
             (when value
               (setq exclude-src-list (cons value exclude-src-list))))
            ((pg/string-starts-with record "exclude")
             (when value
               (setq exclude-list (cons value exclude-list))))
            (t
             (error "Syntax error line %d: %s" line-number record))))
    (list src-list include-list exclude-list exclude-src-list)))

(defun rtags-is-excluded-p (path excluded-regexs)
  "Return non-nil if the specified path matches any regex in
the list of excluded regexs"
  (catch 'return
    (dolist (excluded excluded-regexs)
      (when (string-match excluded path)
        (throw 'return t)))
    (throw 'return nil)))

(defun rtags-scan-include-directories (dir excluded-regexs)
  "Return a list of subdirectories under the specified root dir,
excluding any that match any regex in the specified excluded
regex list."
  (let ((result ()))
    (dolist (subdir (cons dir (pg/directory-tree dir)))
      (unless (rtags-is-excluded-p subdir excluded-regexs)
        (setq result (cons subdir result))))
    result))

(defun rtags-load-compile-includes-file (dir)
  "Loads the `compile_includes' file from the specified directory
and sets up the project's source dirs and include dirs. Return
true on success. Normally you should not use this function
directly: use `rtags-create-compilation-database' instead"
  (interactive "DProject root: ")
  (let ((compile-includes-file (concat (file-name-as-directory dir)
                                       "compile_includes")))
    (cond ((file-exists-p compile-includes-file)
           ;; Parse the file and return 3 lists: src, include, exclude
           (let ((directives (rtags-load-compile-includes-file-content
                              compile-includes-file)))
             (setq *rtags-project-source-dirs*   ()
                   *rtags-project-include-dirs*  ()
                   *rtags-project-exclude-files* ())
             (let ((source-dirs (first directives))
                   (incl-dirs   (second directives))
                   (excl-regexs (third directives)))
               ;; Set the source exclude patterns
               (setq *rtags-project-exclude-files* (fourth directives))
               ;; Scan src to get all subdirs that do not match the excludes
               (dolist (path source-dirs)
                 (unless (file-name-absolute-p path)
                   (setq path (expand-file-name path
                                                (or *rtags-compile-includes-base-dir*
                                                    dir))))
                 (message "Scanning source dir: %s ..." path)
                 (setq *rtags-project-source-dirs*
                       (append *rtags-project-source-dirs*
                               (rtags-scan-include-directories path excl-regexs))))
               ;; Same with includes
               (dolist (path incl-dirs)
                 (setq path (expand-file-name path *rtags-compile-includes-base-dir*))
                 (message "Scanning include dir: %s ..." path)
                 (setq *rtags-project-include-dirs*
                       (append *rtags-project-include-dirs*
                               (rtags-scan-include-directories path excl-regexs))))
               ;; Done
               (message "Project has %d source dirs and %d include dirs"
                        (length *rtags-project-source-dirs*)
                        (length *rtags-project-include-dirs*))))
           t)
          (t
           (message "No compilation_includes file")
           nil))))

(defun rtags-create-compilation-command ()
  "Return a string containing the clang compilation command to
  use for the compilation database, using the content of
  `*rtags-project-source-dirs*' and `*rtags-project-include-dirs*'"
  (assert *rtags-project-source-dirs*)
  (let ((command *rtags-clang-command-prefix*))
    (dolist (path *rtags-project-source-dirs*)
      (setq command (concat command " -I" path)))
    (dolist (path *rtags-project-include-dirs*)
      (setq command (concat command " -I" path)))
    (concat command *rtags-clang-command-suffix*)))

(defun rtags-create-compilation-database (dir)
  "Regenerates `compile_commands.json' in the specified
directory"
  (interactive "DProject root: ")
  (when (rtags-load-compile-includes-file dir)
    (let ((dbfilename (concat (file-name-as-directory dir)
                              "compile_commands.json"))
          (compile-command (rtags-create-compilation-command))
          (num-files 0))
      (with-temp-buffer
        (insert "[")
        (newline)
        ;; Note: dynamic bunding of default-directory
        (dolist (default-directory *rtags-project-source-dirs*)
          (message "Processing directory: %s ..." default-directory)
          (let ((files (file-expand-wildcards "*.cpp"))
                ;; rdm does not like directories starting with "~/"
                (dirname (if (pg/string-starts-with default-directory "~/")
                             (substitute-in-file-name
                              (concat "$HOME/" (substring default-directory 2)))
                           default-directory)))
            (dolist (file files)
              (unless (rtags-is-excluded-p file *rtags-project-exclude-files*)
                (incf num-files)
                (insert "  { \"directory\": \"" dirname "\",")
                (newline)
                (insert "    \"command\":   \""
                        compile-command
                        (file-name-sans-extension file) ".o "
                        file "\",")
                (newline)
                (insert "    \"file\":      \"" file "\" },")
                (newline)))))
        (insert "];")
        (newline)
        (write-region (buffer-string) nil dbfilename))
      (when (yes-or-no-p
             (format "Wrote compile_commands.json (%d files). Reload it?" num-files))
        (rtags-call-rc :path t :output nil "-J" dir)
        (message "Reloaded (check rdm's logs)")))))

;; Mode for compile_includes

(defconst rtags-compile-includes-mode-keywords
  ;; Words and associated face.
  `((,(regexp-opt '("src" "include" "exclude" "excludesrc") 'words)
     . font-lock-keyword-face)))

(defconst rtags-compile-includes-mode-syntax-table
  ;; Defines a "comment" as anything that starts with hash tag
  (let ((synTable (make-syntax-table)))
    (modify-syntax-entry ?\# "< b" synTable)
    (modify-syntax-entry ?\n "> b" synTable)
    synTable))

(define-derived-mode rtags-compile-includes-mode fundamental-mode
  "compile-includes"
  "Mode for editing compile_includes files"
  :syntax-table rtags-compile-includes-mode-syntax-table
  ;; Syntax highlighting:
  (setq font-lock-defaults '((rtags-compile-includes-mode-keywords))))

(add-to-list 'auto-mode-alist
             '("compile_includes" . rtags-compile-includes-mode))


;;; RTags auto-complete

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

(defun rtags-ac-init ()
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
  (require 'rtags-ac)
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
  ;; Create an auto-complete source for headers using compile_includes
  (rtags-load-compile-includes-file (projectile-project-root))
  (dolist (dir *rtags-project-source-dirs*)
    (add-to-list 'achead:include-directories dir))
  (dolist (dir *rtags-project-include-dirs*)
    (add-to-list 'achead:include-directories dir))
  ;; Turn on RTags auto-complete
  (setq rtags-completions-enabled t)
  (add-hook 'c++-mode-hook
            (lambda ()
              (setq ac-sources '(ac-source-my-rtags ac-source-my-c-headers)))))

(define-key c-mode-base-map [(control c)(r)(A)]
  'rtags-diagnostics-auto-complete)


(provide 'init-rtags)
