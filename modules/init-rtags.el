;;;; Rtags - see `https://github.com/Andersbakken/rtags'
;;;
;;; -------------- -------------------------------------------------------
;;; Key            Definition
;;; -------------- -------------------------------------------------------
;;; F3
;;; C-x r .        Find symbol at point
;;; F4
;;; C-x r ,        Find references at point
;;; C-x r >        Find symbol (prompts for symbol name)
;;; C-x r <        Find references (prompts for symbol name)
;;; C-x r R        Rename symbol
;;; M-C-g
;;; C-x r I        Ido-based select a symbol in the current file
;;; C-x r v        Find virtuals at point
;;; C-x r q        Show diagnostics buffer (without reparsing)
;;; -------------- -------------------------------------------------------
;;;
;;; Whenever rtags jumps somewhere it pushes a location onto its stack. Jump
;;; back and forward in this stack with M-left and M-right (or default
;;; C-x r [ and R-x r ] )
;;;
;;; Functions:
;;; `rtags-find-file': jump to file by name (full or partial)
;;; `rtags-print-cursorinfo': print debugging info about symbol at point
;;; `rtags-print-dependencies': show all include files (recursively)
;;; `rtags-diagnostics': starts an async process to receive warnings or errors
;;;     from clang; integrates with flymake to put highlighting on code with
;;;     warnings and errors.
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
;;;     Index files which are reloaded when `rdm' restarts.
;;; `~/.rdmrc' (optional)
;;;     Config file for rdm (see rdm.cpp) containing default command line args.
;;; `compile_commands.json' (optional, located in project root dir)
;;;     Compilation database for a given project, containing for each file the
;;;     clang command to build it. Not needed if you use the compiler wrapper
;;;     scripts.
;;; `.rtags-config' (optional, located in project root dir)
;;;     Project configuration file. Not needed if there is a .git or .svn at
;;;     the project root.
;;;
;;; In addition, this module uses an optional file `compile_includes' located
;;; at project root, containing a list of include root directories to use for
;;; the clang command (one directory per line).
;;;
;;; Running rdm
;;; ===========
;;; First, run `rdm' in a separate window or in the background. Use -L to
;;; specify a log file. Use --help for the list of options. You can stop it
;;; gracefully with "rc -q".
;;;
;;; There are 2 ways to create an index:
;;;
;;; 1. Building the project using the compiler wrapper scripts.
;;;    The wrapper will tell rdm to parse and index each compilation unit
;;;    before it gets compiled.
;;;    Plus: the easiest way; all you need to do is to build.
;;;    Minus: you need to build before you can use the latest index, and any
;;;    unused header won't be indexed.
;;;
;;; 2. Create a compilation database JSON file in the project root dir.
;;;    See `http://clang.llvm.org/docs/JSONCompilationDatabase.html'.
;;;    use "rc -J" to reload it.
;;;
;;; The rest of this documentation assumes we use a compilation database with
;;; multiple projects. Note that there is only one index, so any project can
;;; use the symbols of another one (e.g. no need to reindex common libraries
;;; multiple times).
;;;
;;; Setting up a new project
;;; ========================
;;; If the project root dir does not contain a .git or .svn repo, create a file
;;; `.rtags-config' in the root dir with the specified content:
;;; project: /path/to/project
;;;
;;; The next step is to create the compilation database
;;; `compile_commands.json', which tells rdm how to compile each individual
;;; file in your project. Each entry in the file looks like this (simplified
;;; for clarity):
;;;
;;; { "directory": "/home/phil/workspaces/foo/",
;;;   "command":   "/usr/bin/clang++ -Irelative
;;;                 -I/home/phil/workspaces/bde/groups/bsl/bsl+stdhdrs
;;;                 -I/home/phil/workspaces/bde/groups/bsl/bslma
;;;                 -I/home/phil/workspaces/bde/groups/bsl/bsls
;;;                 -c -o bar.o bar.cpp",
;;;   "file":      "bar.cpp" },
;;;
;;; You can generate this file with this command:
;;; M-x `create-compilation-database'.
;;;
;;; This command will first prompt for a directory (use the project's root
;;; directory). It will then scan recursively for any directory in your project
;;; as well as any external include directory indicated as a dependency in a
;;; file `compile_includes', if such file exists. It will then generate the
;;; compilation database file.
;;;
;;; If the file `compile_includes' exists, it must contain directories to scan
;;; for include files, with one path per line. For example:
;;;
;;; bde/groups/bsl
;;; bde/groups/bdl
;;;
;;; Note that these directories come in addition to the project's own directory
;;; tree (you should only use it for external dependencies). Also note that
;;; `create-compilation-database' will recursively scan any directory in this
;;; file. You can specify subdirectories to exclude by setting this variable in
;;; your `init-local.el':
;;;
;;; (setq *rtags-clang-exclude-directories*
;;;       '("/group" "/doc" "/package" "/test"))
;;;
;;; If you want the paths in `compile_includes' to be relative, you can set a
;;; prefix to be added to each path in your `init-local.el' like so:
;;;
;;; (setq *rtags-clang-include-dir-prefix* "/home/phil/workspaces/")
;;;
;;; To control the clang++ command that is put into the compilation database,
;;; you can also set a few variables in your `init_local.el':
;;;
;;; (setq *rtags-clang-command-prefix* "/usr/bin/clang++ -Irelative ")
;;; (setq *rtags-clang-command-suffix* " -c -o ")
;;;
;;; Once the compilation database file is ready, tell rdm to reload it with "rc
;;; -J". Watch for errors in rdm's logs. It may crash a few times on a file and
;;; finally give up with that file.
;;;
;;; View the list of projects in the index with "rc -w". Switch to another
;;; project with "rc -w <proj>".
;;;
;;; Using `rc' to control the index
;;; ===============================
;;; (use --help to see all options)
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

(require 'init-prolog)
(require 'rtags)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Key bindings

;; Default keys from rtags (with default prefix)
(rtags-enable-standard-keybindings c-mode-base-map)

;; Alias keys for common operations
(define-key c-mode-base-map (kbd "<f3>") (function rtags-find-symbol-at-point))
(define-key c-mode-base-map (kbd "<f4>") (function rtags-find-references-at-point))
(define-key c-mode-base-map [(meta control g)] (function rtags-imenu))
(define-key c-mode-base-map [(meta left)] (function rtags-location-stack-back))
(define-key c-mode-base-map [(meta right)] (function rtags-location-stack-forward))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Display the diagnostics buffer without force reparsing

(defun rtags-show-diagnostics-buffer ()
  "Show the diagnostics buffer (same as `rtags-diagnostics' but
without reparsing)"
  (interactive)
  (let ((buffer-name "*RTags Diagnostics*"))
    (if (get-buffer buffer-name)
        (display-buffer buffer-name)
      (message "Rtags diagnostic is not running"))))
(define-key c-mode-base-map [(control x)(r)(q)] 'rtags-show-diagnostics-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(defun rtags-load-compile-include-file-content (compile-includes-file)
  "Read and parse the specified compile-includes file, and return
a list of 3 sublists:
- The list of src directives
- The list of include directives
- The list of exclude directive."
  (let ((line-number  1)
        (value        nil)
        (src-list     ())
        (include-list ())
        (exclude-list ()))
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
            ((pg/string-starts-with record "exclude")
             (when value
               (setq exclude-list (cons value exclude-list))))
            (t
             (error "Syntax error line %d: %s" line-number record))))
    (list src-list include-list exclude-list)))

(defun rtags-include-directory-excluded-p (dir excluded-regexs)
  "Return non-nil if the specified directory matches any regex in
the list of excluded regexs"
  (catch 'return
    (dolist (excluded excluded-regexs)
      (when (string-match excluded dir)
        (throw 'return t)))
    (throw 'return nil)))

(defun rtags-scan-include-directories (dir excluded-regexs)
  "Return a list of subdirectories under the specified root dir,
excluding any that match any regex in the specified excluded
regex list."
  (let ((result ()))
    (dolist (subdir (cons dir (pg/directory-tree dir)))
      (unless (rtags-include-directory-excluded-p subdir excluded-regexs)
        (setq result (cons subdir result))))
    result))

(defun rtags-load-compile-include-file (dir)
  "Loads the `compile_includes' file from the specified directory
and sets up the project's source dirs and include dirs. Return
true on success. Normally you should not use this function
directly: use `rtags-create-compilation-database' instead"
  (interactive "DProject root: ")
  (let ((compile-includes-file (concat (file-name-as-directory dir)
                                       "compile_includes")))
    (cond ((file-exists-p compile-includes-file)
           ;; Parse the file and return 3 lists: src, include, exclude
           (let ((directives (rtags-load-compile-include-file-content
                              compile-includes-file)))
             (setq *rtags-project-source-dirs*  ()
                   *rtags-project-include-dirs* ())
             (let ((source-dirs (first directives))
                   (incl-dirs   (second directives))
                   (excl-regexs (third directives)))
               ;; TODO: if no source dirs use dir itself
               ;; TODO: relative paths
               ;; Scan src to get all subdirs that do not match the excludes
               (dolist (path source-dirs)
                 (message "Scanning source dir: %s" path)
                 (setq *rtags-project-source-dirs*
                       (append *rtags-project-source-dirs*
                               (rtags-scan-include-directories path excl-regexs))))
               ;; Same with includes
               (dolist (path incl-dirs)
                 (message "Scanning include dir: %s" path)
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
  (when (rtags-load-compile-include-file dir)
    (let ((dbfilename (concat (file-name-as-directory dir)
                              "compile_commands.json"))
          (compile-command (rtags-create-compilation-command))
          (num-files 0))
      (with-temp-buffer
        (insert "[")
        (newline)
        (dolist (default-directory *rtags-project-source-dirs*)
          (message "Processing directory: %s" default-directory)
          (let ((files (file-expand-wildcards "*.cpp"))
                ;; rdm does not like directories starting with "~/"
                (dirname (if (pg/string-starts-with default-directory "~/")
                             (substitute-in-file-name
                              (concat "$HOME/" (substring default-directory 2)))
                           default-directory)))
            (dolist (file files)
              (incf num-files)
              (insert "  { \"directory\": \"" dirname "\",")
              (newline)
              (insert "    \"command\":   \""
                      compile-command
                      (file-name-sans-extension file) ".o "
                      file "\",")
              (newline)
              (insert "    \"file\":      \"" file "\" },")
              (newline))))
        (insert "];")
        (newline)
        (write-region (buffer-string) nil dbfilename))
      (when (yes-or-no-p
             (format "Wrote compile_commands.json (%d files). Load it?" num-files))
        (rtags-call-rc :path t :output nil "-J" dir)
        ("Done (check rdm's logs).")))))

;; Mode for compile_includes

(defconst rtags-compile-includes-mode-keywords
  ;; Words and associated face.
  `((,(regexp-opt '("src" "include" "exclude") 'words)
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

(provide 'init-rtags)
