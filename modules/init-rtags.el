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
;;; back and forward in this stack with M-[ and M-] (or default
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

(rtags-enable-standard-keybindings c-mode-base-map)

(define-key c-mode-base-map (kbd "<f3>") (function rtags-find-symbol-at-point))
(define-key c-mode-base-map (kbd "<f4>") (function rtags-find-references-at-point))
(define-key c-mode-base-map [(meta control g)] (function rtags-imenu))
(define-key c-mode-base-map "\M-[" (function rtags-location-stack-back))
(define-key c-mode-base-map "\M-]" (function rtags-location-stack-forward))

(defun rtags-start-rdm ()
  "Start the rdm deamon in a subprocess and display output in a
buffer"
  (interactive)
  (let ((buffer (get-buffer-create "*Rtags rdm*")))
    (switch-to-buffer buffer)
    (let ((process (start-process "rdm" buffer "rdm")))
      (message "Started rdm - PID %d" (process-id process)))))

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

(defvar *rtags-clang-command-prefix*
  "/usr/bin/clang++ -Irelative "
  "Compilation command prefix to use for creating compilation
  databases. Override this variable for your local environment.")

(defvar *rtags-clang-command-suffix*
  " -c -o "
  "Compilation command suffix to use for creating compilation
  databases. Override this variable for you local environment.")

(defvar *rtags-clang-include-projects*
  ()
  "List of default projects to include in all compilation
  databases (a list of strings e.g. paths to project roots). This
  is not needed if you index these projects individually.")

(defvar *rtags-clang-exclude-directories*
  '("/group$" "/doc$" "/package$" "/test$")
  "List of regex to exclude from the include paths
  when computing a compilation command")

(defvar *rtags-clang-include-dir-prefix*
  ""
  "Prefix to add to any directory listed in `compile_includes',
  if you only want relative paths in that file.")

(defun rtags-is-include-directory-excluded (dir)
  "Return non-nil if the specified directory is member of
  *rtags-clang-exclude-directories*"
  (catch 'return
    (dolist (excluded *rtags-clang-exclude-directories*)
      (when (string-match excluded dir)
        (throw 'return t)))
    (throw 'return nil)))

(defun rtags-add-dir-prefix (dirs)
  "Add the prefix `*rtags-clang-include-dir-prefix*' to any
  directory in the specified list"
  (let ((result ()))
    (dolist (dir dirs)
      (setq result (cons (concat *rtags-clang-include-dir-prefix* dir)
                         result)))
    result))

(defun rtags-create-compilation-command (dir)
  "Return the clang++ command string to use to compile the
  specified directory. Assumes that the directory holds a file
  `compile_includes' containing a list of project root
  directories, and add -I directives for all subdirectories
  within these root directories."
  (let ((compile-includes-file (concat (file-name-as-directory dir)
                                       "compile_includes")))
    (cond ((file-exists-p compile-includes-file)
           (let ((command *rtags-clang-command-prefix*)
                 (included-projects (rtags-add-dir-prefix
                                     (pg/read-file-lines
                                      compile-includes-file))))
             (dolist (project (cons dir included-projects))
               (message "Creating compilation command: scanning %s" project)
               ;; Add the project directory itself
               (unless (rtags-is-include-directory-excluded project)
                 (setq command (concat command " -I" project)))
               ;; Add any subdirectory
               (dolist (subdir (pg/directory-tree project))
                 (unless (rtags-is-include-directory-excluded subdir)
                   (setq command (concat command " -I" subdir)))))
             (concat command *rtags-clang-command-suffix*)))
          (t
           ;; No "compile_includes" file, use default
           (concat *rtags-clang-command-prefix*
                   *rtags-clang-command-suffix*)))))

(defun create-compilation-database (dir)
  "Regenerates `compile_commands.json' in a specified directory"
  (interactive "DProject root: ")
  (let ((dbfilename (concat (file-name-as-directory dir)
                            "compile_commands.json"))
        (compile-command (rtags-create-compilation-command dir))
        (projdirs (cons dir *rtags-clang-include-projects*)))
    (with-temp-buffer
      (insert "[")
      (newline)
      (dolist (projdir projdirs)
        (message "Processing project: %s" projdir)
        (let ((subdirs (cons projdir (pg/directory-tree projdir))))
          (dolist (default-directory subdirs)
            (let ((files (file-expand-wildcards "*.cpp"))
                  ;; rdm does not like directories starting with "~/"
                  (dirname (if (pg/string-starts-with default-directory "~/")
                               (substitute-in-file-name
                                (concat "$HOME/" (substring default-directory 2)))
                             default-directory)))
              (dolist (file files)
                (insert "  { \"directory\": \"" dirname "\",")
                (newline)
                (insert "    \"command\":   \""
                        compile-command
                        (file-name-sans-extension file) ".o "
                        file "\",")
                (newline)
                (insert "    \"file\":      \"" file "\" },")
                (newline))))))
      (insert "];")
      (newline)
      (write-region (buffer-string) nil dbfilename))))

(provide 'init-rtags)
