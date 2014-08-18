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
;;; -------------- -------------------------------------------------------
;;;
;;; Whenever rtags jumps somewhere it pushes a location onto its stack. Jump
;;; back and forward in this stack with M-C-left and M-C-right (or default
;;; C-x r [ and R-x r ] )
;;;
;;; Functions:
;;; `rtags-find-file': jump to file by name (full or partial)
;;; `rtags-print-cursorinfo': print debugging info about symbol at point
;;; `rtags-print-dependencies': show all include files (recursively)
;;; `rtags-diagnostics': starts an async process to receive warnings or errors
;;;     from clang; integrates with flymake to put highlighting on code with
;;      warnings and errors.
;;;
;;; Files
;;; =====
;;; Rtags uses the following files:
;;; `~/.rtags' (created automatically)
;;;     Index files which are reloaded when `rdm' restarts.
;;; `~/.rdmrc' (optional)
;;;     Config file for rdm (see rdm.cpp) containing default command line args.
;;; `compile_commands.json' (optional)
;;;     Compilation database for a given project, containing for each file the
;;;     clang command to build it. Must be at the root of the project. Not
;;;     needed if you use the compiler wrapper scripts.
;;; `.rtags-config' (optional)
;;;     Project configuration file. Must be at the root of the project. Not
;;;     needed if there is a .git or .svn at the project root.
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
;;;    Use "rc -J" to reload it.
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
;;; Set the variable `my-clang-command' to contain the exact command needed for
;;; compiling a cpp file, with all necessary includes.
;;;
;;; Then create a compilation DB with M-x `create-compilation-database'. It
;;; will first prompt for the project's root dir, then scan all ".cpp" files
;;; and write a file `compile_commands.json'. If additional directories need to
;;; be scanned, use variable `my-included-projects'.
;;;
;;; Finally reload the compilation DB with "rc -J". Watch for errors in rdm's
;;; logs. It may crash a few times on a file and finally give up with that
;;; file.
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

(require 'rtags)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Key bindings

(rtags-enable-standard-keybindings c-mode-base-map)

(define-key c-mode-base-map (kbd "<f3>") (function rtags-find-symbol-at-point))
(define-key c-mode-base-map (kbd "<f4>") (function rtags-find-references-at-point))
(define-key c-mode-base-map [(meta control g)] (function rtags-imenu))
(define-key c-mode-base-map "\M-[" (function rtags-location-stack-back))
(define-key c-mode-base-map "\M-]" (function rtags-location-stack-forward))

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

(defvar *rtags-clang-include-projects* nil
  "List of default projects to include in all compilation
  databases (a list of strings e.g. paths to project roots). This
  is not needed if you index these projects individually.")

(defvar *rtags-clang-exclude-directories* '("/group" "/doc" "/package" "/test")
  "List of subdirectory names to exclude when computing a compilaton
  command")

(defvar *rtags-clang-include-dir-prefix* ""
  "Prefix to add to any directory listed in `compile_includes'")

(defun* rtags-is-include-directory-excluded (dir)
  "Return non-nil if the specified directory is member of
  *rtags-clang-exclude-directories*"
  (catch 'return
    (dolist (excluded *rtags-clang-exclude-directories*)
      (when (pg/string-ends-with dir excluded)
        (throw 'return t)))
    (throw 'return nil)))

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
                 (included-projects (pg/read-file-lines
                                     compile-includes-file)))
             (dolist (included-project included-projects)
               (dolist (subdir (pg/directory-tree
                                (concat *rtags-clang-include-dir-prefix*
                                        included-project)))
                 (unless (rtags-is-include-directory-excluded subdir)
                   (setq command (concat command " -I" subdir)))))
             (concat command *rtags-clang-command-suffix*)))
          (t
           ;; No "compile_includes" file, use default
           (concat *rtags-clang-command-prefix*
                   *rtags-clang-command-suffix*)))))

(defun rtags-create-compilation-database (dir)
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
