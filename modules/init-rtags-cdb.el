;;;; Command to create a compilation database.
;;;
;;; ---------- ----------------------------------------------------------------
;;; Key        Command
;;; ---------- ----------------------------------------------------------------
;;;            `rtags-create-compilation-database': see doc below.
;;; ---------- ----------------------------------------------------------------
;;;
;;; This module provides a single command, `rtags-create-compilation-database',
;;; which is an easy way to generate a CLang compilation database
;;; (`compile_commands.json') for non-CMake projects.
;;;
;;; The first step is to create a file `compile_includes' in the project root
;;; dir, which specifies how to compile your project and in particular where
;;; are all the source files and all the include files. For example:
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
;;;   # Optional: patterns to exclude in -I directives and for looking for
;;;   # sources:
;;;   exclude /test$
;;;   exclude /doc$
;;;   exclude /group$
;;;   exclude /package$
;;;
;;;   # Optional: if any file name pattern must be excluded from the "src" files,
;;;   # use the "excludesrc" directive. For example this will exclude all test
;;;   # drivers:
;;;   excludesrc \.t\.cpp$
;;;
;;; In addition, the creation of a compilation database uses these variables:
;;;
;;; - `rtags-compile-includes-base-dir': set this to your workspace path
;;;   if you want to use relative paths in `compile_includes' (by default any
;;;   relative path in this file is relative to the project root dir).
;;; - `rtags-clang-command-prefix': default is "/usr/bin/clang++ -Irelative"
;;;   (Note that rtags ignores the clang++ command because it uses libclang).
;;; - `rtags-clang-command-suffix': default is "-c -o".
;;;
;;; Once you have created the `compile_includes' file, run the command
;;; M-x `rtags-create-compilation-database'. It will:
;;;
;;; - Prompt for the project root dir
;;; - Scan all source dirs and include dirs
;;; - Create `compilation_database.json' (it overwrites without asking)
;;; - Ask if you want to reload it (if rdm is running).

(with-no-warnings (require 'cl))
(require 'init-lib)

;; Override these variables in your .emacs as needed:

(defvar rtags-clang-command-prefix
  "/usr/bin/clang++ "
  "Compilation command prefix to use for creating compilation
  databases. Override this variable for your local environment.")

(defvar rtags-clang-command-suffix
  " -c -o "
  "Compilation command suffix to use for creating compilation
  databases. Override this variable for you local environment.")

(defvar rtags-compile-includes-base-dir
  nil
  "If non-nil, base directory to use for all relative paths in
  `compile_include'. Use nil for absolute paths.")


;;; Creating a compilation DB

(defun rtags-load-compile-includes-file-content (compile-includes-file)
  "Read and parse the specified compile-includes file, and return
a list of five sublists:
- The list of `src' directives,
- The list of `include' directives,
- The list of `exclude' directives,
- The list of `excludesrc' directives,
- The list of `macro' directives."
  (let ((line-number      1)
        (value            nil)
        (src-list         ())
        (include-list     ())
        (exclude-list     ())
        (exclude-src-list ())
        (macro-list       ()))
    (dolist (record (exordium-read-file-lines compile-includes-file))
      (incf line-number)
      (setq value (second (split-string record " ")))
      (cond ((or (eq "" record)
                 (string-prefix-p "#" record))
             ;; Comment or empty string; skip it
             nil)
            ((string-prefix-p "src" record)
             (when value
               (setq src-list (cons value src-list))))
            ((string-prefix-p "include" record)
             (when value
               (setq include-list (cons value include-list))))
            ((string-prefix-p "excludesrc" record)
             (when value
               (setq exclude-src-list (cons value exclude-src-list))))
            ((string-prefix-p "exclude" record)
             (when value
               (setq exclude-list (cons value exclude-list))))
            ((string-prefix-p "macro" record)
             (when value
               (setq macro-list (cons value macro-list))))
            (t
             (error "Syntax error line %d: %s" line-number record))))
    (list src-list include-list exclude-list exclude-src-list macro-list)))

(defun rtags-is-excluded-p (path excluded-regexs)
  "Return non-nil if the specified path matches any regex in
the list of excluded regexs"
  (catch 'return
    (dolist (excluded excluded-regexs)
      (when (string-match excluded path)
        (throw 'return t)))
    (throw 'return nil)))

(defun rtags-directory-contains-sources-p (path)
  "Return non-nil if the specified path contains any C/C++ source
  or header file"
  (directory-files path nil ".*\\.\\(c\\|cpp\\|h\\|hpp\\)$" nil))

(defun rtags-scan-subdirectories (dir excluded-regexs)
  "Return a list of subdirectories under the specified root dir,
excluding any that match any regex in the specified excluded
regex list."
  (let ((result ()))
    (dolist (subdir (cons dir (exordium-directory-tree dir)))
      (when (and (rtags-directory-contains-sources-p subdir)
                 (not (rtags-is-excluded-p subdir excluded-regexs)))
        (setq result (cons subdir result))))
    result))

(defun rtags-load-compile-includes-file (dir)
  "Loads the `compile_includes' file from the specified directory
and returns its content as a property list, or nil if the file
could not be loaded. The property list looks like this:
'(:src-dirs (...)
  :include-dirs (...)
  :exclude-src (...)
  :macros (...))"
  (let ((compile-includes-file (concat (file-name-as-directory dir)
                                       "compile_includes")))
    (cond ((file-exists-p compile-includes-file)
           ;; Parse the file and return 3 lists: src, include, exclude
           (let ((directives (rtags-load-compile-includes-file-content
                              compile-includes-file)))
             (let ((src-dirs    (first directives))
                   (incl-dirs   (second directives))
                   (excl-regexs (third directives))
                   (excl-src    (fourth directives))
                   (macros      (fifth directives))
                   (result      ()))
               ;; Scan src to get all subdirs that do not match the excludes
               (let (dirs)
                 (dolist (path src-dirs)
                   (unless (file-name-absolute-p path)
                     (setq path (expand-file-name path
                                                  (or rtags-compile-includes-base-dir
                                                      dir))))
                   (message "Scanning source dir: %s ..." path)
                   (setq dirs (nconc dirs (rtags-scan-subdirectories path excl-regexs))))
                 (setq result (list :src-dirs dirs)))
               ;; Same with includes
               (let (dirs)
                 (dolist (path incl-dirs)
                   (setq path (expand-file-name path rtags-compile-includes-base-dir))
                   (message "Scanning include dir: %s ..." path)
                   (setq dirs (nconc dirs (rtags-scan-subdirectories path excl-regexs))))
                 (setq result (nconc result (list :include-dirs dirs))))
               ;; Add exclude-src and macros into the result
               (setq result (nconc result (list :exclude-src excl-src
                                                :macros macros)))
               ;; Done
               (message "Project has %d source dirs and %d include dirs"
                        (length (plist-get result :src-dirs))
                        (length (plist-get result :include-dirs)))
               result)))
          (t
           (message "No compilation_includes file")
           nil))))

(defun rtags-create-compilation-command (plist)
  "Returns a string containing the clang compilation command to
use for the compilation database, using the content of PLIST."
  (let ((command rtags-clang-command-prefix))
    ;; -D options:
    (dolist (m (plist-get plist :macros))
      (setq command (concat command " -D" m)))
    ;; -I options
    (dolist (path (plist-get plist :src-dirs))
      (setq command (concat command " -I" path)))
    (dolist (path (plist-get plist :include-dirs))
      (setq command (concat command " -I" path)))
    (concat command rtags-clang-command-suffix)))

(defun rtags-prompt-compilation-database-dir ()
  "Prompts the user for the directory where to generate the
compilation database. If we're in a projectile project, propose
the project root first, and prompt for a dir if the user
declines. Returns the directory string."
  (let ((project-root (and (featurep 'projectile)
                           (projectile-project-root))))
    (if (and project-root
             (y-or-n-p (format "Create at project root (%s)?" project-root)))
        project-root
      (read-directory-name "Project root: "))))

(defun rtags-create-compilation-database (dir)
  "Regenerates `compile_commands.json' from `compile_includes' in
the specified directory."
  (interactive (list (rtags-prompt-compilation-database-dir)))
  (let ((plist (rtags-load-compile-includes-file dir)))
    (when plist
      (let ((dbfilename (concat (file-name-as-directory dir)
                                "compile_commands.json"))
            (compile-command (rtags-create-compilation-command plist))
            (exclude-files (plist-get plist :exclude-src))
            (num-files 0))
        (with-temp-buffer
          (insert "[")
          (newline)
          ;; Note: dynamic binding of variable default-directory
          (dolist (default-directory (plist-get plist :src-dirs))
            (message "Processing directory: %s ..." default-directory)
            (let ((files (mapcan #'file-expand-wildcards
                                 exordium-rtags-source-file-extensions))
                  ;; rdm does not like directories starting with "~/"
                  (dirname (if (string-prefix-p "~/" default-directory)
                               (substitute-in-file-name
                                (concat "$HOME/" (substring default-directory 2)))
                             default-directory)))
              (dolist (file files)
                (unless (rtags-is-excluded-p file exclude-files)
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
          ;; FIXME: rtags-call-rc does not work if you don't specify a current buffer?
          ;; That seems broken.
          (rtags-call-rc :path t :output nil :unsaved (current-buffer) "-J" dir)
          (message "Reloaded (check rdm's logs)"))))))


;;; Mode for compile_includes files

(defconst rtags-compile-includes-mode-keywords
  ;; Words and associated face.
  `(( "\\(^src\\|^include\\|^excludesrc\\|^exclude\\|^macro\\)"
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


(provide 'init-rtags-cdb)
