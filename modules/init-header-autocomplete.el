;;;; Init C++ headers Autocomplete
;;;
;;; This module sets up autocomplete of #include header files in C++ mode. It
;;; reuses the file `compile_include' to find what include directories to use
;;; (see `init_rtags.el' for details about that file).
;;;
;;; Usage: M-x `set-header-autocomplete'. It will prompt for the project root
;;; directory, then it will recursively scan the project directory and any
;;; directory listed in the `compile_include' file. Autocomplete should work
;;; after this operation (you may need to reopen the C++ files that are already
;;; open).
;;;
;;; Note: this is a work in progress. First, the include paths are not
;;; currently saved, so one has to re-run `set-header-autocomplete' each time
;;; she opens Emacs or switches to a different project. Second, this module is
;;; a bit redundant with RTags: both Rtags and this module use a project file
;;; `compile_includes', but without using the same code. I'm just hoping Rtags
;;; will eventually provide autocomplete for header files and therefore make
;;; this module unnecessary.

(require 'init-prolog)

(defvar *header-ac-directories*
  ()
  "List of directories to use for C++ header file
  autocomplete. This should be set to a list of path with all
  include directories. This variable is set with function
  `set-header-autococomplete'.")

(defvar *header-ac-exclude-directories*
  '("/group$" "/doc$" "/package$" "/test$")
  "List of regex to exclude when computing the list
  of header directories in `set-header-autocomplete'.")

(defvar *header-ac-include-dir-prefix*
  ""
  "Prefix to add to any directory listed in `compile_includes',
  if you only want relative paths in that file.")

(defun header-ac-is-include-directory-excluded (dir)
  "Return non-nil if the specified directory is member of
  *header-ac-exclude-directories*"
  (catch 'return
    (dolist (excluded *header-ac-exclude-directories*)
      (when (string-match dir excluded)
        (throw 'return t)))
    (throw 'return nil)))

(defun header-ac-add-dir-prefix (dirs)
  "Add the prefix `*header-ac-include-dir-prefix*' to any
  directory in the specified list"
  (let ((result ()))
    (dolist (dir dirs)
      (setq result (cons (concat *header-ac-include-dir-prefix* dir)
                         result)))
    result))

(defun set-header-autocomplete (dir)
  "Set the C++ header autocomplete for the specified
  directory. Assumes that the directory holds a file
  `compile_includes' containing a list of project root
  directories to include."
  (interactive "DProject root: ")
  (let ((compile-includes-file (concat (file-name-as-directory dir)
                                       "compile_includes")))
    (cond ((file-exists-p compile-includes-file)
           (setq *header-ac-directories* nil)
           (let ((included-projects (header-ac-add-dir-prefix
                                     (pg/read-file-lines
                                      compile-includes-file))))
             (dolist (project (cons dir included-projects))
               (dolist (subdir (pg/directory-tree project))
                 (unless (header-ac-is-include-directory-excluded subdir)
                   (setq *header-ac-directories*
                         (cons subdir *header-ac-directories*))))))
           (message "Header autocomplete list set with %d directories"
                    (length *header-ac-directories*)))
          (t
           (message "No file 'compile_includes' there, sorry!")
           (setq *header-ac-directories* nil)))))

(defun show-header-autocomplete ()
  (interactive)
  (message "%s" *header-ac-directories*))

(defun header-ac-c-hook ()
  "C++ mode hook for autocompleting system headers"
  (require 'auto-complete-c-headers)
  ;; Standard OS headers
  (add-to-list 'ac-sources 'ac-source-c-headers)
  ;; Project specific headers
  (dolist (dir *header-ac-directories*)
  (add-to-list 'achead:include-directories dir)))

(add-hook 'c++-mode-hook 'header-ac-c-hook)
(add-hook 'c-mode-hook 'header-ac-c-hook)

(provide 'init-header-autocomplete)
