;;; init-rtags-cmake.el --- RTags-CMake integration -*- lexical-binding: t -*-

;;; Commentary:
;;
;;
;; This module indexes CMake-based projects with zero configuration.
;; To enable it, set this variable in ~/.emacs.d/prefs.el:
;;
;;  (setq exordium-rtags-cmake t)
;;
;; You may also set these variables:
;; - `exordium-rtags-rdm-args': list of strings containing the arguments to
;;   rdm, empty by default.  For example:
;;   '("--isystem" "/opt/bb/lib64/clang/3.6.2/include -DBAS_NOBBENV")
;; - `exordium-rtags-cmake-build-dir': Path to your build dir, absolute or
;;   relative to the root of a project.  Default "cmake.bld/<arch>" where
;;   <arch> is replaced by the uname of your OS.
;;
;; When you open a C++ file, it will detect if your project is CMake-enabled
;; by looking for CMakeLists.txt files up to the root of your repo.  If it
;; finds such a file, it starts rdm if it is not running, and indexes your
;; CMake-generated compilation database (e.g. "rd -J <build-dir>").
;;
;; TODO: it should work as well when creating a new file, by running "cmake"
;; on the project to update the complation database.  This can be done using an
;; after-save-hook I think.

(eval-when-compile
  (unless (featurep 'init-require)
    (load (file-name-concat (locate-user-emacs-file "modules") "init-require"))))
(exordium-require 'init-prefs)
(exordium-require 'init-rtags)

(require 'cl-lib)

;;; Code:

;; Note: for now we actually don't need to keep the build-dir of any known
;; project but we should need it later to automatically index new files created
;; from Emacs.
(defvar exordium-rtags-cmake-mode-known-projects ()
  "Alist of known projects, growing as the user opens files.
Each pair is (PROJECT-DIR . BUILD-DIR).  Note that the BUILD-DIR
contains the compilation database.")

(defvar exordium-rtags-cmake-mode-enabled nil
  "Whether cmake mode is enabled.")

(define-minor-mode rtags-cmake-mode
  "Minor mode for using RTags with zero-configuration, for CMake enabled projects."
  :lighter "Cm"
  :global  nil ; buffer local
  (when rtags-cmake-mode
    ;; This is a workaround for C/C++ mode hooks being called twice:
    ;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=16759
    (unless (local-variable-p 'exordium-rtags-cmake-mode-buffer-enabled)
      (setq-local exordium-rtags-cmake-mode-buffer-enabled t)
      ;; Check if this is a CMake project
      (let ((project-dir (exordium-rtags-cmake-find-buffer-project-root)))
        (when project-dir
          ;; Start rdm if needed, and index the project if this is the first
          ;; time we're seeing a file from the project
          (let ((buff (current-buffer)))
            (when (exordium-rtags-start-rdm-maybe)
              ;; For some weird reason the buffer is in rdm log mode!
              (switch-to-buffer buff)
              (with-current-buffer buff
                (c++-mode))))
          (exordium-rtags-cmake-index-project-maybe project-dir))))))

(defun exordium-toggle-rtags-cmake-mode ()
  "Enable or disable `rtags-cmake-mode' for C/C++ buffers."
  (interactive)
  (cond (exordium-rtags-cmake-mode-enabled
         (remove-hook 'c-mode-hook   #'rtags-cmake-mode)
         (remove-hook 'c++-mode-hook #'rtags-cmake-mode)
         (setq exordium-rtags-cmake-mode-enabled nil)
         (message "rtags-cmake-mode is DISABLED"))
        (t
         (add-hook 'c-mode-hook   #'rtags-cmake-mode)
         (add-hook 'c++-mode-hook #'rtags-cmake-mode)
         (setq exordium-rtags-cmake-mode-enabled t)
         (message "rtags-cmake-mode is ENABLED"))))

;;; Find the (CMake) project root directory

(defun exordium-rtags-cmake-find-buffer-project-root ()
  "Find root directory of the project for current buffer.
If the current buffer's project is a CMake project, return the
directory.  Otherwise return nil.  This is a new buffer not saved
or this is not a CMake project.  Note that this function stops
the search as soon as it finds a .git subdirectory."
  (let ((filename (buffer-file-name)))
    (when (and filename (file-exists-p filename))
      (let ((dir (file-name-directory filename)))
        (exordium-rtags-cmake-find-project-root-impl dir nil)))))

(defun exordium-rtags-cmake-find-project-root-impl (dir &optional is-cmake)
  "Find path of the git repo containing the specified DIR.
If the repo is a CMake repo return it.  Otherwise return nil.
The extra argument IS-CMAKE is for recursion."
  (unless is-cmake
    (setq is-cmake (file-exists-p (concat dir "CMakeLists.txt"))))
  (cond ((string= dir "/")
         ;; stop: we've reached the root directory
         nil)
        ((file-exists-p (concat dir ".git"))
         ;; stop: we've reached the root of the repo
         (if is-cmake dir nil))
        (t
         ;; try the parent directory
         (exordium-rtags-cmake-find-project-root-impl (exordium-parent-directory dir)
                                                      is-cmake))))

;;; Index the project

(defun exordium-rtags-cmake-index-project-maybe (project-dir)
  "Index the project at PROJECT-DIR unless we have done that before."
  (unless (assoc project-dir exordium-rtags-cmake-mode-known-projects)
    ;; Find the build dir containing the compilation DB
    (let ((build-dir (exordium-rtags-cmake-get-build-dir project-dir)))
      (if (not (file-exists-p (concat (file-name-as-directory build-dir)
                                      "compile_commands.json")))
          (message "Can't find compilation DB in: %s" build-dir)
        ;; Tell rdm to index the compilation DB and add this project to the
        ;; list of projects that have been indexed already
        (exordium-rtags-cmake-index-project-impl build-dir)
        (setq exordium-rtags-cmake-mode-known-projects
              (cons (cons project-dir build-dir)
                    exordium-rtags-cmake-mode-known-projects))))))

(defun exordium-rtags-cmake-index-project-impl (comp-db-dir)
  "Invoke rc -J <COMP-DB-DIR>."
  (message "Indexing: %s" comp-db-dir)
  (with-current-buffer (rtags-get-buffer)
    (rtags-call-rc "-J" comp-db-dir)))

;;; Find the compilation database (e.g. the build directory)

(defun exordium-rtags-cmake-get-build-dir (project-dir)
  "Return absolute path of the build directory for the PROJECT-DIR.
If a file .rtags exist at project-dir, it is expected to contain
a record \\='build <dir>\\='.  Otherwise the pref
`expordium-rtags-cmake-build-dir' is assumed to contain the
relative path of any project's build directory."
  (let* ((rtags-file (concat project-dir ".rtags"))
         (build-dir
          (if (file-exists-p rtags-file)
              (or (exordium-rtags-cmake-get-key-from-file rtags-file "build") "")
            (exordium-rtags-cmake-get-expanded-build-dir))))
    (if (string-prefix-p "/" build-dir)
        build-dir ; already an absolute path
      (concat project-dir build-dir))))

(defun exordium-rtags-cmake-get-key-from-file (file key)
  "Read FILE and return value associated with KEY, or nil if no such KEY."
  (let* ((records          (exordium-read-file-lines file))
         (matching-records (cl-remove-if-not (lambda (record)
                                               (string-prefix-p key record))
                                          records)))
    (when matching-records
      (cl-second (split-string (car matching-records) " ")))))

(defun exordium-rtags-cmake-get-expanded-build-dir ()
  "Return the value of the pref `exordium-rtags-cmake-build-dir'.
Any occurrence of <arch> replaced by the uname of the local OS."
  (let ((uname (replace-regexp-in-string "\n$" "" (shell-command-to-string "uname"))))
    (replace-regexp-in-string "<arch>" uname exordium-rtags-cmake-build-dir)))


(when exordium-rtags-cmake
  (setq rtags-autostart-diagnostics t)
  (exordium-toggle-rtags-cmake-mode))


(provide 'init-rtags-cmake)

;;; init-rtags-cmake.el ends here
