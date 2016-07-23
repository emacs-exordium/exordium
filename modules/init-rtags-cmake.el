;;;; RTags-CMake integration
;;;
;;; This module indexes CMake-based projects with zero configuration.
;;; To enable it, set this variable in ~/.emacs.d/prefs.el:
;;;
;;;  (setq exordium-rtags-cmake t)
;;;
;;; If needed you can also set:
;;; - `exordium-rtags-rdm-args': list of strings containing the arguments to
;;;   rdm. Default empty.
;;; - `exordium-rtags-cmake-build-dir': Path to your build dir relative to the
;;;   root of a project. Default "cmake.bld/<arch>" where <arch> is replace by
;;;   the uname of your OS.
;;;
;;; When you open a C++ file, it will detect if your project is CMake-enabled
;;; by looking up for CMakeLists.txt files up to the root of your repo. If it
;;; finds such a file, it starts rdm if it is not running, and indexes your
;;; CMake-generated compilation database (e.g. "rd -J <build-dir>").
;;;
;;; TODO: it should work as well when creating a new file, by running "cmake"
;;; on the project to update the complation database. This can be done using an
;;; after-save-hook I think.
;;;
;;; TODO: add support for a .rtags file in the project root, which would tell
;;; where the build dir is (and perhaps other things later).

(require 'init-prefs)
(require 'rtags)

(defvar exordium-rtags-cmake-mode-known-projects ()
  "Alist of known projects, growing as the user opens files. Each
  pair is (project-dir . compilation-db-path).")

(defvar exordium-rtags-cmake-real-build-dir nil
  "Relative build dir path for the compilation DB. Set to
  `exordium-rtags-cmake-build-dir' with <arch> set to the right
  architecture, if <arch> is used.")

(define-minor-mode rtags-cmake-mode
  "Minor mode for using RTags with zero-configuration, for CMake
enabled projects."
  :lighter "Cm"
  :global nil ; buffer local
  (when rtags-cmake-mode
    ;; This is a workaround for C/C++ mode hooks being called twice:
    ;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=16759
    (unless (local-variable-p 'exordium-rtags-cmake-mode-buffer-enabled)
      (setq-local exordium-rtags-cmake-mode-buffer-enabled t)
      ;; Check if this is a CMake project
      (let ((project-dir (exordium-rtags-cmake-find-buffer-project-root)))
        (when project-dir
          (unless exordium-rtags-cmake-real-build-dir
            (setq exordium-rtags-cmake-real-build-dir
                  (exordium-rtags-cmake-get-build-dir exordium-rtags-cmake-build-dir)))
          ;; Start rdm if needed, and index the project if this is the first
          ;; time we're seeing a file from the project
          (let ((buff (current-buffer)))
            (when (exordium-rtags-start-rdm-maybe)
              ;; For some weird reason the buffer is in rdm log mode!
              (switch-to-buffer buff)
              (with-current-buffer buff
                (c++-mode))))
          (exordium-rtags-cmake-index-project-maybe project-dir))))))

(defun exordium-enable-rtags-cmake-mode ()
  "Enable rtags-cmake-mode for C/C++ buffers."
  (add-hook 'c-mode-hook #'rtags-cmake-mode)
  (add-hook 'c++-mode-hook #'rtags-cmake-mode))

(defun exordium-rtags-cmake-find-buffer-project-root ()
  "Check if the current buffer's project is a CMake project, and
  if so return the root directory of the project. Otherwise
  return nil: this is a new buffer not saved or this is not a
  CMake project. Note that this function stops the search as soon
  as it finds a .git subdirectory."
  (let ((filename (buffer-file-name)))
    (when (and filename (file-exists-p filename))
      (let ((dir (file-name-directory filename)))
        (exordium-rtags-cmake-find-project-root-impl dir nil)))))

(defun exordium-rtags-cmake-find-project-root-impl (dir &optional is-cmake)
  "Return the path of the git repo containing the specified dir,
if this is a CMake repo, otherwise return nil. The extra argument
is-cmake is for recursion."
  (unless is-cmake
    (setq is-cmake (file-exists-p (concat dir "CMakeLists.txt"))))
  (cond ((string= dir "/")
         nil)
        ((file-exists-p (concat dir ".git"))
         (if is-cmake dir nil))
        (t
         (exordium-rtags-cmake-find-project-root-impl (exordium-parent-directory dir)
                                                      is-cmake))))

(defun exordium-rtags-cmake-get-build-dir (dir)
  "Return dir where any occurence of <arch> is replaced by the
uname of the OS"
  (let ((uname (replace-regexp-in-string "\n$" "" (shell-command-to-string "uname"))))
    (replace-regexp-in-string "<arch>" uname dir)))

(defun exordium-rtags-cmake-index-project-maybe (project-dir)
  "Index the project at project-dir unless we have done that before."
  (unless (assoc project-dir exordium-rtags-cmake-mode-known-projects)
    (let* ((comp-db-dir  (concat project-dir exordium-rtags-cmake-build-dir))
           (comp-db-file (concat (file-name-as-directory comp-db-dir)
                                 "compile_commands.json")))
      (if (not (file-exists-p comp-db-file))
          (message "Can't find compilation DB in: %s" comp-db-dir)
        (exordium-rtags-cmake-index-project-impl comp-db-dir)
        (setq exordium-rtags-cmake-mode-known-projects
              (cons (cons project-dir comp-db-dir)
                    exordium-rtags-cmake-mode-known-projects))))))

(defun exordium-rtags-cmake-index-project-impl (comp-db-dir)
  "Invoke rc -J <comp-db-dir>"
  (message "Indexing: %s" comp-db-dir)
  (with-current-buffer (rtags-get-buffer)
    (rtags-call-rc "-J" comp-db-dir)))


(when exordium-rtags-cmake
  (setq rtags-autostart-diagnostics t)
  (exordium-enable-rtags-cmake-mode))
(provide 'init-rtags-cmake)
