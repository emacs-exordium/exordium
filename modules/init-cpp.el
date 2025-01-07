;;; init-cpp.el --- Configuration for C++            -*- lexical-binding: t -*-

;;; Commentary:
;;
;; -------------- -------------------------------------------------------
;; Key            Definition
;; -------------- -------------------------------------------------------
;; C-tab          Switch between .h and .cpp
;; C-c ;          IEdit mode (rename selected variable)
;;
;; Features:
;; - Open .h files in C++ mode by default
;; - Highlight dead code between #if 0 and #endif (after saving)

;;; Code:

(eval-when-compile
  (unless (featurep 'init-require)
    (load (file-name-concat (locate-user-emacs-file "modules") "init-require"))))
(exordium-require 'init-prefs)
(exordium-require 'init-lib)
(when exordium-help-extensions
  (exordium-require 'init-help))

(require 'cl-lib)

(use-package cc-mode
  :ensure nil
  :config
  ;;; Open a header file in C++ mode by default
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode)))


;;; IEdit: rename the symbol under point
(use-package iedit
  :custom
  ;; Fix A bug: normal key is "C-;", but we're going to roll our own bindings
  ;; with bind-key
  (iedit-toggle-key-default nil)
  :init
  (use-package isearch
    :ensure nil
    :defer t
    :bind
    (:map isearch-mode-map
     ("C-c ;" . #'iedit-mode-from-isearch)))
  (use-package help
    :ensure nil
    :defer t
    :bind
    (:map help-map
     ("C-;" . #'iedit-mode-toggle-on-function)))
  (when exordium-help-extensions
    (use-package helpful
      :defer t
      :bind
      (:map helpful-mode-map
       ("C-;" . #'iedit-mode-toggle-on-function))))
  :bind
  (("C-c ;" . #'iedit-mode)
   :map esc-map
   ("C-;" . #'iedit-execute-last-modification)))

;;; Don't show the abbrev minor mode in the mode line
(diminish 'abbrev-mode)


;;; Highlight dead code between #if 0 and #endif

(use-package cpp
  :ensure nil)
(defun cpp-highlight-dead-code ()
  "Highlight c/c++ #if 0 #endif macros."
  (let ((color (face-background 'shadow)))
    (setq cpp-known-face 'default)
    (setq cpp-unknown-face 'default)
    (setq cpp-known-writable 't)
    (setq cpp-unknown-writable 't)
    (setq cpp-edit-list `(("0" (background-color . ,color) default both)
                          ("1" default (background-color . ,color) both)))
    (cpp-highlight-buffer t)))

(defun cpp-highlight-dead-code-hook ()
  "Highlight dead code blocks."
  (cpp-highlight-dead-code)
  (add-hook 'after-save-hook #'cpp-highlight-dead-code 'append 'local))

;;; Highlight dead code between "#if 0" and "#endif"
(add-hook 'c-mode-common-hook #'cpp-highlight-dead-code-hook)
(when (and exordium-treesit-modes-enable
           ;; same conditions as in init-treesit.el
           (version< "29" emacs-version) (treesit-available-p))
  (progn
    (add-hook 'c-ts-mode-hook (lambda () (run-hooks 'c-mode-common-hook)))
    (add-hook 'c++-ts-mode-hook (lambda () (run-hooks 'c-mode-common-hook)))
    (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
    (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
    (add-to-list 'major-mode-remap-alist
                 '(c-or-c++-mode . c-or-c++-ts-mode))))


;;; Switch between .h <--> .cpp <--> t.cpp

(defconst exordium-cpp-header-switches
  '(("t.cpp"   . ("h" "cpp"))
    ("g.cpp"   . ("h" "cpp"))
    ("u.t.cpp" . ("h" "cpp"))
    ("i.t.cpp" . ("h" "cpp"))
    ("h"       . ("cpp" "cc" "t.cpp" "g.cpp" "u.t.cpp" "i.t.cpp" "c"))
    ("cpp"     . ("h" "t.cpp" "g.cpp" "u.t.cpp" "i.t.cpp"))
    ("cc"      . ("h" "t.cc" "u.t.cc" "i.t.cc"))
    ("c"       . ("h")))
  "A-list of extension -> list of matching extensions.")

(defun bde-file-name-extension (file-name)
                                        ; checkdoc-params: (file-name)
  "Like `file-name-extension' respecting a BDE-style test driver infixes."
  (cond ((string-suffix-p ".u.t.cpp" file-name)
         "u.t.cpp")
        ((string-suffix-p ".i.t.cpp" file-name)
         "i.t.cpp")
        ((string-suffix-p ".t.cpp" file-name)
         "t.cpp")
        ((string-suffix-p ".g.cpp" file-name)
         "g.cpp")
        (t
         (file-name-extension file-name))))

(defun cpp-switch-h-cpp (arg)
                                        ; checkdoc-params: (arg)
  "Switch between .h and .cpp buffer or file.
Look first into the open buffers, and look into the current
directory if no matching buffer was found.

With argument, switch to the second choice.  For example, from a
.h or a .cpp open the .t.cpp, or from a .t.cpp open the .cpp."
  (interactive "P")
  (let ((ext (bde-file-name-extension (buffer-file-name))))
    (let ((base-name    (exordium-string-truncate (buffer-name) (length ext)))
          (base-path    (exordium-string-truncate (buffer-file-name) (length ext)))
          (matching-ext (cdr (cl-find-if (lambda (i)
                                           (string= (car i) ext))
                                         exordium-cpp-header-switches))))
      (when (and arg matching-ext)
        (setq matching-ext (cdr matching-ext)))
      (cond (matching-ext
             (unless
                 (catch 'found
                   (cl-flet ((when-exists-find-and-throw
                              (file)
                              (when (file-exists-p file)
                                (find-file file)
                                (throw 'found t))))
                     (dolist (candidate-ext matching-ext)
                       ;; Look for a buffer matching candidate-ext
                       (let ((buff (concat base-name candidate-ext)))
                         (when (bufferp (get-buffer buff))
                           (switch-to-buffer buff)
                           (throw 'found t)))
                       ;; No buffer => look for a file
                       (when-exists-find-and-throw
                        (concat base-path candidate-ext))
                       ;; No file in current dir => look in test subdirectory
                       (cond (arg
                              (let ((base-dir (file-name-directory (buffer-file-name)))
                                    (test-dirs '("test" "tst")))
                                (dolist (test-dir test-dirs)
                                  (let ((test-path
                                         (concat (file-name-as-directory test-dir)
                                                 base-name candidate-ext)))
                                    (when-exists-find-and-throw
                                     (concat base-dir test-path))
                                    (when-exists-find-and-throw
                                     (concat (file-name-directory
                                              (directory-file-name base-dir))
                                             test-path))))))
                             ;; If test file => look in parent and group directories
                             ((string-match ".*/t\\(e\\)?st/.*\\.[gt]\\.cpp\\'"
                                            (buffer-file-name))
                              (let ((base-dir
                                    (file-name-directory
                                     (directory-file-name (file-name-directory
                                                           (buffer-file-name))))))
                                (when-exists-find-and-throw
                                 (concat base-dir base-name candidate-ext))
                                (when (string-match "^\\([a-z]+\\)_" base-name)
                                  (when-exists-find-and-throw
                                   (concat base-dir
                                           (file-name-as-directory
                                            (match-string 1 base-name))
                                           base-name candidate-ext))))))))
                   ;; No buffer or file for any matching-ext
                   nil)
               (message "No matching buffer or file")))
            (t (message "This is not a C/C++ file"))))))

;;; Ctrl-Tab to switch between .h and .cpp
(bind-key "C-TAB" #'cpp-switch-h-cpp c-mode-base-map)


;;; C++11 keywords
(pcase exordium-enable-c++11-keywords
  (:simple
   (let ((keywords (concat "\\<"
                                (regexp-opt
                                 '("alignas"
                                   "alignof"
                                   "char16_t"
                                   "char32_t"
                                   "constexpr"
                                   "decltype"
                                   "final"
                                   "noexcept"
                                   "nullptr"
                                   "override"
                                   "static_assert"
                                   "thread_local")
                                 t)
                                "\\>")))
     (add-hook 'c++-mode-hook
               (lambda()
                 ;; This can be completed with other things later (C++17?)
                 (font-lock-add-keywords
                  nil `((,keywords . font-lock-keyword-face))))
               t)))
  (:modern
   (use-package modern-cpp-font-lock
     :diminish modern-c++-font-lock-mode
     :hook (c++-mode . modern-c++-font-lock-mode))))



(provide 'init-cpp)

;;; init-cpp.el ends here
