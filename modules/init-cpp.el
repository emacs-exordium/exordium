;;;; Configuration for C++
;;;
;;; -------------- -------------------------------------------------------
;;; Key            Definition
;;; -------------- -------------------------------------------------------
;;; C-tab          Switch between .h and .cpp
;;; C-c ;          IEdit mode (rename selected variable)
;;;
;;; Features:
;;; - Open .h files in C++ mode by default
;;; - Highlight dead code between #if 0 and #endif (after saving)

(with-no-warnings (require 'cl))
(require 'cc-mode)
(require 'init-lib)

;;; Open a header file in C++ mode by default
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;;; IEdit: rename the symbol under point
;;; Fix A bug (normal key is "C-;")
(define-key global-map (kbd "C-c ;") 'iedit-mode)

;;; Don't show the abbrev minor mode in the mode line
(diminish 'abbrev-mode)


;;; Highlight dead code between #if 0 and #endif

(require 'cpp)
(defun cpp-highlight-dead-code ()
  "highlight c/c++ #if 0 #endif macros"
  (let ((color (face-background 'shadow)))
    (setq cpp-known-face 'default)
    (setq cpp-unknown-face 'default)
    (setq cpp-known-writable 't)
    (setq cpp-unknown-writable 't)
    (setq cpp-edit-list `(("0" (background-color . ,color) default both)
                          ("1" default (background-color . ,color) both)))
    (cpp-highlight-buffer t)))

(defun cpp-highlight-dead-code-hook ()
  (cpp-highlight-dead-code)
  (add-hook 'after-save-hook 'cpp-highlight-dead-code 'append 'local))

;;; Highlight dead code between "#if 0" and "#endif"
(add-hook 'c-mode-common-hook 'cpp-highlight-dead-code-hook)


;;; Switch between .h <--> .cpp

(defconst exordium-cpp-header-switches
  '(("t.cpp" . ("h" "cpp"))
    ("h"     . ("cpp" "cc" "t.cpp" "c"))
    ("cpp"   . ("h" "t.cpp"))
    ("cc"    . ("h" "t.cc"))
    ("c"     . ("h")))
  "A-list of extension -> list of matching extensions")

(defun bde-file-name-extension (file-name)
  "Like `file-name-extension' but returning '.t.cpp' for a
  BDE-style test driver"
  (if (string-suffix-p ".t.cpp" file-name)
      "t.cpp"
    (file-name-extension file-name)))

(defun cpp-switch-h-cpp (arg)
  "Switch between .h and .cpp buffer or file. Look first into the
 open buffers, and look into the current directory if no matching
 buffer was found.
 With argument, switch to the second choice. For example, from a
 .h or a .cpp open the .t.cpp, or from a .t.cpp open the .cpp."
  (interactive "P")
  (let ((ext (bde-file-name-extension (buffer-file-name))))
    (let ((base-name    (string-truncate (buffer-name) (length ext)))
          (base-path    (string-truncate (buffer-file-name) (length ext)))
          (matching-ext (cdr (find-if (lambda (i)
                                        (string= (car i) ext))
                                      exordium-cpp-header-switches))))
      (when (and arg matching-ext)
        (setq matching-ext (cdr matching-ext)))
      (cond (matching-ext
             (unless (catch 'found
                       (dolist (candidate-ext matching-ext)
                         ;; Look for a buffer matching candidate-ext
                         (let ((buff (concat base-name candidate-ext)))
                           (when (bufferp (get-buffer buff))
                             (switch-to-buffer buff)
                             (throw 'found t)))
                         ;; No buffer => look for a file
                         (let ((file (concat base-path candidate-ext)))
                           (when (file-exists-p file)
                             (find-file file)
                             (throw 'found t))))
                       ;; No buffer or file for any matching-ext
                       nil)
               (message "No matching buffer or file")))
            (t (message "This is not a C/C++ file"))))))

;;; Ctrl-Tab to switch between .h and .cpp
(define-key c-mode-base-map [(control tab)] 'cpp-switch-h-cpp)


;;; C++11 keywords

(require 'init-prefs)
(with-no-warnings (require 'cl))

(defconst exordium-extra-c++-keywords
  (remove-if #'null
             (list
              ;; This can be completed with other things later (C++17?)
              (when exordium-enable-c++11-keywords
                '("\\<\\(alignas\\|alignof\\|char16_t\\|char32_t\\|constexpr\\|decltype\\|noexcept\\|nullptr\\|static_assert\\|thread_local\\|override\\|final\\)\\>" . font-lock-keyword-face))))
  "A-list of pairs (regex . face) for highlighting extra keywords in C++ mode")

(when exordium-extra-c++-keywords
  (add-hook 'c++-mode-hook
            #'(lambda()
                (font-lock-add-keywords nil exordium-extra-c++-keywords))
            t))


(provide 'init-cpp)
;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:
