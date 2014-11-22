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

(require 'cc-mode)

;;; Open a header file in C++ mode by default
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;;; IEdit: rename the symbol under point
;;; Fix A bug (normal key is "C-;")
(define-key global-map (kbd "C-c ;") 'iedit-mode)

;;; Don't show the abbrev minor mode in the mode line
(diminish 'abbrev-mode)

;;; Turn on FCI automatically
(add-hook 'python-mode-hook 'fci-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Highlight dead code between #if 0 and #endif

(defun cpp-highlight-dead-code ()
  "highlight c/c++ #if 0 #endif macros"
  (let ((color (face-background 'region)))
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Switch between .h <--> .cpp

(defconst *cpp-header-switches* '(("h" .   ("cpp" "c"))
                                  ("cpp" . ("h"))
                                  ("c" .   ("h"))))

(defun cpp-switch-h-cpp ()
  "Switch .h and .cpp file."
  (interactive)
  (let* ((ext (file-name-extension (buffer-file-name)))
         (base-name (pg/string-without-last (buffer-name) (length ext)))
         (base-path (pg/string-without-last (buffer-file-name) (length ext)))
         (count-ext (cdr (pg/find-if (lambda (i)
                                       (string= (car i) ext))
                                     *cpp-header-switches*))))
    (cond (count-ext
           (catch 'found
             ;; first look into the existing buffers
             (let ((buffers (mapcar (lambda (i)
                                      (concat base-name i))
                                    count-ext)))
               (dolist (buff buffers)
                 (when (bufferp (get-buffer buff))
                   (switch-to-buffer buff)
                   (throw 'found nil))))
             ;; if not such buffer, look into the files in the same dir
             (let ((files (mapcar (lambda (count-ext)
                                    (concat base-path count-ext))
                                  count-ext)))
               (dolist (file files)
                 (when (file-exists-p file)
                   (find-file file)
                   (throw 'found nil))))))
          (t (message "This is not a C/C++ file")))))

;;; Ctrl-Tab to switch between .h and .cpp
(define-key c-mode-base-map [(control tab)] 'cpp-switch-h-cpp)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Font lock changes

;;; Display TODO: and FIXME: and TBD: in red
(font-lock-add-keywords
 'c++-mode
 '(("\\<\\(FIXME\\):" 1 font-lock-warning-face prepend)
   ("\\<\\(TBD\\):" 1 font-lock-warning-face prepend)
   ("\\<\\(TODO\\):" 1 font-lock-warning-face prepend)))


(provide 'init-cpp)
