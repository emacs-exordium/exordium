;;; bde-util.el --- Provide C++ editing functions

;;; Authors: Philippe Grenet (pgrenet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Usage
;;; Installation:
;;;
;;; First create a symbolic link from ~/.emacs.d/lisp/bde-util.el to this file.
;;; Alternatively you can add the location of this file in your .emacs with:
;;;     (add-to-list 'load-path "/path/to/this/file/")
;;;
;;; Then add this in your .emacs (replace keys as you see fit):
;;;     (require 'bde-util)
;;;
;;;     ;; Ctrl-Tab to switch between .h and .cpp
;;;     (global-set-key [(control tab)] `bde-switch-h-cpp)
;;;
;;;     ;; Highlight dead code like this one:
;;;     ;; #if 0
;;;     ;; // dead code here
;;;     ;; #endif
;;;     (add-hook 'c-mode-common-hook 'bde-highlight-dead-code-hook)
;;;
;;; Utilities:
;;;  * `bde-switch-h-cpp' - switch between the header and implementation of the
;;;     component you are working on.
;;;  * `bde-highlight-dead-code' - highlight region between "#if 0" and
;;;     "#endif".  Set the color in `*bde-highlight-dead-code-color*'.
;;;
;;; TODO
;;; - bde-switch-h-cpp should create a new buffer if no buffer or file exists.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Switch .h .cpp

(defun bde-string-without-last (string n)
  "Return string minus the last n characters."
  (substring string 0 (max 0(- (length string) n))))

(defun bde-string-ends-with (string tail)
  "Predicate checking whether string ends with the given tail."
  (string= tail (substring string (- (length tail)))))

(defun bde-find-if (predicate list)
  (catch 'found
    (dolist (elt list nil)
      (when (funcall predicate elt)
        (throw 'found elt)))))

(defvar bde-header-switches '(("h" .   ("cpp" "c"))
                              ("cpp" . ("h"))
                              ("c" .   ("h"))))

(defun bde-switch-h-cpp ()
  "Switch .h and .cpp file."
  (interactive)
  (let* ((ext (file-name-extension (buffer-file-name)))
         (base-name (bde-string-without-last (buffer-name) (length ext)))
         (base-path (bde-string-without-last (buffer-file-name) (length ext)))
         (count-ext (cdr (bde-find-if (lambda (i)
                                        (string= (car i) ext))
                                      bde-header-switches))))
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BDE verify

(defun bde-verify-buffer ()
  "Runs bdeverify on the current file (asks to save if needed)"
  (interactive)
  (compile
   (format "/bbshr/bde/bde-tools/bin/bdeverify %s"
           (shell-quote-argument (buffer-file-name)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Highlight dead code

;;(defvar *bde-highlight-dead-code-color* "Gray50")

(defun bde-highlight-dead-code ()
  "highlight c/c++ #if 0 #endif macros"
  (let ((color (face-background 'region)))
    (setq cpp-known-face 'default)
    (setq cpp-unknown-face 'default)
    (setq cpp-known-writable 't)
    (setq cpp-unknown-writable 't)
    (setq cpp-edit-list `(("0" (background-color . ,color) default both)
                          ("1" default (background-color . ,color) both)))
    (cpp-highlight-buffer t)))

(defun bde-highlight-dead-code-hook ()
  (bde-highlight-dead-code)
  (add-hook 'after-save-hook 'bde-highlight-dead-code 'append 'local))

;;; End of file
(provide 'bde-util)
