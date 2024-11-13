;;; init-rtags-helm.el --- RTags and Helm integration -*- lexical-binding: t -*-

;;; Commentary:
;;
;;
;; -------------- -------------------------------------------------------
;; Key            Definition
;; -------------- -------------------------------------------------------
;; M-C-g          `rtags-helm-select-taglist' = select a symbol in the
;;                current file using Helm.
;; -------------- -------------------------------------------------------

;;; Code:
(eval-when-compile
  (unless (featurep 'init-require)
    (load (file-name-concat (locate-user-emacs-file "modules") "init-require"))))
(exordium-require 'init-prefs)
(exordium-require 'init-rtags)
(exordium-require 'init-helm)

(defcustom rtags-helm-show-variables nil
  "Whether `rtags-helm-select-taglist' shows variables and parameters."
  :group 'rtags
  :type 'boolean)

(defcustom rtags-helm-show-enums nil
  "Whether `rtags-helm-select-taglist' shows enums."
  :group 'rtags
  :type 'boolean)

(when (or exordium-helm-everywhere exordium-rtags-helm-everywhere)
  (setq rtags-helm-show-variables t)
  (setq rtags-helm-show-enums t))

(defun rtags-helm-sort-list (pairs)
  "Sort list of PAIRS using their cdrs."
  (sort pairs (lambda (p1 p2) (< (cdr p1) (cdr p2)))))

(defun rtags-helm-jump-to-line (line)
  "Compiler-happy equivalent of (goto-line LINE)."
  (goto-char (point-min))
  (forward-line (1- line))
  (recenter))

(defun rtags-helm-propertize-function (text)
  "Return propertized TEXT for a method, constructor or function declaration."
  (cond ((string-match "^\\(.*\\) \\(.*\\)::\\(.*\\)$" text)
         (let ((return-type (match-string-no-properties 1 text))
               (class (match-string-no-properties 2 text))
               (method (match-string-no-properties 3 text)))
           (format "%s %s::%s"
                   (propertize return-type 'face 'font-lock-type-face)
                   (propertize class 'face 'font-lock-constant-face)
                   (propertize method 'face 'font-lock-function-name-face))))
        ((string-match "^\\(.*\\)::\\(.*\\)$" text)
         (let ((class (match-string-no-properties 1 text))
               (ctor (match-string-no-properties 2 text)))
           (format "%s::%s"
                   (propertize class 'face 'font-lock-constant-face)
                   (propertize ctor 'face 'font-lock-function-name-face))))
        ((string-match "^\\(.*\\) \\(.*\\)$" text)
         (let ((return-type (match-string-no-properties 1 text))
               (function (match-string-no-properties 2 text)))
           (format "%s %s"
                   (propertize return-type 'face 'font-lock-type-face)
                   (propertize function 'face 'font-lock-function-name-face))))
        (t text)))

(defun rtags-helm-propertize-variable (text)
  "Return propertized TEXT for a variable declaration."
  (cond ((string-match "^const \\(.*\\) \\(.*\\)$" text)
         (let ((type (match-string-no-properties 1 text))
               (var (match-string-no-properties 2 text)))
           (format "%s %s %s"
                   (propertize "const" 'face 'font-lock-keyword-face)
                   (propertize type 'face 'font-lock-type-face)
                   var)))
        ((string-match "^\\(.*\\) \\(.*\\)$" text)
         (let ((type (match-string-no-properties 1 text))
               (var (match-string-no-properties 2 text)))
           (format "%s %s"
                   (propertize type 'face 'font-lock-type-face)
                   var)))
        (t text)))

(defun rtags-helm-propertize-macro (text)
  "Return propertized TEXT for a #include or a #define."
  (cond ((string-match "^#include \\(.*\\)$" text)
         (let ((file (match-string-no-properties 1 text)))
           (format "%s %s"
                   (propertize "#include" 'face 'font-lock-preprocessor-face)
                   (propertize (concat "<" file ">") 'face 'font-lock-string-face))))
        (t text)))

;;;###autoload
(defun rtags-helm-select-taglist ()
  "Display the list of symbols of the current file in an Helm buffer.
For example: classes, functions, variables, enums and other."
  (interactive)
  (let* ((fn (buffer-file-name))
         functions classes variables enums macros other)
    ;; Fetch taglists. Each list is a list of pairs (text . line-number)
    (with-temp-buffer
      (rtags-call-rc :path fn :path-filter fn "-F" "--cursor-kind" "--display-name" "--no-context")
      ;;(message "%s" (buffer-string))
      (unless (= (point-min) (point-max))
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties (line-beginning-position)
                                                      (line-end-position))))
            (when (string-match "^\\(.*:\\)\\([0-9]+\\)\\(:[0-9]+:\\)\t\\(.*\\)\t\\(.*\\)$" line)
                (let ((linenum (match-string-no-properties 2 line))
                      (text (match-string-no-properties 4 line))
                      (type (match-string-no-properties 5 line)))
                  (cond ((or (string= type "FunctionDecl")
                             (string= type "CXXMethod")
                             (string= type "CXXConstructor")
                             (string= type "CXXDestructor"))

                         (cl-pushnew (cons (rtags-helm-propertize-function text)
                                           (string-to-number linenum))
                                     functions
                                     :test (lambda (e1 e2)
                                             (and (equal (car e1) (car e2))
                                                  (eql (cdr e1) (cdr e2))))))
                        ((or (string= type "ClassDecl")
                             (string= type "StructDecl"))
                         (cl-pushnew (cons (propertize text 'face 'font-lock-type-face)
                                           (string-to-number linenum))
                                     classes
                                     :test (lambda (e1 e2)
                                             (and (equal (car e1) (car e2))
                                                  (eql (cdr e1) (cdr e2))))))
                        ((string= type "FieldDecl")
                         (cl-pushnew (cons (rtags-helm-propertize-variable text)
                                           (string-to-number linenum))
                                     variables
                                     :test (lambda (e1 e2)
                                             (and (equal (car e1) (car e2))
                                                  (eql (cdr e1) (cdr e2))))))
                        ((and rtags-helm-show-variables
                              (or (string= type "VarDecl")
                                  (string= type "ParmDecl")))
                         (cl-pushnew (cons (rtags-helm-propertize-variable text)
                                           (string-to-number linenum))
                                     variables
                                     :test (lambda (e1 e2)
                                             (and (equal (car e1) (car e2))
                                                  (eql (cdr e1) (cdr e2))))))
                        ((and rtags-helm-show-enums
                              (or (string= type "EnumDecl")
                                  (string= type "EnumConstantDecl")))
                         (cl-pushnew (cons text (string-to-number linenum))
                                     enums
                                     :test (lambda (e1 e2)
                                             (and (equal (car e1) (car e2))
                                                  (eql (cdr e1) (cdr e2))))))
                        ((or (string= type "macro definition")
                             (string= type "include directive")
                             (string= type "inclusion directive"))
                         (cl-pushnew (cons (rtags-helm-propertize-macro text)
                                           (string-to-number linenum))
                                     macros
                                     :test (lambda (e1 e2)
                                             (and (equal (car e1) (car e2))
                                                  (eql (cdr e1) (cdr e2))))))
                        (t
                         (cl-pushnew (cons text (string-to-number linenum))
                                     other
                                     :test (lambda (e1 e2)
                                             (and (equal (car e1) (car e2))
                                                  (eql (cdr e1) (cdr e2))))))))))
          (forward-line))))
    ;; Display them in Helm
    (helm :sources
          `(((name . "Classes")
             (candidates . ,(rtags-helm-sort-list classes))
             (action . rtags-helm-jump-to-line))
            ((name . "Functions")
             (candidates . ,(rtags-helm-sort-list functions))
             (action . rtags-helm-jump-to-line))
            ((name . ,(if rtags-helm-show-variables "Fields and Variables" "Fields"))
             (candidates . ,(rtags-helm-sort-list variables))
             (action . rtags-helm-jump-to-line))
            ((name . "Enums")
             (candidates . ,(rtags-helm-sort-list enums))
             (action . rtags-helm-jump-to-line))
            ((name . "Macros and Includes")
             (candidates . ,(rtags-helm-sort-list macros))
             (action . rtags-helm-jump-to-line))))))

(define-key c-mode-base-map [(meta control g)] 'rtags-helm-select-taglist)


(provide 'init-rtags-helm)

;;; init-rtags-helm.el ends here
