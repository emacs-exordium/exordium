;;;; RTags and Helm integration
;;;
;;; -------------- -------------------------------------------------------
;;; Key            Definition
;;; -------------- -------------------------------------------------------
;;; M-C-g          `rtags-helm-select-taglist' = select a symbol in the
;;;                current file using Helm.
;;; -------------- -------------------------------------------------------

(require 'helm)

(defun rtags-helm-jump-to-line (line)
  (goto-line line)
  (recenter))

(defun rtags-helm-propertize-function (text)
  "Return a colored string for a method, constructor or function declaration"
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
  "Return a colored string for a variable declaration"
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
  "Return a colored string for a #include or a #define"
  (cond ((string-match "^#include \\(.*\\)$" text)
         (let ((file (match-string-no-properties 1 text)))
           (format "%s %s"
                   (propertize "#include" 'face 'font-lock-preprocessor-face)
                   (propertize (concat "<" file ">") 'face 'font-lock-string-face))))
        (t text)))

(defun rtags-helm-select-taglist ()
  "Display the symbols of the current file in an Helm
buffer (classes, functions, variables, enums and other)"
  (interactive)
  (let* ((fn (buffer-file-name))
         functions classes variables enums macros other)
    ;; Fetch taglists. Each list is a list of pairs (text . line-number)
    (with-temp-buffer
      (rtags-call-rc :path fn :path-filter fn "-F" "--cursor-kind" "--display-name" "--no-context")
      ;;(message "%s" (buffer-string))
      (unless (= (point-min) (point-max))
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
            (when (string-match "^\\(.*:\\)\\([0-9]+\\)\\(:[0-9]+:\\)\t\\(.*\\)\t\\(.*\\)$" line)
                (let ((loc-start (match-string-no-properties 1 line))
                      (linenum (match-string-no-properties 2 line))
                      (loc-end (match-string-no-properties 3 line))
                      (text (match-string-no-properties 4 line))
                      (type (match-string-no-properties 5 line)))
                  (cond ((or (string= type "FunctionDecl")
                             (string= type "CXXMethod")
                             (string= type "CXXConstructor")
                             (string= type "CXXDestructor"))
                         (add-to-list 'functions
                                      (cons (rtags-helm-propertize-function text)
                                            (string-to-number linenum))))
                        ((or (string= type "ClassDecl")
                             (string= type "StructDecl"))
                         (add-to-list 'classes
                                      (cons (propertize text 'face 'font-lock-type-face)
                                            (string-to-number linenum))))
                        ((or (string= type "VarDecl")
                             (string= type "FieldDecl")
                             (string= type "ParmDecl"))
                         (add-to-list 'variables
                                      (cons (rtags-helm-propertize-variable text)
                                            (string-to-number linenum))))
                        ((or (string= type "EnumDecl")
                             (string= type "EnumConstantDecl"))
                         (add-to-list 'enums
                                      (cons text (string-to-number linenum))))
                        ((or (string= type "macro definition")
                             (string= type "include directive")
                             (string= type "inclusion directive"))
                         (add-to-list 'macros
                                      (cons (rtags-helm-propertize-macro text)
                                            (string-to-number linenum))))
                        (t
                         (add-to-list 'other
                                      (cons text (string-to-number linenum))))))))
          (forward-line))))
    ;; Display them in Helm
    (helm :sources
          `(((name . "Classes")
             (candidates . classes)
             (action . rtags-helm-jump-to-line))
            ((name . "Functions")
             (candidates . functions)
             (action . rtags-helm-jump-to-line))
            ((name . "Variables")
             (candidates . variables)
             (action . rtags-helm-jump-to-line))
            ((name . "Enums")
             (candidates . enums)
             (action . rtags-helm-jump-to-line))
            ((name . "Macros and Includes")
             (candidates . macros)
             (action . rtags-helm-jump-to-line))))))

(define-key c-mode-base-map [(meta control g)] 'rtags-helm-select-taglist)

(provide 'init-rtags-helm)
