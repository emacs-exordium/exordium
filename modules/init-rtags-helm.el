;;;; RTags and Helm integration
;;;
;;; -------------- -------------------------------------------------------
;;; Key            Definition
;;; -------------- -------------------------------------------------------
;;; M-C-g          `rtags-helm-select-taglist' = select a symbol in the
;;;                current file using Helm.
;;; -------------- -------------------------------------------------------

(require 'rtags)
(require 'helm)
(require 'helm-rtags)
(require 'init-prefs)

(defcustom rtags-helm-show-variables nil
  "Whether `rtags-helm-select-taglist' shows variables and parameters"
  :group 'rtags
  :type 'boolean)

(defcustom rtags-helm-show-enums nil
  "Whether `rtags-helm-select-taglist' shows enums"
  :group 'rtags
  :type 'boolean)

(when (or exordium-helm-everywhere exordium-rtags-helm-everywhere)
  (setq rtags-helm-show-variables t)
  (setq rtags-helm-show-enums t)
  (setq rtags-use-helm t))

(defun rtags-helm-sort-list (pairs)
  (sort pairs #'(lambda (p1 p2) (< (cdr p1) (cdr p2)))))

(defun rtags-helm-jump-to-line (line)
  ;;Compiler-happy equivalent of (goto-line line):
  (goto-char (point-min))
  (forward-line (1- line))
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

;;;###autoload
(defun rtags-helm-select-taglist ()
  "Display the list of symbols of the current file in an Helm
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
                        ((string= type "FieldDecl")
                         (add-to-list 'variables
                                      (cons (rtags-helm-propertize-variable text)
                                            (string-to-number linenum))))
                        ((and rtags-helm-show-variables
                              (or (string= type "VarDecl")
                                  (string= type "ParmDecl")))
                         (add-to-list 'variables
                                      (cons (rtags-helm-propertize-variable text)
                                            (string-to-number linenum))))
                        ((and rtags-helm-show-enums
                              (or (string= type "EnumDecl")
                                  (string= type "EnumConstantDecl")))
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
