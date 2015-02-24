;;;; RTags and Helm integration
;;;
;;; -------------- -------------------------------------------------------
;;; Key            Definition
;;; -------------- -------------------------------------------------------
;;; M-C-g          `rtags-helm-select-taglist' = select a symbol in the
;;;                current file using Helm.
;;; -------------- -------------------------------------------------------

;;; This is an attempt to make this module display the function/methods
;;; arguments.
;;;
;;; I'm not even sure it is a good idea: the list is busy with wrapping lines
;;; for functions having many parameters, and that defeats the purpose which is
;;; to show a clean list of functions in order to find one quickly. But if we
;;; want it, there seem to be 2 problems:
;;;
;;; 1. For a function like:
;;;
;;; void DmpPublisher::onSubscriptionData(
;;;                                   const simm::SubscriptionData& data,
;;;                                   int                           clientId,
;;;                                   int                           subscriptionId)
;;;
;;; ... for the first parameter, rdm returns:
;;;
;;;   Users/pgrenet/AeroFS/sre/src/groups/sim/simb/simb_dmppublisher.cpp:477:65:
;;;   const simm::SubscriptionData& data,	const SubscriptionData &data	ParmDecl
;;;
;;; So the only parameter is the line number which is +1 for the function.
;;;
;;; 2. There is a bug in my build of rdm (old build, maybe fixed since):
;;;
;;;   /Users/pgrenet/AeroFS/sre/src/groups/sim/simb/simb_dmppublisher.cpp:35:34:
;;;  dmpat::RecordId makeRecordId(int recordId, int clientId)	int recordIParmDecl
;;;                                                                        ^ BUG!

(require 'rtags)
(require 'helm)

(defcustom rtags-helm-show-variables nil
  "Whether `rtags-helm-select-taglist' shows variables and parameters"
  :group 'rtags
  :type 'boolean)

(defcustom rtags-helm-show-enums nil
  "Whether `rtags-helm-select-taglist' shows enums"
  :group 'rtags
  :type 'boolean)

(defun rtags-helm-sort-list (pairs)
  (sort pairs #'(lambda (p1 p2) (< (cdr p1) (cdr p2)))))

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

;;;###autoload
(defun rtags-helm-select-taglist ()
  "Display the list of symbols of the current file in an Helm
buffer (classes, functions, variables, enums and other)"
  (interactive)
  (let ((fn (buffer-file-name))
        (incomplete-functions (make-hash-table :test 'equal))
        current-incomplete
        functions classes variables enums macros other)
    ;; Fetch taglists. Each list is a list of pairs (text . line-number)
    (with-temp-buffer
      (rtags-call-rc :path fn :path-filter fn "-F" "--cursor-kind" "--display-name")
      ;;(message "%s" (buffer-string))
      (unless (= (point-min) (point-max))
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
            (when (string-match "^\\(.*:\\)\\([0-9]+\\)\\(:[0-9]+:\\)\t\\(.*\\)\t\\(.*\\)\t\\(.*\\)$" line)
                (let ((loc-start (match-string-no-properties 1 line))
                      (linenum (match-string-no-properties 2 line))
                      (loc-end (match-string-no-properties 3 line))
                      (context (match-string-no-properties 4 line))
                      (text (match-string-no-properties 5 line))
                      (type (match-string-no-properties 6 line)))
                  (cond ((or (string= type "FunctionDecl")
                             (string= type "CXXMethod")
                             (string= type "CXXConstructor")
                             (string= type "CXXDestructor"))
                         (if (string-match "\)\\(\\s-\\)*\\(const\\)?\\(;\\)?$" context)
                             (add-to-list 'functions
                                          (cons context
                                                (string-to-number linenum)))
                           (puthash linenum text incomplete-functions)))
                        ((string= type "ParmDecl")
                         (let ((fun (gethash linenum incomplete-functions)))
                           (cond (fun
                                  ;;(message "fun -> %s ( %s" fun text)
                                  (setq current-incomplete (cons (concat fun "(" text)
                                                                 (string-to-number linenum))))
                                 (current-incomplete
                                  ;;(message "current -> %s" text)
                                  (setf (car current-incomplete) (concat (car current-incomplete) ", " text))
                                  (when (pg/string-ends-with context ")")
                                    (setf (car current-incomplete) (concat (car current-incomplete) ")"))
                                    (add-to-list 'functions current-incomplete)
                                    (setq current-incomplete nil)))))))

                  ;; (cond ((or (string= type "FunctionDecl")
                  ;;            (string= type "CXXMethod")
                  ;;            (string= type "CXXConstructor")
                  ;;            (string= type "CXXDestructor"))
                  ;;        (add-to-list 'functions
                  ;;                     (cons (rtags-helm-propertize-function text)
                  ;;                           (string-to-number linenum))))
                  ;;       ((or (string= type "ClassDecl")
                  ;;            (string= type "StructDecl"))
                  ;;        (add-to-list 'classes
                  ;;                     (cons (propertize text 'face 'font-lock-type-face)
                  ;;                           (string-to-number linenum))))
                  ;;       ((string= type "FieldDecl")
                  ;;        (add-to-list 'variables
                  ;;                     (cons (rtags-helm-propertize-variable text)
                  ;;                           (string-to-number linenum))))
                  ;;       ((and rtags-helm-show-variables
                  ;;             (or (string= type "VarDecl")
                  ;;                 (string= type "ParmDecl")))
                  ;;        (add-to-list 'variables
                  ;;                     (cons (rtags-helm-propertize-variable text)
                  ;;                           (string-to-number linenum))))
                  ;;       ((and rtags-helm-show-enums
                  ;;             (or (string= type "EnumDecl")
                  ;;                 (string= type "EnumConstantDecl")))
                  ;;        (add-to-list 'enums
                  ;;                     (cons text (string-to-number linenum))))
                  ;;       ((or (string= type "macro definition")
                  ;;            (string= type "include directive")
                  ;;            (string= type "inclusion directive"))
                  ;;        (add-to-list 'macros
                  ;;                     (cons (rtags-helm-propertize-macro text)
                  ;;                           (string-to-number linenum))))
                  ;;       (t
                  ;;        (add-to-list 'other
                  ;;                     (cons text (string-to-number linenum)))))
                  )))
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
