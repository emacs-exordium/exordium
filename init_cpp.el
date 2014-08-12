;;;; Configuration for C++
;;;
;;; -------------- -------------------------------------------------------
;;; Key            Definition
;;; -------------- -------------------------------------------------------
;;; Ctrl-tab       Switch between .h and .cpp
;;; Ctrl-c =       Insert BDE class header (definition)
;;; Ctrl-c -       Insert BDE class header (implementation)
;;; Ctrl-c ;       IEdit mode (rename selected variable)


(require 'bde-style)
(require 'bde-util)

;;; Ctrl-Tab to switch between .h and .cpp
(global-set-key [(control tab)] 'bde-switch-h-cpp)

;;; Ctrl-C = and Ctrl-C - for class header
(global-set-key [(control c)(=)] 'bde-insert-define-class-header)
(global-set-key [(control c)(-)] 'bde-insert-declare-class-header)

;;; Highlight dead code between "#if 0" and "#endif"
(add-hook 'c-mode-common-hook 'bde-highlight-dead-code-hook)

;;; Ctrl-> to right-aligh the text after point
(global-set-key [(control >)] 'bde-aligh-right-after-point)

;;; IEdit: rename the symbol under point
;;; Fix A bug (normal key is "C-;")
(define-key global-map (kbd "C-c ;") 'iedit-mode)
