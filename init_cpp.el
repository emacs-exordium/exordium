;;;; Configuration for C++

(require 'bde-style)
(require 'bde-util);

;;; Ctrl-Tab to switch between .h and .cpp
(global-set-key [(control tab)] 'bde-switch-h-cpp)

;;; Ctrl-C = and Ctrl-C - for class header
(global-set-key [(control c)(=)] 'bde-insert-define-class-header)
(global-set-key [(control c)(-)] 'bde-insert-declare-class-header)

;;; Highlight dead code between "#if 0" and "#endif"
;;(setq *bde-highlight-dead-code-color* "darkred")
;;(add-hook 'c-mode-common-hook 'bde-highlight-dead-code)
