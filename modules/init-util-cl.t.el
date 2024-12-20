;;; init-util-cl.t.el --- Unit tests for init-util.el obsolete cl aliases -*- lexical-binding: t -*-

;;; Commentary:
;;
;; To run all tests:
;;     M-x eval-buffer
;;     M-x ert

;;; Code:

(eval-when-compile
  (unless (featurep 'init-require)
    (load (file-name-concat (locate-user-emacs-file "modules") "init-require"))))
(exordium-require 'init-util)
(require 'ert)
(require 'cl-lib)


;; Test to ensure no new obsolete `cl' aliases are added.
;; It utilises `exordium-refs-cl-aliases'
;; to find all aliases.

(defvar exordium--test-found-obsolete-aliases nil)

(defun exordium--test-collect-aliases (_orig-fun &rest args)
                                        ; checkdoc-params: (_orig-fun args)
  "Collect results into `exordium--test-found-obsoltete-aliases'.

This is meant to be applied as an advice around `elisp-refs--show-results'."
  (when-let* ((files (mapcar (lambda (result)
                               (string-trim-right
                                (string-trim-left (buffer-name (cdr result))
                                                  " \\*refs-")
                                "\\*"))
                             (cl-third args))))
    (add-to-list 'exordium--test-found-obsolete-aliases
                 (cons (cl-first args) files))))

(ert-deftest test-exordium-no-obsolete-aliases ()
  (setq exordium--test-found-obsolete-aliases nil)
  (let ((exordium-refs-cl-aliases--advice #'exordium--test-collect-aliases))
    (exordium-refs-cl-aliases))
  (should (eq exordium--test-found-obsolete-aliases nil)))


(provide 'init-util-cl.t)

;;; init-util-cl.t.el ends here
