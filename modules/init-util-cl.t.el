;;; Unit tests for init-util.el obsolete cl aliases.
;;; To run all tests:
;;;     M-x eval-buffer
;;;     M-x ert

(require 'init-util)
(require 'ert)
(require 'cl-lib)


;; Test to ensure no new obsolete `cl' aliases are added.
;; It utilises `exordium-refs-cl-aliases'
;; to find all aliases.

(defvar exordium--test-found-obsolete-aliases)

(defun exordium--test-collect-aliases (orig-fun &rest args)
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

;; Local Variables:
;; no-byte-compile: t
;; End:
