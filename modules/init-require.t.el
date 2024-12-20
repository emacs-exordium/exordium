;;; init-require.t.el --- Tests for exordium-require -*- lexical-binding: t -*-

;;; Commentary:
;;
;; To run all tests:
;;     M-x eval-buffer
;;     M-x ert

;;; Code:

(eval-when-compile
  (unless (featurep 'init-require)
    (load (file-name-concat (locate-user-emacs-file "modules") "init-require"))))
(use-package use-package-core
  :ensure use-package
  :defer t
  :autoload (use-package-statistics-convert
             use-package-reset-statistics))


(declare-function exordium--require-load nil)

(defun exordium-require-t-recursive-find-if (predicate form)
  "Recursively find first subform in FORM that satisfies the PREDICATE.
Recursion is in order: FORM, (car FORM), (cdr FORM)."
  (when form
    (if (funcall predicate form)
        form
      (if-let* (((listp (car form)))
                (match (exordium-require-t-recursive-find-if
                        predicate (car form))))
          match
        (exordium-require-t-recursive-find-if predicate (cdr form))))))

(defun exordium--require-t-calls-exordium--require-load-within-eval-and-compile (form)
  "Non nil when the FORM has `exordium--require-load' within `eval-and-compile'."
  (exordium-require-t-recursive-find-if
   (lambda (form)
     (and (listp form)
          (eq #'exordium--require-load (car form))))
   (exordium-require-t-recursive-find-if
    (lambda (form)
      (and (listp form)
           (eq #'eval-and-compile (car form))))
    (macroexpand form))))


(ert-deftest exordium-require-basic-call ()
  (use-package-reset-statistics)
  (ignore-errors (unload-feature 'init-require.t-dummy))
  (let ((byte-compile-current-file nil)
        (use-package-compute-statistics nil)
        (form '(exordium-require 'init-require.t-dummy)))
    (should (eq 'init-require.t-dummy (eval form)))
    (should-not
     (exordium--require-t-calls-exordium--require-load-within-eval-and-compile
      form)))
  (should (featurep 'init-require.t-dummy))
  (should-error (use-package-statistics-convert 'init-require.t-dummy)))


(ert-deftest exordium-require-while-byte-compile ()
  (use-package-reset-statistics)
  (ignore-errors (unload-feature 'init-require.t-dummy))
  (let ((byte-compile-current-file t)
        (use-package-compute-statistics nil)
        (form '(exordium-require 'init-require.t-dummy)))
    (should (eq 'init-require.t-dummy
                (eval form)))
    (should
     (exordium--require-t-calls-exordium--require-load-within-eval-and-compile
      form)))
  (should (featurep 'init-require.t-dummy))
  (should-error (use-package-statistics-convert 'init-require.t-dummy)))

(ert-deftest exordium-require-with-stats ()
  (use-package-reset-statistics)
  (ignore-errors (unload-feature 'init-require.t-dummy))
  (let ((byte-compile-current-file nil)
        (use-package-compute-statistics t)
        (form '(exordium-require 'init-require.t-dummy)))
    (should (eq 'init-require.t-dummy
                (eval form)))
    (should-not
     (exordium--require-t-calls-exordium--require-load-within-eval-and-compile
      form)))
  (should (featurep 'init-require.t-dummy))
  (should (use-package-statistics-convert 'init-require.t-dummy)))

(ert-deftest exordium-require-with-stats-while-byte-compile ()
  (use-package-reset-statistics)
  (ignore-errors (unload-feature 'init-require.t-dummy))
  (let ((byte-compile-current-file t)
        (use-package-compute-statistics t)
        (form '(exordium-require 'init-require.t-dummy)))
    (should (eq 'init-require.t-dummy
                (eval form)))
    (should
     (exordium--require-t-calls-exordium--require-load-within-eval-and-compile
      form)))
  (should (featurep 'init-require.t-dummy))
  (should (use-package-statistics-convert 'init-require.t-dummy)))


(ert-deftest exordium-require-with-location ()
  ;; delay `exordium-require' macro expansion to test execution
  (ignore-errors (unload-feature 'init-require.t-dummy))
  (should (eq 'init-require.t-dummy
              (eval '(exordium-require 'init-require.t-dummy
                       :location "modules"))))
  (should (featurep 'init-require.t-dummy)))

(ert-deftest exordium-require-with-location-and-declarations ()
  ;; delay `exordium-require' macro expansion to test execution
  (ignore-errors (unload-feature 'init-require.t-dummy))
  (should
   (eq 'init-require.t-dummy
       (eval '(exordium-require
                  'init-require.t-dummy
                :location "modules"
                :functions (require.t-dummy-1
                            (require.t-dummy-2 . (arg1 arg2)))
                :defines (require.t-dummy-3)))))
  (should (featurep 'init-require.t-dummy)))

(ert-deftest exordium-require-swallow-error-when-called-with-non-existing-feature ()
  (ignore-errors (unload-feature 'no-such-feature))
  (let* ((debug-on-error nil))
    (should-not (eq 'no-such-feature
                    (eval '(exordium-require 'no-such-feature)))))
  (should-not (featurep 'no-such-feature)))

(ert-deftest exordium-require-signals-error-with-non-existing-feature-while-debug ()
  "TODO: the `should-error' is not catching a file-missing signaled from `load'."
  :expected-result :failed
  (skip-unless (not (getenv "ci_tests")))
  (ignore-errors (unload-feature 'no-such-feature))
  (let* ((debug-on-error t))
    (should-error (eval '(exordium-require 'no-such-feature))))
  (should-not (featurep 'no-such-feature)))

(ert-deftest exordium-require-swallow-error-when-called-with-non-existing-location ()
  (ignore-errors (unload-feature 'init-require.t-dummy))
  (let* ((debug-on-error nil))
    (should-not (eq 'init-require.t-dummy
                    (eval '(exordium-require 'init-require.t-dummy
                             :location "themes")))))
  (should-not (featurep 'init-require.t-dummy)))

(ert-deftest exordium-require-signals-error-with-non-existing-location-while-debug ()
  "TODO: the `should-error' is not catching a file-missing signaled from `load'."
  :expected-result :failed
  (skip-unless (not (getenv "ci_tests")))
  (ignore-errors (unload-feature 'init-require.t-dummy))
  (let* ((debug-on-error t))
    (should-error (eval '(exordium-require 'init-require.t-dummy "themes"))))
  (should-not (featurep 'init-require.t-dummy)))


(ert-deftest exordium-require-signals-error-with-wrong-feature-type ()
  (should-error (eval '(exordium-require nil)))
  (should-error (eval '(exordium-require t)))
  (should-error (eval '(exordium-require 42)))
  (should-error (eval '(exordium-require ? )))
  (should-error (eval '(exordium-require init-require.t-dummy)))
  (should-error (eval '(exordium-require :init-require.t-dummy)))
  (should-error (eval '(exordium-require "init-require.t-dummy")))
  (should-error (eval '(exordium-require
                           (if t 'init-require.t-dummy
                             init-require.t-dummy)))))

(ert-deftest exordium-require-signals-error-with-wrong-location-type ()
  (should-error (eval '(exordium-require 'init-require.t-dummy
                         :location "")))
  (should-error (eval '(exordium-require 'init-require.t-dummy
                         :location t)))
  (should-error (eval '(exordium-require 'init-require.t-dummy
                         :location 42)))
  (should-error (eval '(exordium-require 'init-require.t-dummy
                         :location ? )))
  (should-error (eval '(exordium-require 'init-require.t-dummy
                         :location 'modules)))
  (should-error (eval '(exordium-require 'init-require.t-dummy
                         :location :modules)))
  (should-error (eval '(exordium-require 'init-require.t-dummy
                         :location modules)))
  (should-error (eval '(exordium-require 'init-require.t-dummy
                           :location (if t "modules" "themes")))))


(ert-deftest exordium-require-signals-error-with-wrong-defines-type ()
  (should-error (eval '(exordium-require
                           'init-require.t-dummy
                         :defines (nil))))
  (should-error (eval '(exordium-require
                           'init-require.t-dummy
                         :defines (t))))
  (should-error (eval '(exordium-require
                           'init-require.t-dummy
                         :defines (42))))
  (should-error (eval '(exordium-require
                           'init-require.t-dummy
                         :defines ("var"))))
  (should-error (eval '(exordium-require
                           'init-require.t-dummy
                         :defines ('var))))
  (should-error (eval '(exordium-require
                           'init-require.t-dummy
                         :defines (:var))))
  (should-error (eval '(exordium-require
                           'init-require.t-dummy
                         :defines ((if t 'var 'var))))))

(ert-deftest exordium-require-signals-error-with-wrong-functions-type ()
  ;; invalid function without args
  (should-error (eval '(exordium-require
                           'init-require.t-dummy
                         :functions (nil))))
  (should-error (eval '(exordium-require
                           'init-require.t-dummy
                         :functions (t))))
  (should-error (eval '(exordium-require
                           'init-require.t-dummy
                         :functions (42))))
  (should-error (eval '(exordium-require
                           'init-require.t-dummy
                         :functions ("fn"))))
  (should-error (eval '(exordium-require
                           'init-require.t-dummy
                         :functions ('fn))))
  (should-error (eval '(exordium-require
                           'init-require.t-dummy
                         :functions (:fn))))
  (should-error (eval '(exordium-require
                           'init-require.t-dummy
                         :functions ((nil . (arg))))))
  ;; invalid function with valid arg
  (should-error (eval '(exordium-require
                           'init-require.t-dummy
                         :functions ((t . (arg))))))
  (should-error (eval '(exordium-require
                           'init-require.t-dummy
                         :functions ((42 . (arg))))))
  (should-error (eval '(exordium-require
                           'init-require.t-dummy
                         :functions (("fn" . (arg))))))
  (should-error (eval '(exordium-require
                           'init-require.t-dummy
                         :functions (('fn . (arg))))))
  (should-error (eval '(exordium-require
                           'init-require.t-dummy
                         :functions ((:fn . (arg))))))
  ;; valid function with invalid arg
  (should-error (eval '(exordium-require
                           'init-require.t-dummy
                         :functions ((fn . (nil))))))
  (should-error (eval '(exordium-require
                           'init-require.t-dummy
                         :functions ((fn . (t))))))
  (should-error (eval '(exordium-require
                           'init-require.t-dummy
                         :functions ((fn . (42))))))
  (should-error (eval '(exordium-require
                           'init-require.t-dummy
                         :functions ((fn . ("arg"))))))
  (should-error (eval '(exordium-require
                           'init-require.t-dummy
                         :functions ((fn . ('arg))))))
  (should-error (eval '(exordium-require
                           'init-require.t-dummy
                         :functions ((fn . (:arg))))))
  (should-error (eval '(exordium-require
                           'init-require.t-dummy
                         :functions ((fn . '(arg)))))))

(provide 'init-require.t)

;;; init-require.t.el ends here
