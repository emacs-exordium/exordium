;;; init-vc-checkout.t.el --- Tests for vc-checkout -*- lexical-binding: t -*-

;;; Commentary:
;;
;; To run all tests:
;;     M-x eval-buffer
;;     M-x ert

;;; Code:

(eval-when-compile
  (unless (featurep 'init-require)
    (load (file-name-concat (locate-user-emacs-file "modules") "init-require"))))

(exordium-require 'init-vc-checkout)

(use-package el-mock
  :ensure t
  :autoload (mocklet
             mocklet-function))

(require 'package)
(require 'use-package)


(defun exordium-vc-checkout-t-recursive-find-if (predicate form)
  "Recursively find first subform in FORM that satisfies the PREDICATE.
Recursion is in order: FORM, (car FORM), (cdr FORM)."
  (when form
    (if (funcall predicate form)
        form
      (if-let* (((listp (car form)))
                (match (exordium-vc-checkout-t-recursive-find-if
                        predicate (car form))))
          match
        (exordium-vc-checkout-t-recursive-find-if predicate (cdr form))))))

(defun exordium--vc-checkout-install-exists (package dir form)
  "Determine `exordium-vc-checkout' call with PACKAGE and DIR args is in FORM."
  (exordium-vc-checkout-t-recursive-find-if
   (lambda (f)
     (and (eq 'exordium--vc-checkout-install (car f))
          (or (eq package 'any) (eq package (eval (cadr f))))
          (or (eq dir 'any) (equal dir (caddr f)))))
   form))


(ert-deftest exordium-vc-checkout-not-always-macroexpansion-no-keyword ()
  (let ((exordium-always-vc-checkout nil)
        (form '(use-package dummy-package)))
    (should-not
     (exordium--vc-checkout-install-exists 'any 'any
                                           (macroexpand form)))))

(ert-deftest exordium-vc-checkout-always-macroexpansion-no-keyword ()
  (let ((exordium-always-vc-checkout t)
        (form '(use-package dummy-package)))
    (should
     (exordium--vc-checkout-install-exists 'dummy-package nil
                                           (macroexpand form)))))

(ert-deftest exordium-vc-checkout-not-always-macroexpansion-no-args ()
  (let ((exordium-always-vc-checkout nil)
        (form '(use-package dummy-package
                 :exordium-vc-checkout)))
    (should
     (exordium--vc-checkout-install-exists 'dummy-package nil
                                           (macroexpand form)))))

(ert-deftest exordium-vc-checkout-always-macroexpansion-no-args ()
  (let ((exordium-always-vc-checkout t)
        (form '(use-package dummy-package
                 :exordium-vc-checkout)))
    (should
     (exordium--vc-checkout-install-exists 'dummy-package nil
                                           (macroexpand form)))))

(ert-deftest exordium-vc-checkout-not-always-macroexpansion-nil ()
  (let ((exordium-always-vc-checkout nil)
        (form '(use-package dummy-package
                 :exordium-vc-checkout nil)))
    (should-not
     (exordium--vc-checkout-install-exists 'any 'any
                                           (macroexpand form)))))

(ert-deftest exordium-vc-checkout-always-macroexpansion-nil ()
  (let ((exordium-always-vc-checkout t)
        (form '(use-package dummy-package
                 :exordium-vc-checkout nil)))
    (should-not
     (exordium--vc-checkout-install-exists 'any 'any
                                           (macroexpand form)))))

(ert-deftest exordium-vc-checkout-not-always-macroexpansion-t ()
  (let ((exordium-always-vc-checkout nil)
        (form '(use-package dummy-package
                 :exordium-vc-checkout t)))
    (should
     (exordium--vc-checkout-install-exists 'dummy-package nil
                                           (macroexpand form)))))

(ert-deftest exordium-vc-checkout-always-macroexpansion-t ()
  (let ((exordium-always-vc-checkout t)
        (form '(use-package dummy-package
                 :exordium-vc-checkout t)))
    (should
     (exordium--vc-checkout-install-exists 'dummy-package nil
                                           (macroexpand form)))))

(ert-deftest exordium-vc-checkout-not-always-macroexpansion-dir ()
  (let ((exordium-always-vc-checkout nil)
        (form '(use-package dummy-package
                 :exordium-vc-checkout "/a/path")))
    (should
     (exordium--vc-checkout-install-exists 'dummy-package "/a/path"
                                           (macroexpand form)))))

(ert-deftest exordium-vc-checkout-always-macroexpansion-dir ()
  (let ((exordium-always-vc-checkout t)
        (form '(use-package dummy-package
                 :exordium-vc-checkout "/a/path")))
    (should
     (exordium--vc-checkout-install-exists 'dummy-package "/a/path"
                                           (macroexpand form)))))

(ert-deftest exordium-vc-checkout-not-always-macroexpansion-no-args-ensure-t ()
  (let ((exordium-always-vc-checkout nil)
        (form '(use-package dummy-package
                 :ensure t
                 :exordium-vc-checkout)))
    (should
     (exordium--vc-checkout-install-exists 'dummy-package nil
                                           (macroexpand form)))))

(ert-deftest exordium-vc-checkout-not-always-macroexpansion-dir-ensure-t ()
  (let ((exordium-always-vc-checkout nil)
        (form '(use-package dummy-package
                 :ensure t
                 :exordium-vc-checkout "/a/path")))
    (should
     (exordium--vc-checkout-install-exists 'dummy-package "/a/path"
                                           (macroexpand form)))))

(ert-deftest exordium-vc-checkout-always-macroexpansion-no-keyword-ensure-t ()
  (let ((exordium-always-vc-checkout t)
        (form '(use-package dummy-package
                 :ensure t)))
    (should
     (exordium--vc-checkout-install-exists 'dummy-package nil
                                           (macroexpand form)))))

(ert-deftest exordium-vc-checkout-not-always-macroexpansion-no-args-always-ensure ()
  (let ((exordium-always-vc-checkout nil)
        (use-package-always-ensure t)
        (form '(use-package dummy-package
                 :exordium-vc-checkout)))
    (should
     (exordium--vc-checkout-install-exists 'dummy-package nil
                                           (macroexpand form)))))

(ert-deftest exordium-vc-checkout-not-always-macroexpansion-dir-always-ensure ()
  (let ((exordium-always-vc-checkout nil)
        (use-package-always-ensure t)
        (form '(use-package dummy-package
                 :exordium-vc-checkout "/a/path")))
    (should
     (exordium--vc-checkout-install-exists 'dummy-package "/a/path"
                                           (macroexpand form)))))

(ert-deftest exordium-vc-checkout-always-macroexpansion-no-keyword-always-ensure ()
  (let ((exordium-always-vc-checkout t)
        (use-package-always-ensure t)
        (form '(use-package dummy-package)))
    (should
     (exordium--vc-checkout-install-exists 'dummy-package nil
                                           (macroexpand form)))))


(ert-deftest exordium-vc-checkout-not-always-macroexpansion-no-args-ensure-other ()
  (let ((exordium-always-vc-checkout nil)
        (form '(use-package dummy-package
                 :ensure other-package
                 :exordium-vc-checkout)))
    (should
     (exordium--vc-checkout-install-exists 'other-package nil
                                           (macroexpand form)))))

(ert-deftest exordium-vc-checkout-not-always-macroexpansion-dir-ensure-other ()
  (let ((exordium-always-vc-checkout nil)
        (form '(use-package dummy-package
                 :ensure other-package
                 :exordium-vc-checkout "/a/path")))
    (should
     (exordium--vc-checkout-install-exists 'other-package "/a/path"
                                           (macroexpand form)))))

(ert-deftest exordium-vc-checkout-always-macroexpansion-no-keyword-ensure-other ()
  (let ((exordium-always-vc-checkout t)
        (form '(use-package dummy-package
                 :ensure (other-package :pin gnu))))
    (should
     (exordium--vc-checkout-install-exists 'other-package nil
                                           (macroexpand form)))))


(ert-deftest exordium-vc-checkout-not-always-macroexpansion-no-args-ensure-other-pin ()
  (let ((exordium-always-vc-checkout nil)
        (form '(use-package dummy-package
                 :ensure (other-package :pin gnu)
                 :exordium-vc-checkout)))
    (should
     (exordium--vc-checkout-install-exists 'other-package nil
                                           (macroexpand form)))))

(ert-deftest exordium-vc-checkout-not-always-macroexpansion-dir-ensure-other-pin ()
  (let ((exordium-always-vc-checkout nil)
        (form '(use-package dummy-package
                 :ensure (other-package :pin gnu)
                 :exordium-vc-checkout "/a/path")))
    (should
     (exordium--vc-checkout-install-exists 'other-package "/a/path"
                                           (macroexpand form)))))

(ert-deftest exordium-vc-checkout-always-macroexpansion-no-keyword-ensure-other-pin ()
  (let ((exordium-always-vc-checkout t)
        (form '(use-package dummy-package
                 :ensure (other-package :pin gnu))))
    (should
     (exordium--vc-checkout-install-exists 'other-package nil
                                           (macroexpand form)))))

(ert-deftest exordium--vc-checkout-valid-p-basic ()
  (should-not (exordium--vc-checkout-valid-p nil))
  (should-not (exordium--vc-checkout-valid-p "/non-existing-directory"))
  (let ((file (make-temp-file "exordium-vc-checkout-test-")))
    (unwind-protect
        (should-not (exordium--vc-checkout-valid-p file))
      (delete-file file)))
  (let ((dir (make-temp-file "exordium-vc-checkout-test-" 'dir)))
    (unwind-protect
        (should-not (exordium--vc-checkout-valid-p dir))
      (delete-directory dir 'recursive)))

  (when-let* ((dir (make-temp-file "exordium-vc-checkout-test-" 'dir))
              (git (executable-find "git")))
    (unwind-protect
        (let ((default-directory dir))
          (should (eq 0 (with-temp-buffer
                          (let ((ret (shell-command (format "%s init" git)
                                                    nil (current-buffer))))
                            ;; On error return ERROR-OUTPUT such that `ert' can
                            ;; display it.
                            (if (eq 0 ret)
                                ret
                              (buffer-string))))))
          (should (exordium--vc-checkout-valid-p dir)))
      (delete-directory dir 'recursive))))


(ert-deftest use-package-normalize/:exordium-vc-checkout-basic ()
  (should-not (use-package-normalize/:exordium-vc-checkout
                  'dummy-package :exordium-vc-checkout '(nil)))
  (should (equal (use-package-normalize/:exordium-vc-checkout
                  'dummy-package :exordium-vc-checkout '(t))
                 '(dummy-package)))
  (should (equal (use-package-normalize/:exordium-vc-checkout
                  'dummy-package :exordium-vc-checkout '("/a/path"))
                 '(dummy-package "/a/path")))

  (should (equal (use-package-normalize/:exordium-vc-checkout
                  'dummy-package :exordium-vc-checkout nil)
                 '(dummy-package)))
  (should-error (use-package-normalize/:exordium-vc-checkout
                 'dummy-package :exordium-vc-checkout t))
  (should-error (use-package-normalize/:exordium-vc-checkout
                 'dummy-package :exordium-vc-checkout 1))
  (should-error (use-package-normalize/:exordium-vc-checkout
                 'dummy-package :exordium-vc-checkout '("/a/path" .
                                                        "/other/path")))
  (should-error (use-package-normalize/:exordium-vc-checkout
                 'dummy-package :exordium-vc-checkout '("/a/path"
                                                        "/other/path"))))


(ert-deftest exordium--vc-checkout-package-delete-basic ()
  (let ((load-path '("/a/path" "/other/path"))
        (desc (package-desc-create :name 'dummy-package
                                   :dir "/a/path")))
    (eval
     `(mocklet (((package-delete ,desc 'force) :times 1))
        (exordium--vc-checkout-package-delete ,desc)
        (should (equal load-path '("/other/path")))))))


(ert-deftest exordium--vc-checkout-install-not-intalled ()
  (let (package-alist)
    (mocklet (((exordium--vc-checkout-valid-p "/a/path") => t)
              (exordium--vc-checkout-package-delete not-called)
              ((package-vc-install-from-checkout
                "/a/path" "dummy-package")
               :times 1))
      (exordium--vc-checkout-install 'dummy-package "/a/path"))))

(ert-deftest exordium--vc-checkout-install-intalled-from-vc ()
  (let* ((desc (package-desc-create :name 'dummy-package
                                    :kind 'vc
                                    :dir "/other/path"))
         (package-alist `((dummy-package ,desc))))
    (eval
     `(mocklet (((exordium--vc-checkout-valid-p "/a/path") => t)
                ((exordium--vc-checkout-package-delete ,desc) :times 1)
                ((package-vc-install-from-checkout
                  "/a/path" "dummy-package")
                 :times 1))
        (exordium--vc-checkout-install 'dummy-package "/a/path")))))

(ert-deftest exordium--vc-checkout-install-intalled-from-vc-same-dir ()
  (let* ((desc (package-desc-create :name 'dummy-package
                                    :kind 'vc
                                    :dir "/a/path"))
         (package-alist `((dummy-package ,desc))))
    (eval
     `(mocklet (((exordium--vc-checkout-valid-p "/a/path") => t)
                (exordium--vc-checkout-package-delete not-called)
                (package-vc-install-from-checkout not-called))
        (exordium--vc-checkout-install 'dummy-package "/a/path")))))

(ert-deftest exordium--vc-checkout-install-intalled-from-elsewhere ()
  (let* ((desc (package-desc-create :name 'dummy-package
                                    :dir "/other/path"))
         (package-alist `((dummy-package ,desc))))
    (eval
     `(mocklet (((exordium--vc-checkout-valid-p "/a/path") => t)
                ((exordium--vc-checkout-package-delete ,desc) :times 1)
                ((package-vc-install-from-checkout
                  "/a/path" "dummy-package")
                 :times 1))
        (exordium--vc-checkout-install 'dummy-package "/a/path")))))

(ert-deftest exordium--vc-checkout-install-intalled-from-elsewhere-same-dir ()
  (let* ((desc (package-desc-create :name 'dummy-package
                                    :dir "/a/path"))
         (package-alist `((dummy-package ,desc))))
    (eval
     `(mocklet (((exordium--vc-checkout-valid-p "/a/path") => t)
                ((exordium--vc-checkout-package-delete ,desc) :times 1)
                ((package-vc-install-from-checkout
                  "/a/path" "dummy-package")
                 :times 1))
        (exordium--vc-checkout-install 'dummy-package "/a/path")))))


(provide 'init-vc-checkout.t)

;;; init-vc-checkout.t.el ends here
