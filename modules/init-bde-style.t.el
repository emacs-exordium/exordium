;;; Unit tests for bde-style.el.
;;; To run all tests:
;;;     M-x eval-buffer
;;;     M-x ert

(with-no-warnings (require 'cl))
(require 'init-bde-style)

;; Test apparatus

(cl-defstruct test-case
  input               ;; string: initial buffer content
  (output nil)        ;; string: expected final buffer content
  (use-region nil)    ;; select the whole buffer?
  (cursor-pos 0))     ;; move cursor to this position (nil = end of buffer)

(defmacro with-test-case-output (tst &rest body)
  "Execute BODY using a temporary buffer created according to test case TST.
TST defines the inital content, cursor position, and if a region
is selected or not. Then compare the content of the buffer with
the expected output as defined in TST. Return the final buffer
content string"
  (declare (indent defun))
  `(let ((input      (test-case-input ,tst))
         (output     (test-case-output ,tst))
         (use-region (test-case-use-region ,tst))
         (cursor-pos (test-case-cursor-pos ,tst)))
     (with-temp-buffer
       ;; Set the initial state of the buffer
       (rename-buffer "mqbnet_foo.h")
       (c++-mode)
       (insert input)
       (when cursor-pos (goto-char cursor-pos))
       (when use-region (mark-whole-buffer))
       ;; Execute body
       ,@body
       ;; Return the content of the buffer as raw string (without font-lock)
       (substring-no-properties (buffer-string)))))

(defmacro with-test-case-return (tst &rest body)
  "Execute BODY using a temporary buffer created according to test case TST.
TST defines the inital content, cursor position, and if a region
is selected or not. Return the body return value."
  (declare (indent defun))
  `(let ((input      (test-case-input ,tst))
         (use-region (test-case-use-region ,tst))
         (cursor-pos (test-case-cursor-pos ,tst)))
     (with-temp-buffer
       ;; Set the initial state of the buffer
       (rename-buffer "mqbnet_foo.h")
       (c++-mode)
       (insert input)
       (when cursor-pos (goto-char cursor-pos))
       (when use-region (mark-whole-buffer))
       ;; Execute body
       ,@body)))


;; Tests for `bde-insert-redundant-include-guard-region'

(ert-deftest test-redundant-include-guard-1 ()
  "Breathing test: single #include, no region"
  (let ((tst (make-test-case :input "#include <mqbscm_version.h>
"
                             :output "#ifndef INCLUDED_MQBSCM_VERSION
#include <mqbscm_version.h>
#endif
")))
    (should (string= (with-test-case-output tst
                       (bde-insert-redundant-include-guard-region))
                   (test-case-output tst)))))

(ert-deftest test-redundant-include-guard-2 ()
  "Test bugfix: output was wrong without a trailing newline"
  (let ((tst (make-test-case :input "#include <mqbscm_version.h>" ; no newline
                             :output "#ifndef INCLUDED_MQBSCM_VERSION
#include <mqbscm_version.h>
#endif
"
                             :cursor-pos nil)))
    (should (string= (with-test-case-output tst
                       (bde-insert-redundant-include-guard-region))
                     (test-case-output tst)))))



;; Tests for `bde-align-funcall'

(ert-deftest test-bde-align-funcall ()
  (let ((tst (make-test-case :input "
{
    bslma::ManagedPtr<BufferedMessage> message(
          groupInfo->bufferedMessages()[0], &d_bufferedMessagePool);
}
"
                             :output "
{
    bslma::ManagedPtr<BufferedMessage> message(
                                              groupInfo->bufferedMessages()[0],
                                              &d_bufferedMessagePool);
}
"
                             :cursor-pos 62)))
    (should (string= (with-test-case-output tst
                       (bde-align-funcall))
                     (test-case-output tst)))))


;; Tests for `bde-is-member-function-declaration'


(ert-deftest test-bde-is-member-function-declaration-basic-1 ()
  (let ((tst (make-test-case :input "void foo();")))
    (should (with-test-case-return tst (bde-is-member-function-declaration)))))

(ert-deftest test-bde-is-member-function-declaration-basic-2 ()
  (let ((tst (make-test-case :input "void foo() ;")))
    (should (with-test-case-return tst (bde-is-member-function-declaration)))))

(ert-deftest test-bde-is-member-function-declaration-basic-3 ()
  (let ((tst (make-test-case :input "void foo(); ")))
    (should (with-test-case-return tst (bde-is-member-function-declaration)))))

(ert-deftest test-bde-is-member-function-declaration-basic-4 ()
  (let ((tst (make-test-case :input "void foo()")))
    (should (not (with-test-case-return tst (bde-is-member-function-declaration))))))

(ert-deftest test-bde-is-member-function-declaration-c-1 ()
  (let ((tst (make-test-case :input "void foo() const;")))
    (should (with-test-case-return tst (bde-is-member-function-declaration)))))

(ert-deftest test-bde-is-member-function-declaration-c-2 ()
  (let ((tst (make-test-case :input "void foo() const ;")))
    (should (with-test-case-return tst (bde-is-member-function-declaration)))))

(ert-deftest test-bde-is-member-function-declaration-c-3 ()
  (let ((tst (make-test-case :input "void foo() const; ")))
    (should (with-test-case-return tst (bde-is-member-function-declaration)))))

(ert-deftest test-bde-is-member-function-declaration-c-4 ()
  (let ((tst (make-test-case :input "void foo() const")))
    (should (not (with-test-case-return tst (bde-is-member-function-declaration))))))

(ert-deftest test-bde-is-member-function-declaration-basic-pure-1 ()
  (let ((tst (make-test-case :input "void foo()=0;")))
    (should (with-test-case-return tst (bde-is-member-function-declaration)))))

(ert-deftest test-bde-is-member-function-declaration-basic-pure-2 ()
  (let ((tst (make-test-case :input "void foo() = 0 ;")))
    (should (with-test-case-return tst (bde-is-member-function-declaration)))))

(ert-deftest test-bde-is-member-function-declaration-basic-pure-3 ()
  (let ((tst (make-test-case :input "void foo() =0 ; ")))
    (should (with-test-case-return tst (bde-is-member-function-declaration)))))

(ert-deftest test-bde-is-member-function-declaration-basic-pure-4 ()
  (let ((tst (make-test-case :input "void foo()= 0;")))
    (should (with-test-case-return tst (bde-is-member-function-declaration)))))

(ert-deftest test-bde-is-member-function-declaration-c-pure-1 ()
  (let ((tst (make-test-case :input "void foo() const=0;")))
    (should (with-test-case-return tst (bde-is-member-function-declaration)))))

(ert-deftest test-bde-is-member-function-declaration-c-pure-2 ()
  (let ((tst (make-test-case :input "void foo() const = 0 ;")))
    (should (with-test-case-return tst (bde-is-member-function-declaration)))))

(ert-deftest test-bde-is-member-function-declaration-c-pure-3 ()
  (let ((tst (make-test-case :input "void foo() const =0; ")))
    (should (with-test-case-return tst (bde-is-member-function-declaration)))))

(ert-deftest test-bde-is-member-function-declaration-c-pure-4 ()
  (let ((tst (make-test-case :input "void foo() const= 0;")))
    (should (with-test-case-return tst (bde-is-member-function-declaration)))))

(ert-deftest test-bde-is-member-function-declaration-basic-delete-1 ()
  (let ((tst (make-test-case :input "void foo()=delete;")))
    (should (with-test-case-return tst (bde-is-member-function-declaration)))))

(ert-deftest test-bde-is-member-function-declaration-basic-delete-2 ()
  (let ((tst (make-test-case :input "void foo() = delete ;")))
    (should (with-test-case-return tst (bde-is-member-function-declaration)))))

(ert-deftest test-bde-is-member-function-declaration-basic-delete-3 ()
  (let ((tst (make-test-case :input "void foo() =delete ; ")))
    (should (with-test-case-return tst (bde-is-member-function-declaration)))))

(ert-deftest test-bde-is-member-function-declaration-basic-delete-4 ()
  (let ((tst (make-test-case :input "void foo()= delete;")))
    (should (with-test-case-return tst (bde-is-member-function-declaration)))))

(ert-deftest test-bde-is-member-function-declaration-basic-delete-pure-1 ()
  (let ((tst (make-test-case :input "void foo() = delete =0;")))
    (should (not (with-test-case-return tst (bde-is-member-function-declaration))))))

(ert-deftest test-bde-is-member-function-declaration-basic-delete-pure-2 ()
  (let ((tst (make-test-case :input "void foo() = delete = 0;")))
    (should (not (with-test-case-return tst (bde-is-member-function-declaration))))))

(ert-deftest test-bde-is-member-function-declaration-basic-default-1 ()
  (let ((tst (make-test-case :input "void foo()=default;")))
    (should (with-test-case-return tst (bde-is-member-function-declaration)))))

(ert-deftest test-bde-is-member-function-declaration-basic-default-2 ()
  (let ((tst (make-test-case :input "void foo() = default ;")))
    (should (with-test-case-return tst (bde-is-member-function-declaration)))))

(ert-deftest test-bde-is-member-function-declaration-basic-default-3 ()
  (let ((tst (make-test-case :input "void foo() =default ; ")))
    (should (with-test-case-return tst (bde-is-member-function-declaration)))))

(ert-deftest test-bde-is-member-function-declaration-basic-default-4 ()
  (let ((tst (make-test-case :input "void foo()= default;")))
    (should (with-test-case-return tst (bde-is-member-function-declaration)))))

(ert-deftest test-bde-is-member-function-declaration-basic-default-pure-1 ()
  (let ((tst (make-test-case :input "void foo() = default =0;")))
    (should (not (with-test-case-return tst (bde-is-member-function-declaration))))))

(ert-deftest test-bde-is-member-function-declaration-basic-default-pure-2 ()
  (let ((tst (make-test-case :input "void foo() = default = 0;")))
    (should (not (with-test-case-return tst (bde-is-member-function-declaration))))))

(ert-deftest test-bde-is-member-function-declaration-basic-default-delete-1 ()
  (let ((tst (make-test-case :input "void foo() = default delete;")))
    (should (not (with-test-case-return tst (bde-is-member-function-declaration))))))

(ert-deftest test-bde-is-member-function-declaration-basic-default-delete-2 ()
  (let ((tst (make-test-case :input "void foo() = default = delete;")))
    (should (not (with-test-case-return tst (bde-is-member-function-declaration))))))

(ert-deftest test-bde-is-member-function-declaration-basic-bsl-delete-1 ()
  (let ((tst (make-test-case :input "void foo() BSLS_CPP11_DELETED;")))
    (should (with-test-case-return tst (bde-is-member-function-declaration)))))

(ert-deftest test-bde-is-member-function-declaration-basic-bsl-delete-2 ()
  (let ((tst (make-test-case :input "void foo() BSLS_CPP11_DELETED ;")))
    (should (with-test-case-return tst (bde-is-member-function-declaration)))))

(ert-deftest test-bde-is-member-function-declaration-basic-bsl-default-1 ()
  (let ((tst (make-test-case :input "void foo() BSLS_CPP11_DEFAULT;")))
    (should (with-test-case-return tst (bde-is-member-function-declaration)))))

(ert-deftest test-bde-is-member-function-declaration-basic-bsl-default-2 ()
  (let ((tst (make-test-case :input "void foo() BSLS_CPP11_DEFAULT ;")))
    (should (with-test-case-return tst (bde-is-member-function-declaration)))))

(ert-deftest test-bde-is-member-function-declaration-basic-bsl-default-deleted-1 ()
  (let ((tst (make-test-case :input "void foo() BSLS_CPP11_DEFAULT BSLS_CPP11_DELETED;")))
    (should (not (with-test-case-return tst (bde-is-member-function-declaration))))))

(ert-deftest test-bde-is-member-function-declaration-basic-override-1 ()
  (let ((tst (make-test-case :input "void foo() override;")))
    (should (with-test-case-return tst (bde-is-member-function-declaration)))))

(ert-deftest test-bde-is-member-function-declaration-basic-override-2 ()
  (let ((tst (make-test-case :input "void foo()  override ;")))
    (should (with-test-case-return tst (bde-is-member-function-declaration)))))

(ert-deftest test-bde-is-member-function-declaration-basic-override-3 ()
  (let ((tst (make-test-case :input "void foo() override; ")))
    (should (with-test-case-return tst (bde-is-member-function-declaration)))))

(ert-deftest test-bde-is-member-function-declaration-basic-override-4 ()
  (let ((tst (make-test-case :input "void foo() override")))
    (should (not (with-test-case-return tst (bde-is-member-function-declaration))))))

(ert-deftest test-bde-is-member-function-declaration-basic-override-5 ()
  (let ((tst (make-test-case :input "void foo() deleted override;")))
    (should (not (with-test-case-return tst (bde-is-member-function-declaration))))))

(ert-deftest test-bde-is-member-function-declaration-basic-override-6 ()
  (let ((tst (make-test-case :input "void foo() default override;")))
    (should (not (with-test-case-return tst (bde-is-member-function-declaration))))))

(ert-deftest test-bde-is-member-function-declaration-basic-bsl-override-1 ()
  (let ((tst (make-test-case :input "void foo() BSLS_CPP11_OVERRIDE;")))
    (should (with-test-case-return tst (bde-is-member-function-declaration)))))

(ert-deftest test-bde-is-member-function-declaration-basic-bsl-override-2 ()
  (let ((tst (make-test-case :input "void foo() BSLS_CPP11_OVERRIDE ;")))
    (should (with-test-case-return tst (bde-is-member-function-declaration)))))

(ert-deftest test-bde-is-member-function-declaration-basic-bsl-override-3 ()
  (let ((tst (make-test-case :input "void foo() BSLS_CPP11_OVERRIDE; ")))
    (should (with-test-case-return tst (bde-is-member-function-declaration)))))

(ert-deftest test-bde-is-member-function-declaration-basic-bsl-override-4 ()
  (let ((tst (make-test-case :input "void foo() BSLS_CPP11_OVERRIDE")))
    (should (not (with-test-case-return tst (bde-is-member-function-declaration))))))

(ert-deftest test-bde-is-member-function-declaration-basic-bsl-override-5 ()
  (let ((tst (make-test-case :input "void foo() BSLS_CPP11_DELETED BSLS_CPP11_OVERRIDE;")))
    (should (not (with-test-case-return tst (bde-is-member-function-declaration))))))

(ert-deftest test-bde-is-member-function-declaration-basic-bsl-override-6 ()
  (let ((tst (make-test-case :input "void foo() BSLS_CPP11_DEFAULT BSLS_CPP11_OVERRIDE;")))
    (should (not (with-test-case-return tst (bde-is-member-function-declaration))))))

(ert-deftest test-bde-is-member-function-declaration-basic-noexcept-1 ()
  (let ((tst (make-test-case :input "void foo() noexcept;")))
    (should (with-test-case-return tst (bde-is-member-function-declaration)))))

(ert-deftest test-bde-is-member-function-declaration-basic-noexcept-2 ()
  (let ((tst (make-test-case :input "void foo() noexcept ;")))
    (should (with-test-case-return tst (bde-is-member-function-declaration)))))

(ert-deftest test-bde-is-member-function-declaration-basic-noexcept-3 ()
  (let ((tst (make-test-case :input "void foo() noexcept; ")))
    (should (with-test-case-return tst (bde-is-member-function-declaration)))))

(ert-deftest test-bde-is-member-function-declaration-basic-noexcept-4 ()
  (let ((tst (make-test-case :input "void foo() noexcept")))
    (should (not (with-test-case-return tst (bde-is-member-function-declaration))))))

(ert-deftest test-bde-is-member-function-declaration-basic-noexcept-5 ()
  (let ((tst (make-test-case :input "void foo() noexcept = delete;")))
    (should (with-test-case-return tst (bde-is-member-function-declaration)))))

(ert-deftest test-bde-is-member-function-declaration-basic-noexcept-6 ()
  (let ((tst (make-test-case :input "void foo() noexcept = default;")))
    (should (with-test-case-return tst (bde-is-member-function-declaration)))))

(ert-deftest test-bde-is-member-function-declaration-basic-bsl-noexcept-1 ()
  (let ((tst (make-test-case :input "void foo() BSLS_CPP11_NOEXCEPT;")))
    (should (with-test-case-return tst (bde-is-member-function-declaration)))))

(ert-deftest test-bde-is-member-function-declaration-basic-bsl-noexcept-2 ()
  (let ((tst (make-test-case :input "void foo() BSLS_CPP11_NOEXCEPT ;")))
    (should (with-test-case-return tst (bde-is-member-function-declaration)))))

(ert-deftest test-bde-is-member-function-declaration-basic-bsl-noexcept-3 ()
  (let ((tst (make-test-case :input "void foo() BSLS_CPP11_NOEXCEPT; ")))
    (should (with-test-case-return tst (bde-is-member-function-declaration)))))

(ert-deftest test-bde-is-member-function-declaration-basic-bsl-noexcept-4 ()
  (let ((tst (make-test-case :input "void foo() BSLS_CPP11_NOEXCEPT")))
    (should (not (with-test-case-return tst (bde-is-member-function-declaration))))))

(ert-deftest test-bde-is-member-function-declaration-basic-bsl-noexcept-5 ()
  (let ((tst (make-test-case :input "void foo() BSLS_CPP11_NOEXCEPT BSLS_CPP11_DELETED;")))
    (should (with-test-case-return tst (bde-is-member-function-declaration)))))

(ert-deftest test-bde-is-member-function-declaration-basic-bsl-noexcept-6 ()
  (let ((tst (make-test-case :input "void foo() BSLS_CPP11_NOEXCEPT BSLS_CPP11_DEFAULT;")))
    (should (with-test-case-return tst (bde-is-member-function-declaration)))))

(ert-deftest test-bde-is-member-function-declaration-c-noexcept-1 ()
  (let ((tst (make-test-case :input "void foo() const noexcept;")))
    (should (with-test-case-return tst (bde-is-member-function-declaration)))))

(ert-deftest test-bde-is-member-function-declaration-c-noexcept-2 ()
  (let ((tst (make-test-case :input "void foo() const noexcept ;")))
    (should (with-test-case-return tst (bde-is-member-function-declaration)))))

(ert-deftest test-bde-is-member-function-declaration-c-noexcept-3 ()
  (let ((tst (make-test-case :input "void foo() const noexcept; ")))
    (should (with-test-case-return tst (bde-is-member-function-declaration)))))

(ert-deftest test-bde-is-member-function-declaration-c-noexcept-4 ()
  (let ((tst (make-test-case :input "void foo() const noexcept")))
    (should (not (with-test-case-return tst (bde-is-member-function-declaration))))))

(ert-deftest test-bde-is-member-function-declaration-c-noexcept-5 ()
  (let ((tst (make-test-case :input "void foo() const noexcept = delete;")))
    (should (with-test-case-return tst (bde-is-member-function-declaration)))))

(ert-deftest test-bde-is-member-function-declaration-c-noexcept-6 ()
  (let ((tst (make-test-case :input "void foo() const noexcept = default;")))
    (should (with-test-case-return tst (bde-is-member-function-declaration)))))

(ert-deftest test-bde-is-member-function-declaration-c-bsl-noexcept-1 ()
  (let ((tst (make-test-case :input "void foo() const BSLS_CPP11_NOEXCEPT;")))
    (should (with-test-case-return tst (bde-is-member-function-declaration)))))

(ert-deftest test-bde-is-member-function-declaration-c-bsl-noexcept-2 ()
  (let ((tst (make-test-case :input "void foo() const BSLS_CPP11_NOEXCEPT ;")))
    (should (with-test-case-return tst (bde-is-member-function-declaration)))))

(ert-deftest test-bde-is-member-function-declaration-c-bsl-noexcept-3 ()
  (let ((tst (make-test-case :input "void foo() const BSLS_CPP11_NOEXCEPT; ")))
    (should (with-test-case-return tst (bde-is-member-function-declaration)))))

(ert-deftest test-bde-is-member-function-declaration-c-bsl-noexcept-4 ()
  (let ((tst (make-test-case :input "void foo() const BSLS_CPP11_NOEXCEPT")))
    (should (not (with-test-case-return tst (bde-is-member-function-declaration))))))

(ert-deftest test-bde-is-member-function-declaration-c-bsl-noexcept-5 ()
  (let ((tst (make-test-case :input "void foo() const BSLS_CPP11_NOEXCEPT BSLS_CPP11_DELETED;")))
    (should (with-test-case-return tst (bde-is-member-function-declaration)))))

(ert-deftest test-bde-is-member-function-declaration-c-bsl-noexcept-6 ()
  (let ((tst (make-test-case :input "void foo() const BSLS_CPP11_NOEXCEPT BSLS_CPP11_DEFAULT;")))
    (should (with-test-case-return tst (bde-is-member-function-declaration)))))

(ert-deftest test-bde-is-member-function-declaration-ref-1 ()
  (let ((tst (make-test-case :input "void foo() &;")))
    (should (with-test-case-return tst (bde-is-member-function-declaration)))))

(ert-deftest test-bde-is-member-function-declaration-ref-2 ()
  (let ((tst (make-test-case :input "void foo() & ;")))
    (should (with-test-case-return tst (bde-is-member-function-declaration)))))

(ert-deftest test-bde-is-member-function-declaration-ref-3 ()
  (let ((tst (make-test-case :input "void foo() &&; ")))
    (should (with-test-case-return tst (bde-is-member-function-declaration)))))

(ert-deftest test-bde-is-member-function-declaration-ref-4 ()
  (let ((tst (make-test-case :input "void foo() && ;")))
    (should (with-test-case-return tst (bde-is-member-function-declaration)))))

(ert-deftest test-bde-is-member-function-declaration-ref-5 ()
  (let ((tst (make-test-case :input "void foo() &")))
    (should (not (with-test-case-return tst (bde-is-member-function-declaration))))))

(ert-deftest test-bde-is-member-function-declaration-ref-6 ()
  (let ((tst (make-test-case :input "void foo() &&")))
    (should (not (with-test-case-return tst (bde-is-member-function-declaration))))))

(ert-deftest test-bde-is-member-function-declaration-ref-7 ()
  (let ((tst (make-test-case :input "void foo() &&&;")))
    (should (not (with-test-case-return tst (bde-is-member-function-declaration))))))

;; Local Variables:
;; no-byte-compile: t
;; End:
