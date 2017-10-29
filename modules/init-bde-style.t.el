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


;; Tests for `bde-align-fundecl'

(ert-deftest test-bde-align-fundecl-1 ()
  (let ((tst (make-test-case :input "struct A {
    void foo(std::vector<int>& a = {1, 2, 3},
             std::map<int, int> &bb = std::vector(2,2),
             std::vector<int>* c = std::vector{1, 2, 3},
             std::map<int, int> *d = std::map<int, int>{});
};"
                             :output "struct A {
    void foo(std::vector<int>&    a  = {1, 2, 3},
             std::map<int, int>&  bb = std::vector(2,2),
             std::vector<int>    *c  = std::vector{1, 2, 3},
             std::map<int, int>  *d  = std::map<int, int>{});
};"
                             :cursor-pos (1+ (length "struct A {")))))
    (should (string= (with-test-case-output tst (bde-align-fundecl))
                     (test-case-output tst)))))

(ert-deftest test-bde-align-fundecl-2 ()
  (let ((tst (make-test-case :input "struct A {
    void foo(std::vector<int>& a = {1, 2, 3},
             std::map<int, int> &b = std::vector(2,2),
             std::vector<int>* c = std::vector{1, 2, 3},
             std::map<int, int> **dd = std::map<int, int>{});
};"
                             :output "struct A {
    void foo(std::vector<int>&     a  = {1, 2, 3},
             std::map<int, int>&   b  = std::vector(2,2),
             std::vector<int>     *c  = std::vector{1, 2, 3},
             std::map<int, int>  **dd = std::map<int, int>{});
};"
                             :cursor-pos (+ 11 (length "struct A {")))))
    (should (string= (with-test-case-output tst (bde-align-fundecl))
                     (test-case-output tst)))))

(ert-deftest test-bde-align-fundecl-3 ()
  (let ((tst (make-test-case :input "struct A {
    void foo(std::vector<int>& aa = {1, 2, 3},
             std::map<int, int> &b = std::vector(2,2),
             std::vector<int>* c = std::vector{1, 2, 3},
             std::map<int, int> *d = std::map<int, int>{});
};"
                             :output "struct A {
    void foo(std::vector<int>&    aa = {1, 2, 3},
             std::map<int, int>&  b  = std::vector(2,2),
             std::vector<int>    *c  = std::vector{1, 2, 3},
             std::map<int, int>  *d  = std::map<int, int>{});
};"
                             :cursor-pos (+ 200 (length "struct A {")))))
    (should (string= (with-test-case-output tst (bde-align-fundecl))
                     (test-case-output tst)))))

(ert-deftest test-bde-align-fundecl-long-1 ()
  (let ((tst (make-test-case :input "struct A {
    void foo_is_a_long_name_so_parameters_will_not_fit(std::vector<int>& a = {1, 2, 3},
             std::map<int, int> &bb = std::vector(2,2),
             std::vector<int>* c = std::vector{1, 2, 3},
             std::map<int, int> *d = std::map<int, int>{});
};"
                             :output "struct A {
    void foo_is_a_long_name_so_parameters_will_not_fit(
                               std::vector<int>&    a  = {1, 2, 3},
                               std::map<int, int>&  bb = std::vector(2,2),
                               std::vector<int>    *c  = std::vector{1, 2, 3},
                               std::map<int, int>  *d  = std::map<int, int>{});
};"
                             :cursor-pos (1+ (length "struct A {")))))
    (should (string= (with-test-case-output tst (bde-align-fundecl))
                     (test-case-output tst)))))

(ert-deftest test-bde-align-fundecl-long-2 ()
  (let ((tst (make-test-case :input "struct A {
    void foo_is_a_long_name_so_parameters_will_not_fit(
std::vector<int>& a = {1, 2, 3},
std::map<int, int> &b = std::vector(2,2),
std::vector<int>* c = std::vector{1, 2, 3},
std::map<int, int> **dd = std::map<int, int>{});
};"
                             :output "struct A {
    void foo_is_a_long_name_so_parameters_will_not_fit(
                              std::vector<int>&     a  = {1, 2, 3},
                              std::map<int, int>&   b  = std::vector(2,2),
                              std::vector<int>     *c  = std::vector{1, 2, 3},
                              std::map<int, int>  **dd = std::map<int, int>{});
};"
                             :cursor-pos (+ 80 (length "struct A {")))))
    (should (string= (with-test-case-output tst (bde-align-fundecl))
                     (test-case-output tst)))))

(ert-deftest test-bde-align-fundecl-long-3 ()
  (let ((tst (make-test-case :input "struct A {
    void foo_is_a_long_name_so_parameters_will_not_fit(std::vector<int>& aa = {1, 2, 3},
                                                       std::map<int, int> &b = std::vector(2,2),
                                                       std::vector<int>* c = std::vector{1, 2, 3},
                                                       std::map<int, int> *d = std::map<int, int>{});
};"
                             :output "struct A {
    void foo_is_a_long_name_so_parameters_will_not_fit(
                               std::vector<int>&    aa = {1, 2, 3},
                               std::map<int, int>&  b  = std::vector(2,2),
                               std::vector<int>    *c  = std::vector{1, 2, 3},
                               std::map<int, int>  *d  = std::map<int, int>{});
};"
                             :cursor-pos (+ 200 (length "struct A {")))))
    (should (string= (with-test-case-output tst (bde-align-fundecl))
                     (test-case-output tst)))))



;; Tests for `bde-guess-class-name'

(ert-deftest test-bde-guess-class-name-class-1 ()
  (let ((tst (make-test-case :input "class TheName {")))
    (should (string= (with-test-case-return tst (bde-guess-class-name))
                     "class TheName"))))

(ert-deftest test-bde-guess-class-name-class-2 ()
  (let ((tst (make-test-case :input "class TheName : public TheInterface {")))
    (should (string= (with-test-casre-return tst (bde-guess-class-name))
                     "class TheName"))))

(ert-deftest test-bde-guess-class-name-struct-1 ()
  (let ((tst (make-test-case :input "struct TheName {")))
    (should (string= (with-test-case-return tst (bde-guess-class-name))
                     "struct TheName"))))

(ert-deftest test-bde-guess-class-name-class-2 ()
  (let ((tst (make-test-case :input "struct TheName : OtherStruct {")))
    (should (string= (with-test-case-return tst (bde-guess-class-name))
                     "struct TheName"))))

(ert-deftest test-bde-guess-class-name-not-class-1 ()
  (let ((tst (make-test-case :input "enum TheName {")))
    (should (not (with-test-case-return tst (bde-guess-class-name))))))

(ert-deftest test-bde-guess-class-name-template-1 ()
  (let ((tst (make-test-case :input "template<typename A>
class TheName {")))
    (should (string= (with-test-case-return tst (bde-guess-class-name))
                     "class TheName"))))

(ert-deftest test-bde-guess-class-name-template-2 ()
  (let ((tst (make-test-case :input "template <typename A>
class TheName {")))
    (should (string= (with-test-case-return tst (bde-guess-class-name))
                     "class TheName"))))

(ert-deftest test-bde-guess-class-name-template-3 ()
  (let ((tst (make-test-case :input "template <>
class TheName<int> {")))
    (should (string= (with-test-case-return tst (bde-guess-class-name))
                     "class TheName"))))

(ert-deftest test-bde-guess-class-name-template-4 ()
  (let ((tst (make-test-case :input "template <class A>
class TheName<int> {")))
    (should (string= (with-test-case-return tst (bde-guess-class-name))
                     "class TheName"))))

(ert-deftest test-bde-guess-class-name-template-5 ()
  (let ((tst (make-test-case :input "template <int A>
class TheName<int> {")))
    (should (string= (with-test-case-return tst (bde-guess-class-name))
                     "class TheName"))))

(ert-deftest test-bde-guess-class-name-template-6 ()
  (let ((tst (make-test-case :input "template <int A
class A>
class TheName<int> {")))
    (should (string= (with-test-case-return tst (bde-guess-class-name))
                     "class TheName"))))

(ert-deftest test-bde-guess-class-name-template-7 ()
  (let ((tst (make-test-case :input "template <int A,
         class A>
class TheName<int> {")))
    (should (string= (with-test-case-return tst (bde-guess-class-name))
                     "class TheName"))))

(ert-deftest test-bde-guess-class-name-template-8 ()
  (let ((tst (make-test-case :input "template <typename <class> A>
class TheName<int> {")))
    (should (string= (with-test-case-return tst (bde-guess-class-name))
                     "class TheName"))))

(ert-deftest test-bde-guess-class-name-template-9 ()
  (let ((tst (make-test-case :input "template <typename std::enable_if<std::is_class<T>{}, int>::type = 0>
class TheName {")))
    (should (string= (with-test-case-return tst (bde-guess-class-name))
                     "class TheName"))))

(ert-deftest test-bde-guess-class-name-template-10 ()
  (let ((tst (make-test-case :input "template <typename <class> A
class TheName<int> {")))
    (should (not (with-test-case-return tst (bde-guess-class-name))))))

(ert-deftest test-bde-guess-class-name-template-11 ()
  (let ((tst (make-test-case :input "template <typename A>>
class TheName {")))
    (should (string= (with-test-case-return tst (bde-guess-class-name))
            "class TheName"))))


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


;; Tests for `bde-parse-argument'

(ert-deftest test-bde-parse-argument-simple ()
  (should (equal (bde-parse-argument "int a") '("int" "a" 0 nil))))

(ert-deftest test-bde-parse-argument-simple-ptr ()
  (should (equal (bde-parse-argument "int *a") '("int" "*a" 1 nil))))

(ert-deftest test-bde-parse-argument-simple-ptr-fix ()
  (should (equal (bde-parse-argument "int* a") '("int" "*a" 1 nil))))

(ert-deftest test-bde-parse-argument-simple-ptr-ptr ()
  (should (equal (bde-parse-argument "int **a") '("int" "**a" 2 nil))))

(ert-deftest test-bde-parse-argument-simple-ptr-ptr-fix ()
  (should (equal (bde-parse-argument "int** a") '("int" "**a" 2 nil))))

(ert-deftest test-bde-parse-argument-simple-ref ()
  (should (equal (bde-parse-argument "int& a") '("int&" "a" 0 nil))))

(ert-deftest test-bde-parse-argument-simple-ref-fix ()
  (should (equal (bde-parse-argument "int &a") '("int&" "a" 0 nil))))

(ert-deftest test-bde-parse-argument-simple-ref-ref ()
  (should (equal (bde-parse-argument "int&& a") '("int&&" "a" 0 nil))))

(ert-deftest test-bde-parse-argument-simple-ref-ref-fix ()
  (should (equal (bde-parse-argument "int &&a") '("int&&" "a" 0 nil))))

(ert-deftest test-bde-parse-argument-assign-simple ()
  (should (equal (bde-parse-argument "int a = 1") '("int" "a" 0 "= 1"))))

(ert-deftest test-bde-parse-argument-assign-function ()
  (should (equal (bde-parse-argument "int a = foo(1, 2)")
                 '("int" "a" 0 "= foo(1, 2)"))))

(ert-deftest test-bde-parse-argument-assign-with-eq ()
  (should (equal (bde-parse-argument "Type<sizeof(int) == 4> a = foo(1, 2)")
                 '("Type<sizeof(int) == 4>" "a" 0 "= foo(1, 2)"))))

(ert-deftest test-bde-parse-argument-assign-with-gteq ()
  (should (equal (bde-parse-argument "Type<sizeof(int) >= 4> a = foo(1, 2)")
                 '("Type<sizeof(int) >= 4>" "a" 0 "= foo(1, 2)"))))

(ert-deftest test-bde-parse-argument-assign-with-lteq ()
  (should (equal (bde-parse-argument "Type<sizeof(int) <= 4> a = foo(1, 2)")
                 '("Type<sizeof(int) <= 4>" "a" 0 "= foo(1, 2)"))))

(ert-deftest test-bde-parse-argument-crazy-name1 ()
  (should (equal (bde-parse-argument "int _A_cr4zyNam3")
                 '("int" "_A_cr4zyNam3" 0 nil))))

(ert-deftest test-bde-parse-argument-crazy-name2 ()
  (should (equal (bde-parse-argument "int lessCr4zyNam3")
                 '("int" "lessCr4zyNam3" 0 nil))))

(ert-deftest test-bde-parse-argument-multi-ref-fix ()
  (should (equal (bde-parse-argument "std::vector<int &, int &&> &&a")
                 '("std::vector<int&, int&&>&&" "a" 0 nil))))

(ert-deftest test-bde-parse-argument-variadic ()
  (should (equal (bde-parse-argument "T... a") '("T..." "a" 0 nil))))

(ert-deftest test-bde-parse-argument-variadic-ref ()
  (should (equal (bde-parse-argument "T&&... a") '("T&&..." "a" 0 nil))))

(ert-deftest test-bde-parse-argument-variadic-ref-fix ()
  (should (equal (bde-parse-argument "T &&... a") '("T&&..." "a" 0 nil))))



;; Tests for `exordium-bde-split-arglist'

(ert-deftest test-exordium-bde-split-arglist-nil ()
  (should (equal (exordium-bde-split-arglist nil) nil)))

(ert-deftest test-exordium-bde-split-arglist-empty-string ()
  (should (equal (exordium-bde-split-arglist "") nil)))

(ert-deftest test-exordium-bde-split-arglist-empty ()
  (should (equal (exordium-bde-split-arglist "()") nil)))

(ert-deftest test-exordium-bde-split-arglist-single ()
  (should (equal (exordium-bde-split-arglist "(int a)")
                 '("int a"))))

(ert-deftest test-exordium-bde-split-arglist-two ()
  (should (equal (exordium-bde-split-arglist "(int a, int b)")
                 '("int a" "int b"))))

(ert-deftest test-exordium-bde-split-arglist-two-multiline ()
  (should (equal (exordium-bde-split-arglist "(int a,\n    int b)")
                 '("int a" "int b"))))

(ert-deftest test-exordium-bde-split-arglist-commas-in-type ()
  (should (equal (exordium-bde-split-arglist
                  (concat "(std::map<int, int> a,\n"
                          "std::conditional<X, A&, B&>::type b)"))
                 '("std::map<int, int> a"
                   "std::conditional<X, A&, B&>::type b"))))

(ert-deftest test-exordium-bde-split-arglist-commas-in-assign ()
  (should (equal (exordium-bde-split-arglist
                  (concat "(std::vector<int> a = {1, 2, 3},\n"
                          "std::vector<int> b = std::vector(2,2),\n"
                          "std::vector<int> c = std::vector{1, 2, 3},\n"
                          "std::map<int, int> d = std::map<int, int>{})"))
                 '("std::vector<int> a = {1, 2, 3}"
                   "std::vector<int> b = std::vector(2,2)"
                   "std::vector<int> c = std::vector{1, 2, 3}"
                   "std::map<int, int> d = std::map<int, int>{}"))))

(ert-deftest test-exordium-bde-split-arglist-crazy-arglist ()
  (should (equal (exordium-bde-split-arglist
                  (concat "(std::vector<int> bar = fun<sizeof(int)>(),\n"
                          "                  std::vector<int> = {1,2, 3},\n"
                          "                  std::map<int & , int*> &&baZ2 = {},\n"
                          "            std::vector<std::map<int, char>*> *qux = the_qux(1, 2, 3),\n"
                          "            int (*(*xs)(int*, int &, std::vector<int&>&&))[3],\n"
                          "            TYPE<sizeof(typename TYPE::value) == 0> * Qu2x = new TYPE{nullptr},\n"
                          "            T&&... ts)"))
                  '("std::vector<int> bar = fun<sizeof(int)>()"
                    "std::vector<int> = {1,2, 3}"
                    "std::map<int & , int*> &&baZ2 = {}"
                    "std::vector<std::map<int, char>*> *qux = the_qux(1, 2, 3)"
                    "int (*(*xs)(int*, int &, std::vector<int&>&&))[3]"
                    "TYPE<sizeof(typename TYPE::value) == 0> * Qu2x = new TYPE{nullptr}"
                    "T&&... ts"))))

;; Tests for `exordium-bde-bounds-of-arglist-at-point'

(ert-deftest test-exordium-bde-bounds-of-arglist-at-point-simple-1 ()
  (let ((tst (make-test-case :input "struct A {
    void foo(bool b);
};"
                             :cursor-pos (1+ (length "struct A {")))))
    (should (equal (with-test-case-return tst (exordium-bde-bounds-of-arglist-at-point))
                   (cons 24 32)))))

(ert-deftest test-exordium-bde-bounds-of-arglist-at-point-simple-2 ()
  (let ((tst (make-test-case :input "struct A {
    void foo(bool b);
};"
                             :cursor-pos (+ 11 (length "struct A {")))))
    (should (equal (with-test-case-return tst (exordium-bde-bounds-of-arglist-at-point))
                   (cons 24 32)))))

(ert-deftest test-exordium-bde-bounds-of-arglist-at-point-simple-3 ()
  (let ((tst (make-test-case :input "struct A {
    void foo(bool b);
};"
                             :cursor-pos (+ 15 (length "struct A {")))))
    (should (equal (with-test-case-return tst (exordium-bde-bounds-of-arglist-at-point))
                   (cons 24 32)))))

(ert-deftest test-exordium-bde-bounds-of-arglist-at-point-multi-arg-1 ()
  (let ((tst (make-test-case :input "struct A {
    void foo(bool b, int c);
};"
                             :cursor-pos (1+ (length "struct A {")))))
    (should (equal (with-test-case-return tst (exordium-bde-bounds-of-arglist-at-point))
                   (cons 24 39)))))

(ert-deftest test-exordium-bde-bounds-of-arglist-at-point-multi-arg-2 ()
  (let ((tst (make-test-case :input "struct A {
    void foo(bool b, int c);
};"
                             :cursor-pos (+ 11 (length "struct A {")))))
    (should (equal (with-test-case-return tst (exordium-bde-bounds-of-arglist-at-point))
                   (cons 24 39)))))

(ert-deftest test-exordium-bde-bounds-of-arglist-at-point-multi-arg-3 ()
  (let ((tst (make-test-case :input "struct A {
    void foo(bool b, int c);
};"
                             :cursor-pos (+ 15 (length "struct A {")))))
    (should (equal (with-test-case-return tst (exordium-bde-bounds-of-arglist-at-point))
                   (cons 24 39)))))

(ert-deftest test-exordium-bde-bounds-of-arglist-at-point-multi-arg-4 ()
  (let ((tst (make-test-case :input "struct A {
    void foo(bool b, int c);
};"
                             :cursor-pos (+ 22 (length "struct A {")))))
    (should (equal (with-test-case-return tst (exordium-bde-bounds-of-arglist-at-point))
                   (cons 24 39)))))

(ert-deftest test-exordium-bde-bounds-of-arglist-at-point-multi-line-1 ()
  (let ((tst (make-test-case :input "struct A {
    void foo(bool b,
        int c);
};"
                             :cursor-pos (1+ (length "struct A {")))))
    (should (equal (with-test-case-return tst (exordium-bde-bounds-of-arglist-at-point))
                   (cons 24 47)))))

(ert-deftest test-exordium-bde-bounds-of-arglist-at-point-multi-line-2 ()
  (let ((tst (make-test-case :input "struct A {
    void foo(bool b,
        int c);
};"
                             :cursor-pos (+ 11 (length "struct A {")))))
    (should (equal (with-test-case-return tst (exordium-bde-bounds-of-arglist-at-point))
                   (cons 24 47)))))

(ert-deftest test-exordium-bde-bounds-of-arglist-at-point-multi-line-3 ()
  (let ((tst (make-test-case :input "struct A {
    void foo(bool b,
        int c);
};"
                             :cursor-pos (+ 25 (length "struct A {")))))
    (should (equal (with-test-case-return tst (exordium-bde-bounds-of-arglist-at-point))
                   (cons 24 47)))))

(ert-deftest test-exordium-bde-bounds-of-arglist-at-point-multi-function-1 ()
  (let ((tst (make-test-case :input "struct A {
    void foo(bool b,
        int c);

    template <typename T>
    void bar(T t,
        int c);
};"
                             :cursor-pos (1+ (length "struct A {
    void foo(bo")))))
    (should (equal (with-test-case-return tst (exordium-bde-bounds-of-arglist-at-point))
                   (cons 24 47)))))

(ert-deftest test-exordium-bde-bounds-of-arglist-at-point-multi-function-2 ()
  (let ((tst (make-test-case :input "struct A {
    void foo(bool b,
        int c);

    template <typename T>
    void bar(T t,
        int c);
};"
                             :cursor-pos (1+ (length "struct A {
    void foo(bool b,
        int")))))
    (should (equal (with-test-case-return tst (exordium-bde-bounds-of-arglist-at-point))
                   (cons 24 47)))))

(ert-deftest test-exordium-bde-bounds-of-arglist-at-point-multi-function-3 ()
  (let ((tst (make-test-case :input "struct A {
    void foo(bool b,
        int c);

    template <typename T>
    void bar(T t,
        int c);
};"
                             :cursor-pos (1+ (length "struct A {
    void foo(bool b,
        int c);

    templ")))))
    (should (equal (with-test-case-return tst (exordium-bde-bounds-of-arglist-at-point))
                   (cons 88 108)))))

(ert-deftest test-exordium-bde-bounds-of-arglist-at-point-multi-function-4 ()
  (let ((tst (make-test-case :input "struct A {
    void foo(bool b,
        int c);

    template <typename T>
    void bar(T t,
        int c);
};"
                             :cursor-pos (1+ (length "struct A {
    void foo(bool b,
        int c);

    template <typename T>
    void b")))))
    (should (equal (with-test-case-return tst (exordium-bde-bounds-of-arglist-at-point))
                   (cons 88 108)))))

(ert-deftest test-exordium-bde-bounds-of-arglist-at-point-multi-function-5 ()
  (let ((tst (make-test-case :input "struct A {
    void foo(bool b,
        int c);

    template <typename T>
    void bar(T t,
        int c);
};"
                             :cursor-pos (1+ (length "struct A {
    void foo(bool b,
        int c);

    template <typename T>
    void bar(T")))))
    (should (equal (with-test-case-return tst (exordium-bde-bounds-of-arglist-at-point))
                   (cons 88 108)))))

(ert-deftest test-exordium-bde-bounds-of-arglist-at-point-multi-function-6 ()
  (let ((tst (make-test-case :input "struct A {
    void foo(bool b,
        int c);

    template <typename T>
    void bar(T t,
        int c);
};"
                             :cursor-pos (1+ (length "struct A {
    void foo(bool b,
        int c);

    template <typename T>
    void bar(T t,
        in")))))
    (should (equal (with-test-case-return tst (exordium-bde-bounds-of-arglist-at-point))
                   (cons 88 108)))))

(ert-deftest test-exordium-bde-bounds-of-arglist-at-point-inline-1 ()
  (let ((tst (make-test-case :input "struct A {
    void foo(bool b, int c) {}
};"
                             :cursor-pos (1+ (length "struct A {")))))
    (should (equal (with-test-case-return tst (exordium-bde-bounds-of-arglist-at-point))
                   (cons 24 39)))))

(ert-deftest test-exordium-bde-bounds-of-arglist-at-point-inline-2 ()
  (let ((tst (make-test-case :input "struct A {
    void foo(bool b, int c) {}
};"
                             :cursor-pos (+ 22 (length "struct A {")))))
    (should (equal (with-test-case-return tst (exordium-bde-bounds-of-arglist-at-point))
                   (cons 24 39)))))

(ert-deftest test-exordium-bde-bounds-of-arglist-at-point-noexcept-1 ()
  (let ((tst (make-test-case :input "struct A {
    void foo(bool b, int c) noexcept(noexcept(x));
};"
                             :cursor-pos (1+ (length "struct A {")))))
    (should (equal (with-test-case-return tst (exordium-bde-bounds-of-arglist-at-point))
                   (cons 24 39)))))

(ert-deftest test-exordium-bde-bounds-of-arglist-at-point-noexcept-2 ()
  (let ((tst (make-test-case :input "struct A {
    void foo(bool b, int c) noexcept(noexcept(x));
};"
                             :cursor-pos (+ 11 (length "struct A {")))))
    (should (equal (with-test-case-return tst (exordium-bde-bounds-of-arglist-at-point))
                   (cons 24 39)))))

(ert-deftest test-exordium-bde-bounds-of-arglist-at-point-noexcept-3 ()
  (let ((tst (make-test-case :input "struct A {
    void foo(bool b, int c) noexcept(noexcept(x));
};"
                             :cursor-pos (+ 30 (length "struct A {")))))
    (should (equal (with-test-case-return tst (exordium-bde-bounds-of-arglist-at-point))
                   (cons 24 39)))))


(ert-deftest test-exordium-bde-bounds-of-arglist-at-point-cont-conditions-1 ()
  (let ((tst (make-test-case :input "struct A {
    int foo(
        std::string *bar,
        std::string *baz)
     noexcept;
};"
                             :cursor-pos (1+ (length "struct A {")))))
    (should (equal (with-test-case-return tst (exordium-bde-bounds-of-arglist-at-point))
                   (cons 23 76)))))

(ert-deftest test-exordium-bde-bounds-of-arglist-at-point-cont-conditions-2 ()
  (let ((tst (make-test-case :input "struct A {
    int foo(
        std::string *bar,
        std::string *baz)
     noexcept;
};"
                             :cursor-pos (1+ (length "struct A {
    int fo")))))
    (should (equal (with-test-case-return tst (exordium-bde-bounds-of-arglist-at-point))
                   (cons 23 76)))))

(ert-deftest test-exordium-bde-bounds-of-arglist-at-point-cont-conditions-3 ()
  (let ((tst (make-test-case :input "struct A {
    int foo(
        std::string *bar,
        std::string *baz)
     noexcept;
};"
                             :cursor-pos (1+ (length "struct A {
    int foo(
        std::str")))))
    (should (equal (with-test-case-return tst (exordium-bde-bounds-of-arglist-at-point))
                   (cons 23 76)))))

(ert-deftest test-exordium-bde-bounds-of-arglist-at-point-cont-conditions-4 ()
  (let ((tst (make-test-case :input "struct A {
    int foo(
        std::string *bar,
        std::string *baz)
     noexcept;
};"
                             :cursor-pos (1+ (length "struct A {
    int foo(
        std::string *b")))))
    (should (equal (with-test-case-return tst (exordium-bde-bounds-of-arglist-at-point))
                   (cons 23 76)))))

(ert-deftest test-exordium-bde-bounds-of-arglist-at-point-cont-conditions-5 ()
  (let ((tst (make-test-case :input "struct A {
    int foo(
        std::string *bar,
        std::string *baz)
     noexcept;
};"
                             :cursor-pos (1+ (length "struct A {
    int foo(
        std::string *bar,
        std::str")))))
    (should (equal (with-test-case-return tst (exordium-bde-bounds-of-arglist-at-point))
                   (cons 23 76)))))

(ert-deftest test-exordium-bde-bounds-of-arglist-at-point-cont-conditions-6 ()
  (let ((tst (make-test-case :input "struct A {
    int foo(
        std::string *bar,
        std::string *baz)
     noexcept;
};"
                             :cursor-pos (1+ (length "struct A {
    int foo(
        std::string *bar,
        std::string *b")))))
    (should (equal (with-test-case-return tst (exordium-bde-bounds-of-arglist-at-point))
                   (cons 23 76)))))

(ert-deftest test-exordium-bde-bounds-of-arglist-at-point-cont-conditions-7 ()
  (let ((tst (make-test-case :input "struct A {
    int foo(
        std::string *bar,
        std::string *baz)
     noexcept;
};"
                             :cursor-pos (1+ (length "struct A {
    int foo(
        std::string *bar,
        std::string *baz)
     noex")))))
    (message (format "tst=%s" tst))
    (should (equal (with-test-case-return tst (exordium-bde-bounds-of-arglist-at-point))
                   (cons 23 76)))))

(ert-deftest test-exordium-bde-bounds-of-arglist-at-point-commas-in-type-1 ()
  (let ((tst (make-test-case :input "struct A {
    void foo(std::map<int, int> a,
             std::conditional<X, A&, B&>::type b);"
                             :cursor-pos (1+ (length "struct A {")))))
    (should (equal (with-test-case-return tst (exordium-bde-bounds-of-arglist-at-point))
                   (cons 24 96)))))

(ert-deftest test-exordium-bde-bounds-of-arglist-at-point-commas-in-type-2 ()
  (let ((tst (make-test-case :input "struct A {
    void foo(std::map<int, int> a,
             std::conditional<X, A&, B&>::type b);"
                             :cursor-pos (+ 11 (length "struct A {")))))
    (should (equal (with-test-case-return tst (exordium-bde-bounds-of-arglist-at-point))
                   (cons 24 96)))))

(ert-deftest test-exordium-bde-bounds-of-arglist-at-point-commas-in-type-3 ()
  (let ((tst (make-test-case :input "struct A {
    void foo(std::map<int, int> a,
             std::conditional<X, A&, B&>::type b);"
                             :cursor-pos (+ 70 (length "struct A {")))))
    (should (equal (with-test-case-return tst (exordium-bde-bounds-of-arglist-at-point))
                   (cons 24 96)))))

(ert-deftest test-exordium-bde-bounds-of-arglist-at-point-commas-in-assign-1 ()
  (let ((tst (make-test-case :input "struct A {
    void foo(std::vector<int> a = {1, 2, 3},
             std::vector<int> b = std::vector(2,2),
             std::vector<int> c = std::vector{1, 2, 3},
             std::map<int, int> d = std::map<int, int>{})
{}"
                             :cursor-pos (1+ (length "struct A {")))))
    (should (equal (with-test-case-return tst (exordium-bde-bounds-of-arglist-at-point))
                   (cons 24 222)))))

(ert-deftest test-exordium-bde-bounds-of-arglist-at-point-commas-in-assign-2 ()
  (let ((tst (make-test-case :input "struct A {
    void foo(std::vector<int> a = {1, 2, 3},
             std::vector<int> b = std::vector(2,2),
             std::vector<int> c = std::vector{1, 2, 3},
             std::map<int, int> d = std::map<int, int>{})
{}"
                             :cursor-pos (+ 11 (length "struct A {")))))
    (should (equal (with-test-case-return tst (exordium-bde-bounds-of-arglist-at-point))
                   (cons 24 222)))))

(ert-deftest test-exordium-bde-bounds-of-arglist-at-point-commas-in-assign-3 ()
  (let ((tst (make-test-case :input "struct A {
    void foo(std::vector<int> a = {1, 2, 3},
             std::vector<int> b = std::vector(2,2),
             std::vector<int> c = std::vector{1, 2, 3},
             std::map<int, int> d = std::map<int, int>{})
{}"
                             :cursor-pos (+ 65 (length "struct A {")))))
    (should (equal (with-test-case-return tst (exordium-bde-bounds-of-arglist-at-point))
                   (cons 24 222)))))

(ert-deftest test-exordium-bde-bounds-of-arglist-at-point-commas-in-assign-4 ()
  (let ((tst (make-test-case :input "struct A {
    void foo(std::vector<int> a = {1, 2, 3},
             std::vector<int> b = std::vector(2,2),
             std::vector<int> c = std::vector{1, 2, 3},
             std::map<int, int> d = std::map<int, int>{})
{}"
                             :cursor-pos (+ 103 (length "struct A {")))))
    (should (equal (with-test-case-return tst (exordium-bde-bounds-of-arglist-at-point))
                   (cons 24 222)))))

(ert-deftest test-exordium-bde-bounds-of-arglist-at-point-crazy-arglist-1 ()
  (let ((tst (make-test-case :input "struct A {
    void foo(std::vector<int> bar = fun<sizeof(int)>(),
      std::vector<int> = {1,2, 3},
                  std::map<int & , int*> &&baZ2 = {},
            std::vector<std::map<int, char>*> *qux = the_qux(1, 2, 3),
       int (*(*xs)(int*, int &, std::vector<int&>&&))[3],
            TYPE<sizeof(typename TYPE::value) == 0> * Qu2x = new TYPE{nullptr},
            T&&... ts) = delete;"
                             :cursor-pos (1+ (length "struct A {")))))
    (should (equal (with-test-case-return tst (exordium-bde-bounds-of-arglist-at-point))
                   (cons 24 388)))))

(ert-deftest test-exordium-bde-bounds-of-arglist-at-point-crazy-arglist-2 ()
  (let ((tst (make-test-case :input "struct A {
    void foo(std::vector<int> bar = fun<sizeof(int)>(),
      std::vector<int> = {1,2, 3},
                  std::map<int & , int*> &&baZ2 = {},
            std::vector<std::map<int, char>*> *qux = the_qux(1, 2, 3),
       int (*(*xs)(int*, int &, std::vector<int&>&&))[3],
            TYPE<sizeof(typename TYPE::value) == 0> * Qu2x = new TYPE{nullptr},
            T&&... ts) = delete;"
                             :cursor-pos (+ 10 (length "struct A {")))))
    (should (equal (with-test-case-return tst (exordium-bde-bounds-of-arglist-at-point))
                   (cons 24 388)))))

(ert-deftest test-exordium-bde-bounds-of-arglist-at-point-crazy-arglist-3 ()
  (let ((tst (make-test-case :input "struct A {
    void foo(std::vector<int> bar = fun<sizeof(int)>(),
      std::vector<int> = {1,2, 3},
                  std::map<int & , int*> &&baZ2 = {},
            std::vector<std::map<int, char>*> *qux = the_qux(1, 2, 3),
       int (*(*xs)(int*, int &, std::vector<int&>&&))[3],
            TYPE<sizeof(typename TYPE::value) == 0> * Qu2x = new TYPE{nullptr},
            T&&... ts) = delete;"
                             :cursor-pos (+ 50 (length "struct A {")))))
    (should (equal (with-test-case-return tst (exordium-bde-bounds-of-arglist-at-point))
                   (cons 24 388)))))

(ert-deftest test-exordium-bde-bounds-of-arglist-at-point-crazy-arglist-4 ()
  (let ((tst (make-test-case :input "struct A {
    void foo(std::vector<int> bar = fun<sizeof(int)>(),
      std::vector<int> = {1,2, 3},
                  std::map<int & , int*> &&baZ2 = {},
            std::vector<std::map<int, char>*> *qux = the_qux(1, 2, 3),
       int (*(*xs)(int*, int &, std::vector<int&>&&))[3],
            TYPE<sizeof(typename TYPE::value) == 0> * Qu2x = new TYPE{nullptr},
            T&&... ts) = delete;"
                             :cursor-pos (+ 100 (length "struct A {")))))
    (should (equal (with-test-case-return tst (exordium-bde-bounds-of-arglist-at-point))
                   (cons 24 388)))))

(ert-deftest test-exordium-bde-bounds-of-arglist-at-point-crazy-arglist-5 ()
  (let ((tst (make-test-case :input "struct A {
    void foo(std::vector<int> bar = fun<sizeof(int)>(),
      std::vector<int> = {1,2, 3},
                  std::map<int & , int*> &&baZ2 = {},
            std::vector<std::map<int, char>*> *qux = the_qux(1, 2, 3),
       int (*(*xs)(int*, int &, std::vector<int&>&&))[3],
            TYPE<sizeof(typename TYPE::value) == 0> * Qu2x = new TYPE{nullptr},
            T&&... ts) = delete;"
                             :cursor-pos (+ 200 (length "struct A {")))))
    (should (equal (with-test-case-return tst (exordium-bde-bounds-of-arglist-at-point))
                   (cons 24 388)))))

(ert-deftest test-exordium-bde-bounds-of-arglist-at-point-too-long-function ()
  (let ((tst (make-test-case :input (concat "struct A {
    void f"

                                            (make-string (* 80 25) ?o)
                                            "(bool b,
        int c);
};")
                             :cursor-pos (1+ (length "struct A {
    void f")))))
    (should-not (with-test-case-return tst (exordium-bde-bounds-of-arglist-at-point)))))

(ert-deftest test-exordium-bde-bounds-of-arglist-at-point-too-long-args ()
  (let ((tst (make-test-case :input (concat "struct A {
    void foo(b"
                                            (make-string (* 80 25) ?o)
                                            "l b,
        int c);
};")
                             :cursor-pos (1+ (length "struct A {
    void foo(b")))))
    (should-not (with-test-case-return tst (exordium-bde-bounds-of-arglist-at-point)))))

;; Local Variables:
;; no-byte-compile: t
;; End:
