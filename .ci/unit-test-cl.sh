#!/bin/bash

set -x
set -e
EMACS_DIR="$(cd "${GITHUB_WORKSPACE:-~}"/"${1:-.emacs.d}"; pwd -P)/"
EMACS=${EMACS:=emacs}

${EMACS} -Q --batch \
         --eval '
(progn
  (setq user-emacs-directory "'"${EMACS_DIR}"'")
  (load-file "'"${EMACS_DIR}"'/init.el")
  (setq debug-on-error t)
  (load-file "'"${EMACS_DIR}"'/modules/init-util-cl.t.el")
  (let ((print-level 50)
        (eval-expression-print-level 50)
        (eval-expression-print-length 1000)
        (edebug-print-level 50)
        (edebug-print-length 1000)
        (ert-batch-print-level 50)
        (ert-batch-print-length 1000)
        (ert-batch-backtrace-line-length 1000)
        (ert-batch-backtrace-right-margin 1000))
    (ert-run-tests-batch-and-exit)))'
