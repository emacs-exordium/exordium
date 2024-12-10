#!/usr/bin/env bash

set -x
set -e

EMACS_DIR="$(cd "${HOME}/${1:-.emacs.d}" && pwd -P)"
EMACS=${EMACS:=emacs}

# Run all tests form *.t.el
${EMACS} -Q --batch \
         --eval '
(progn
  (setq user-emacs-directory "'"${EMACS_DIR}"'/"
        exordium-spell-check nil)
  (load-file "'"${EMACS_DIR}"'/init.el")
  (load-file "'"${EMACS_DIR}"'/modules/init-bde-style.t.el")
  (load-file "'"${EMACS_DIR}"'/modules/init-forge.t.el")
  (load-file "'"${EMACS_DIR}"'/modules/init-util.t.el")
  (load-file "'"${EMACS_DIR}"'/modules/init-lib.t.el")
  (load-file "'"${EMACS_DIR}"'/modules/init-require.t.el")
  (load-file "'"${EMACS_DIR}"'/modules/init-highlight.t.el")
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
