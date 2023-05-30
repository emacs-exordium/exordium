#!/bin/bash

set -x
set -e

EMACS_DIR="$(cd "${GITHUB_WORKSPACE:-~}"/"${1:-.emacs.d}"; pwd -P)/"
EMACS=${EMACS:=emacs}

# Run all tests form *.t.el
${EMACS} -Q --batch \
         --eval '
(progn
  (setq user-emacs-directory "'"${EMACS_DIR}"'")
  (load-file "'"${EMACS_DIR}"'/init.el")
  (load-file "'"${EMACS_DIR}"'/modules/init-bde-style.t.el")
  (load-file "'"${EMACS_DIR}"'/modules/init-forge.t.el")
  (load-file "'"${EMACS_DIR}"'/modules/init-util.t.el")
  (load-file "'"${EMACS_DIR}"'/modules/init-lib.t.el")
  (ert-run-tests-batch-and-exit))'
