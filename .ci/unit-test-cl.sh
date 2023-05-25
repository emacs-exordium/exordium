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
   (ert-run-tests-batch-and-exit))'
