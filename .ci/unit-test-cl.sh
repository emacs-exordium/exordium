#!/bin/bash

set -x
set -e
EMACS_DIR="$(cd ${GITHUB_WORKSPACE:-~}/${1:-.emacs.d}; pwd -P)/"
EMACS="${EMACS:=emacs}"

${EMACS} -Q --batch \
         --eval '
(progn
   (setq debug-on-error t
         user-emacs-directory "'${EMACS_DIR}'")
   (load-file "'${EMACS_DIR}'/init.el")
   (load-file "'${EMACS_DIR}'/modules/init-util-cl.t.el")
   (ert-run-tests-batch-and-exit))'
