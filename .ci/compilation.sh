#!/bin/bash

set -x
set -e

EMACS_DIR="${GITHUB_WORKSPACE:-~}/${1:-.emacs.d}"
EMACS="${EMACS:=emacs}"

echo "TODO: fix errors on compile, then enable testing"

# Byte compile all `.el` files in modules, themes, and extensions
# ${EMACS} -Q --batch \
#          --eval '
# (progn
#    (setq debug-on-error t
#          byte-compile-error-on-warn t
#          user-emacs-directory "'${EMACS_DIR}'")
#    (load-file "'${EMACS_DIR}'/init.el"))' \
#          -f batch-byte-compile ${EMACS_DIR}/{modules,themes,extensions}/*.el
