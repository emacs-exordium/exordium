#!/bin/bash

set -x
set -e

EMACS_DIR="$(cd ${GITHUB_WORKSPACE:-~}/${1:-.emacs.d}; pwd -P)/"
EMACS="${EMACS:=emacs}"

# Byte compile all `.el` files in modules, themes, and extensions
${EMACS} -Q --batch \
         --eval '
(progn
   (setq debug-on-error t
         user-emacs-directory "'${EMACS_DIR}'")
   (load-file "'${EMACS_DIR}'/init.el"))' \
         -f batch-byte-compile ${EMACS_DIR}/{modules,extensions}/*.el
