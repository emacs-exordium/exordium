#! /bin/sh
emacs -Q --eval "(progn
       (setq user-emacs-directory \"${PWD}/\")
       (load-file \"${PWD}/init.el\"))"
