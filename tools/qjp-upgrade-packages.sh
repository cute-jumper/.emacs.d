#! /bin/bash
#-*- coding: utf-8 -*-
# Author: qjp
# Date: <2015-04-13 Mon>

emacs -Q\
      --batch\
      --eval "(defvar qjp-base-dir user-emacs-directory)" \
      -l "~/.emacs.d/startup/03qjp-package-manager.el"\
       --eval "(package-list-packages)"\
      --eval "(package-menu-mark-upgrades)"\
      --eval "(package-menu-mark-obsolete-for-deletion)"\
      --eval '(message ">> Start upgrading...")'\
      --eval "(package-menu-execute t)"\
      --eval '(message ">> Finish upgrading!")' \
      && emacs --version | head -n 1 | toilet -F metal -f future
