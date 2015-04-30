#! /bin/bash
#-*- coding: utf-8 -*-
# Author: qjp
# Date: <2015-04-29 Wed>

emacs -Q\
      --batch\
      -l "~/.emacs.d/startup/03qjp-package-manager.el"\
      --eval "(defvar qjp-base-dir user-emacs-directory)" \
      --eval "(package-list-packages)"\
      --eval "(message \">> Start installing $1!\")" \
      --eval "(package-install '$1)"\
      --eval '(qjp-update-installed-package-list)'\
      --eval "(message \">> Finish installing $1!\")" \
      && emacs --version | head -n 1 | toilet -F metal -f future


