;;; init.el --- Let's begin our amazing journey here!

;; Copyright (C) 2013  Junpeng Qiu

;; Author: Junpeng Qiu <qjpchmail@gmail.com>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

;; ----------------------------------------------------------------------- ;;
;; First Part: Load all settings under `startup' to finish initialization. ;;
;; ----------------------------------------------------------------------- ;;

;; Define the very root and the startup code directory
(defvar qjp-base-dir (file-name-directory load-file-name)
  "The base directory for configuration files.")
(defvar qjp-startup-dir (expand-file-name "startup" qjp-base-dir)
  "The directory to store elisp scripts that runs at the very beginning.")

;; Add `startup' to load path
(add-to-list 'load-path qjp-startup-dir)

;; Filter function
(defun qjp-filter (condp lst)
    "Filter function from http://emacswiki.org/emacs/ElispCookbook#toc46"
    (delq nil
          (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

;; Function to get feature list
(defun qjp-get-feature-list (dir)
  "List file names under dir after removing .el suffix"
  (qjp-filter
   (lambda (dir) (not (or (string= "." x) (string= ".." x) (string-match "#" x))))
   (mapcar (lambda (x) (substring x 0 (string-match "\\.el" x))) (directory-files dir))))

;; Require all the features in startup
(mapc 'require (mapcar 'intern (qjp-get-feature-list qjp-startup-dir)))

;; -------------------------------------------------------------- ;;
;; Second Part: Load selected settings to complete configuration. ;;
;; -------------------------------------------------------------- ;;

;; Add `modules' and `site-lisp' to `loat-path'
(add-to-list 'load-path qjp-modules-dir)
(qjp-add-subdirectories-to-load-path qjp-site-lisp-dir)
;; Note: Now we are done with the load path

;; Requrire module features as you want
(require 'qjp-esk)                      ;Emacs starter kit
(require 'qjp-basic)                    ;basic settings
(require 'qjp-functions)                ;useful functions
(require 'qjp-keybindings)              ;key bindings
(require 'qjp-misc)                     ;various modes/features
(require 'qjp-org)                      ;org-mode
(require 'qjp-tex)                      ;TeX
(require 'qjp-programming)              ;Programmning
(require 'qjp-site-lisp)                ;site-lisp

;; Welcome message
(message "Welcome to Emacs %s, %s!" emacs-version user-full-name)

;;; init.el ends here
