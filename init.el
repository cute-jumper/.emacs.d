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

;; Directories for configuration
(defvar qjp-base-dir (file-name-directory load-file-name)
  "The base directory for configuration files.")
(defvar qjp-startup-dir (expand-file-name "startup" qjp-base-dir)
  "The directory to store elisp scripts that runs at the very beginning.")
(defvar qjp-modules-dir (expand-file-name "modules" qjp-base-dir)
  "The directory to place configuration for various modules.")
(defvar qjp-site-lisp-dir (expand-file-name "site-lisp" qjp-base-dir)
  "The directory to hold personal packages.")

;; Declare variables. Change values in startup/qjp-global-variables.el
(defvar qjp-document-dir "" "Personal document base directory.")

(defun qjp-filter (condp lst)
    "Filter function from http://emacswiki.org/emacs/ElispCookbook#toc46"
    (delq nil
          (mapcar (lambda (x) (and (funcall condp x) x)) lst)))
  
(defun qjp-add-subdirectories-to-load-path (base-directory)
  "Add all subdirectories to load path."
  (interactive)
  (mapc
   (lambda (subdir)
     (add-to-list 'load-path subdir))
   (qjp-filter
    (lambda (x)
      (and (file-directory-p x) (not (string-match "\\.$" x)) (not (string= "\\.\\.$" x))))
    (directory-files base-directory t))))

;; Add to load path
(add-to-list 'load-path qjp-startup-dir)
(add-to-list 'load-path qjp-module-dir)
(qjp-add-subdirectories-to-load-path qjp-site-lisp-dir)
;; Now we are done with the load path

(defun qjp-get-feature-list (dir)
  "List file names under dir after removing .el suffix"
  (qjp-filter
   (lambda (dir) (not (or (string= "." x) (string= ".." x) (string-match "#" x))))
   (mapcar (lambda (x) (substring x 0 (string-match "\\.el" x))) (directory-files dir))))

;; Require all the features in startup
(mapc 'require (mapcar 'intern (qjp-get-feature-list qjp-startup-dir)))

;; Welcome message
(message "Welcome to Emacs %s, %s!" emacs-version user-full-name)

;;; init.el ends here
