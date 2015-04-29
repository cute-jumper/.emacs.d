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

(defvar qjp-before-init-time (current-time))

;; Define directories for the very root, the startup code , modules and other
;; configuration
(defvar qjp-base-dir (file-name-directory load-file-name)
  "The base directory for configuration files.")
(defvar qjp-startup-dir (expand-file-name "startup" qjp-base-dir)
  "The directory to store elisp scripts that runs at the very beginning.")
(defvar qjp-modules-dir (expand-file-name "modules" qjp-base-dir)
  "The directory to place configuration for various modules.")
(defvar qjp-site-lisp-dir (expand-file-name "site-lisp" qjp-base-dir)
  "The directory to hold personal packages.")

;; My own profiler
(defvar qjp-startup-times nil)

(defmacro qjp-timed (sexp name)
  `(let ((start-time (current-time)))
     ,sexp
     (let ((elapsed
            (float-time
             (time-subtract
              (current-time)
              start-time))))
       (push (cons ,name elapsed) qjp-startup-times))))

(defun qjp-show-startup-times ()
  (interactive)
  (let ((total-time .0)) 
    (with-current-buffer (get-buffer-create "*qjp-startup-times*")
      (erase-buffer)
      (org-mode)
      (insert "| Function Name | Elapsed Time |\n")
      (insert "|---------------+--------------|\n")
      (dolist (elem qjp-startup-times)
        (insert (format "| %s | %.3f |\n" (car elem) (cdr elem)))
        (setq total-time (+ total-time (cdr elem))))
      (insert (format "| Total Time | %.3f |\n" total-time))
      (goto-char (point-min))
      (forward-line)
      (org-cycle))
    (switch-to-buffer "*qjp-startup-times*")))

;; Add `startup', `modules' and `site-lisp' to load path
(add-to-list 'load-path qjp-startup-dir)
(add-to-list 'load-path qjp-modules-dir)
(add-to-list 'load-path qjp-site-lisp-dir)

;; Require the init file in each directory
(require 'qjp-startup-init)
(require 'qjp-modules-init)
(qjp-timed (require 'qjp-site-lisp-init) "site-lisp")

;; Welcome message
(message "Your initilization takes %.3f s." (float-time
                                             (time-subtract
                                              (current-time)
                                              qjp-before-init-time)))
(message "Welcome to Emacs %s, %s!" emacs-version user-full-name)

;;; init.el ends here
