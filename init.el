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

;; (package-initialize)

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

;; Profiler related
(defvar qjp--init-time-alist nil
  "Alist containing init time information.")

(defmacro qjp-timed (sexp name &optional module)
  (let ((module-name (or module "00default")))
    `(let ((start-time (current-time))
           (init-times (assoc
                        ,module-name
                        qjp--init-time-alist)))
       ,sexp
       (let ((elapsed
              (float-time
               (time-subtract
                (current-time)
                start-time))))
         (if init-times
             (push (cons ,name elapsed) (cdr init-times))
           (push (cons ,module-name (list (cons ,name elapsed)))
                 qjp--init-time-alist))))))

(defmacro qjp-require (library &optional module-name)
  `(qjp-timed (require ,library) ,(symbol-name (cadr library)) ,module-name))

;; Add `startup', `modules' and `site-lisp' to load path
(add-to-list 'load-path qjp-startup-dir)
(add-to-list 'load-path qjp-modules-dir)
(add-to-list 'load-path qjp-site-lisp-dir)

;; Temporarily fix tramp for Emacs 24.5 in Arch Linux
(setq tramp-ssh-controlmaster-options nil)

;; Require the init file in each directory
(qjp-require 'qjp-startup-init)
(qjp-require 'qjp-modules-init)
(qjp-require 'qjp-site-lisp-init)

(defvar qjp-init-duration (float-time
                           (time-since
                            qjp-before-init-time)))
;; Welcome message
(setq initial-scratch-message
      (format "%s;; Your initilization takes %.3f s.
;; Welcome to Emacs %s, %s:-)\n\n"
              initial-scratch-message
              qjp-init-duration
              emacs-version
              user-full-name))
;;; init.el ends here
