;;; qjp-site-lisp-init.el --- init file for site lisp directory  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Junpeng Qiu

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

(qjp-add-subdirectories-to-load-path qjp-site-lisp-dir)

;; ----------------------- ;;
;; Deft, modified version! ;;
;; ----------------------- ;;
(defun qjp-site-lisp-deft ()
  (require 'deft)
  (setq deft-directory qjp-document-dir)
  (setq deft-extension "org")
  (setq deft-text-mode 'org-mode))

;; -------------------------------------------------------- ;;
;; matlab mode from https://github.com/pronobis/matlab-mode ;;
;; -------------------------------------------------------- ;;
(defun qjp-site-lisp-matlab-mode ()
  (load-file
   (concat qjp-modules-dir "/programming/qjp-programming-matlab.el")))

(qjp-site-lisp-deft)
(qjp-site-lisp-matlab-mode)

(provide 'qjp-site-lisp-init)
;;; qjp-site-lisp-init.el ends here
