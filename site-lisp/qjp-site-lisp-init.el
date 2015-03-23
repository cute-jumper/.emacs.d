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

;; --------------- ;;
;; gscholar-bibtex ;;
;; --------------- ;;
(defun qjp-site-lisp-gscholar-bibtex ()
  (add-to-list 'load-path (expand-file-name "~/Programs/EmacsLisp/gscholar-bibtex/"))
  (require 'gscholar-bibtex))

;; ------------ ;;
;; fetch-bibtex ;;
;; ------------ ;;
;; (defun qjp-site-lisp-fetch-bibtex ()
;;   (require 'fetch-bibtex)
;;   (setq fetch-bibtex-script
;;         (expand-file-name
;;          "fetch_bibtex.py" (expand-file-name "fetch-bibtex" qjp-site-lisp-dir))))

;; ------- ;;
;; THUmacs ;;
;; ------- ;;
;; (defun qjp-site-lisp-thumacs ()
;;   (require 'deadline-util)
;;   (setq dp-userid "")                   ;Demo only
;;   (setq dp-userpass "")                 ;Demo only
;;   (setq dp-homework-file (concat qjp-document-dir "Agenda/homework.org")))

(qjp-site-lisp-deft)
(qjp-site-lisp-gscholar-bibtex)

(provide 'qjp-site-lisp-init)
;;; qjp-site-lisp-init.el ends here
