;;; qjp-misc-auto-insert.el --- Config for auto insert mode  -*- lexical-binding: t; -*-

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

(add-hook 'find-file-hooks 'auto-insert)

;; ----------------------------------- ;;
;; Auto-insert for scripting languages ;;
;; ----------------------------------- ;;
;; For time stamp insertion
(autoload 'org-insert-time-stamp "org")

(defun qjp-misc-auto-insert-interpreter (lang)
  "Return interpreter string according to LANG."
  (cond ((equal lang 'python) "#! /usr/bin/env python")
        ((equal lang 'ruby) "#! /usr/bin/env ruby")
        ((equal lang 'bash) "#! /bin/bash")))

(defun qjp-misc-auto-insert-script-template (lang)
  "Generate template according to LANG."
  `(
    "Short descriptions: "
    ,(qjp-misc-auto-insert-interpreter lang) \n
    "#-*- coding: utf-8 -*-" \n
    "# Author: " (user-full-name) \n
    "# Date: " '(org-insert-time-stamp (current-time)) \n
    "# Description: " str \n))

(define-auto-insert '("\\.py\\'" . "Python skeleton")
  (qjp-misc-auto-insert-script-template 'python))
(define-auto-insert '("\\.rb\\'" . "Ruby skeleton")
  (qjp-misc-auto-insert-script-template 'ruby))
(define-auto-insert '("\\.sh\\'" . "Shell scripts skeleton")
  (qjp-misc-auto-insert-script-template 'bash))

;; ------------------------ ;;
;; Auto-insert for Org-mode ;;
;; ------------------------ ;;
(define-auto-insert '("\\.org\\'" . "Org-mode skeleton")
  '("Short description: "
    "* " (file-name-base (buffer-file-name))))

;; --------------------- ;;
;; Auto-insert for LaTeX ;;
;; --------------------- ;;
;; TODO
(define-auto-insert '("\\.tex\\'" . "latex")  "xetex.tpl")

(provide 'qjp-misc-auto-insert)
;;; qjp-misc-auto-insert.el ends here
