;;; qjp-misc-auto-insert-config.el --- Config for auto insert mode  -*- lexical-binding: t; -*-

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

(setq auto-insert t)
(setq auto-insert-query t)
(add-hook 'find-file-hooks 'auto-insert)
(setq auto-insert-directory (concat qjp-base-dir "templates/"))

;; from http://www.emacswiki.org/emacs/AutoInsertMode
(defun my/autoinsert-yas-expand ()
  "Replace text in yasnippet template."
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))
;; auto insert current date
(defun my/autoinsert-current-date ()
  "Auto insert date string using org-mode's utilities"
  (goto-char (1- (point-max)))
  (org-insert-time-stamp (current-time)))

(define-auto-insert '("\\.tex\\'" . "latex")  "xetex.tpl" )
(define-auto-insert '("\\.py\\'" . "python") ["python.tpl" my/autoinsert-current-date])
(define-auto-insert '("\\.rb\\'" . "ruby") ["ruby.tpl" my/autoinsert-current-date])
(define-auto-insert '("\\.sh\\'" . "shell scripts")  ["bash.tpl" my/autoinsert-current-date])
(define-auto-insert '("\\.org\\'" . "org-mode")  ["org.tpl" my/autoinsert-yas-expand])

(provide 'qjp-misc-auto-insert-config)
;;; qjp-misc-auto-insert-config.el ends here
