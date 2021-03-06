;;; qjp-programming-qml.el --- Settings for QML files  -*- lexical-binding: t; -*-

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

;; qml
(add-to-list 'auto-mode-alist '("\\.qml\\'" . qml-mode))

(defun qjp-qml-mode-hook ()
  (add-to-list (make-local-variable 'company-backends) 'company-qml))

(add-hook 'qml-mode-hook 'qjp-qml-mode-hook)

(provide 'qjp-programming-qml)
;;; qjp-programming-qml.el ends here
