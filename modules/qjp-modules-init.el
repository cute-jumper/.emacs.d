;;; qjp-modules-init.el --- init file for modules directory  -*- lexical-binding: t; -*-

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

;; Require module features as you want
(require 'qjp-esk)                      ;Emacs starter kit
(require 'qjp-basic)                    ;basic settings
(require 'qjp-functions)                ;useful functions
(require 'qjp-keybindings)              ;key bindings
(require 'qjp-misc)                     ;various modes/features
(require 'qjp-org)                      ;org-mode
(require 'qjp-tex)                      ;TeX
(require 'qjp-programming)              ;Programmning

(provide 'qjp-modules-init)
;;; qjp-modules-init.el ends here
