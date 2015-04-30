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
(qjp-require 'qjp-esk "20modules")                    ;Emacs starter kit
(qjp-require 'qjp-basic "20modules")                ;basic settings
(qjp-require 'qjp-defuns "20modules")              ;useful defuns
(qjp-require 'qjp-keybindings "20modules")    ;key bindings
(qjp-require 'qjp-misc "20modules")                  ;various modes/features
(qjp-require 'qjp-org "20modules")                    ;org-mode
(qjp-require 'qjp-tex "20modules")                    ;TeX
(qjp-require 'qjp-programming "20modules")    ;Programmning

(provide 'qjp-modules-init)
;;; qjp-modules-init.el ends here
