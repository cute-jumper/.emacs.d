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
(qjp-timed (require 'qjp-esk) "esk")                   ;Emacs starter kit
(qjp-timed (require 'qjp-basic) "basic")                 ;basic settings
(qjp-timed (require 'qjp-defuns) "defuns")                ;useful defuns
(qjp-timed (require 'qjp-keybindings) "keybinginds")           ;key bindings
(qjp-timed (require 'qjp-misc) "misc")                  ;various modes/features
(qjp-timed (require 'qjp-org) "org")                   ;org-mode
(qjp-timed (require 'qjp-tex) "tex")                   ;TeX
(qjp-timed (require 'qjp-programming) "programming")           ;Programmning

(provide 'qjp-modules-init)
;;; qjp-modules-init.el ends here
