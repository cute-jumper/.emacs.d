;;; qjp-misc-builtin-mode.el --- Enable or disable some built-in modes

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

(which-function-mode)
(tooltip-mode)
(electric-pair-mode)
(column-number-mode)
(display-battery-mode)
(tool-bar-mode -1)

;; show-paren-mode
(show-paren-mode)
(setq show-paren-style 'mixed)

;; ibuffer
(setq ibuffer-use-other-window t)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(provide 'qjp-misc-builtin-mode)
;;; qjp-misc-builtin-mode.el ends here
