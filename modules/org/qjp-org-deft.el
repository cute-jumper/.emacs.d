;;; qjp-org-deft.el --- Combine org-mode with deft

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

;; ----------------------- ;;
;; Deft, modified version! ;;
;; ----------------------- ;;
(require 'deft)
(setq deft-directory qjp-document-dir)
(setq deft-extension "org")
(setq deft-text-mode 'org-mode)

(provide 'qjp-org-deft)
;;; qjp-org-deft.el ends here
