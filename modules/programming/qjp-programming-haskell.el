;;; qjp-programming-haskell.el --- Settings for Haskell

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

;; ------- ;;
;; Haskell ;;
;; ------- ;;
(defun qjp-haskell-mode-hook ()
  "My mode hook for Haskell mode."
  (turn-on-haskell-indentation)
  (turn-on-haskell-doc-mode +1)
  (subword-mode +1)
  (eldoc-mode +1))

(add-hook 'haskell-mode-hook #'qjp-haskell-mode-hook)

(provide 'qjp-programming-haskell)
;;; qjp-programming-haskell.el ends here
