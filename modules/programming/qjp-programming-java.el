;;; qjp-programming-java.el --- Settings for Java

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

;; ---- ;;
;; Java ;;
;; ---- ;;
(autoload 'ensime-mode "ensime")

(defun qjp-java-mode-hook ()
  (ensime-mode +1)
  (subword-mode +1)
  (local-set-key [(return)] #'qjp-electrify-return-if-match))

(add-hook 'java-mode-hook #'qjp-java-mode-hook)
(add-to-list 'qjp-indent-when-closing-pair-modes 'java-mode)

(provide 'qjp-programming-java)
;;; qjp-programming-java.el ends here
