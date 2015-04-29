;;; qjp-programming.el --- Settings for programming!

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

;; Require basic feature
(qjp-modules-require-subdir-feature "programming" "qjp-programming-basic")

;; Define lauguages to be configured
(defvar qjp-programming-languages '(c-cpp java python ruby scala haskell elisp javascript lua)
  "My programming lauguages")

;; Load all settings
(mapc (lambda (x)
        (qjp-modules-require-subdir-feature
         "programming"
         (concat "qjp-programming-" (symbol-name x))))
      qjp-programming-languages)

(provide 'qjp-programming)
;;; qjp-programming.el ends here
