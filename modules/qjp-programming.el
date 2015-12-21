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

(add-to-list 'load-path (concat qjp-modules-dir "/programming"))

;; Require basic feature
(require 'qjp-programming-basic)

;; Define lauguages to be configured
(defvar qjp-programming-languages '(c-cpp java python ruby scala haskell elisp javascript lua qml)
  "My programming lauguages.")

;; Load all settings
(mapc #'(lambda (x)
          (require
           (intern
            (concat "qjp-programming-"
                    (symbol-name x)))))
      qjp-programming-languages)

(provide 'qjp-programming)
;;; qjp-programming.el ends here
