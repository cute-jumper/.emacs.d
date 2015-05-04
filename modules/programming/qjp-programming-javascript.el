;;; qjp-programming-javascript.el --- Settings for javascript

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

;; javascript
;; Fix for Greasemonkey scripts
(with-eval-after-load 'js
  (setq js--regexp-literal-fix
         "[^=][=(,:]\\(?:\\s-\\|\n\\)*\\(/\\)\\(?:\\\\.\\|[^/*\\]\\)\\(?:\\\\.\\|[^/\\]\\)*\\(/\\)")
  (setq js-font-lock-syntactic-keywords-fix
        ;; "|" means generic string fence
        `((,js--regexp-literal-fix (1 "|") (2 "|"))))
  (setq js-font-lock-syntactic-keywords js-font-lock-syntactic-keywords-fix))
(add-hook 'js-mode-hook (lambda () (local-set-key [(return)] 'electrify-return-if-match)))

(provide 'qjp-programming-javascript)
;;; qjp-programming-javascript.el ends here
