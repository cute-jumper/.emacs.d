;;; qjp-programming-c-cpp.el --- Settings for both C and CPP

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

;; --- ;;
;; ecb ;;
;; --- ;;
(setq stack-trace-on-error t)           ;for emacs 24, I don't know why
(setq ecb-tip-of-the-day nil)
(setq ecb-layout-name "left3")

;; ------ ;;
;; cscope ;;
;; ------ ;;
;; (require 'xcscope)

;; ----------------- ;;
;; Settings and hook ;;
;; ----------------- ;;
(setq-default c-basic-offset 4)

(defun qjp-insert-parentheses (c)
  (interactive "cWhich kind of parentheses? ")
  (cond
   ((eq c ?\{) (insert-pair 0 ?\{ ?\}))
   ((eq c ?\[) (insert-pair 0 ?\[ ?\]))
   ((not (eq c ?\()) (insert-pair 0 c c))
   (t (insert-pair 0 ?\( ?\)))))

(defun qjp-c-cpp-mode-hook ()
  (local-set-key [(return)] #'qjp-electrify-return-if-match)
  (local-set-key (kbd "M-(") #'qjp-insert-parentheses)
  (local-set-key (kbd "C-m") #'up-list))

(add-hook 'c-mode-hook #'qjp-c-cpp-mode-hook)
(add-hook 'c++-mode-hook #'qjp-c-cpp-mode-hook)

(provide 'qjp-programming-c-cpp)
;;; qjp-programming-c-cpp.el ends here
