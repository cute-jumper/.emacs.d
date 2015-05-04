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

;; ------------------- ;;
;; auto-complete-clang ;;
;; ------------------- ;;
;; (require 'auto-complete-clang)
;; (define-key ac-mode-map (kbd "M-/") 'ac-complete-clang)
;; (defun clang-set-gtk-flags()
;;   (interactive)
;;   (setq ac-clang-flags
;;         (mapcar (lambda
;;                   (item)
;;                   (concat "-I" item))
;;                 (split-string
;;                  "
;; /usr/include/atk-1.0
;; /usr/include/cairo
;; /usr/include/gdk-pixbuf-2.0
;; /usr/include/pango-1.0
;; /usr/include/gio-unix-2.0/
;; /usr/include/glib-2.0
;; /usr/lib/x86_64-linux-gnu/glib-2.0/include
;; /usr/include/pixman-1
;; /usr/include/freetype2
;; /usr/include/libpng12
;; /usr/include/gtk-3.0
;; /usr/include/c++/4.6
;; /usr/include/c++/4.6/x86_64-linux-gnu/.
;; /usr/include/c++/4.6/backward
;; /usr/lib/gcc/x86_64-linux-gnu/4.6.1/include
;; /usr/local/include
;; /usr/lib/gcc/x86_64-linux-gnu/4.6.1/include-fixed
;; /usr/include/x86_64-linux-gnu
;; /usr/include
;; "))))

;; cedet

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
  (local-set-key [(return)] 'electrify-return-if-match)
  (local-set-key [C-backspace] 'c-hungry-delete)
  (local-set-key (kbd "M-(") 'qjp-insert-parentheses)
  (local-set-key (kbd "C-m") 'up-list)
  (hideshowvis-minor-mode))

(add-hook 'c-mode-hook 'qjp-c-cpp-mode-hook)
(add-hook 'c++-mode-hook 'qjp-c-cpp-mode-hook)

(provide 'qjp-programming-c-cpp)
;;; qjp-programming-c-cpp.el ends here
