;;; qjp-misc-multiple-cursors.el --- Settings for multiple-cursors mode

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

;; Rely on region-bindings-mode
(qjp-misc-region-bindings)

;; Region key bindings
(define-key region-bindings-mode-map "a" 'mc/mark-all-like-this)
(define-key region-bindings-mode-map "p" 'mc/mark-previous-like-this)
(define-key region-bindings-mode-map "n" 'mc/mark-next-like-this)
(define-key region-bindings-mode-map "e" 'mc/edit-lines)
(define-key region-bindings-mode-map "s" 'mc/mark-all-symbols-like-this)
(define-key region-bindings-mode-map "S" 'mc/mark-all-symbols-like-this-in-defun)
(define-key region-bindings-mode-map "d" 'mc/mark-all-like-this-dwim)

(define-key ctrl-c-extension-map "ma" 'mc/mark-all-like-this)
(define-key ctrl-c-extension-map "mp" 'mc/mark-previous-like-this)
(define-key ctrl-c-extension-map "mn" 'mc/mark-next-like-this)
(define-key ctrl-c-extension-map "me" 'mc/edit-lines)
(define-key ctrl-c-extension-map "ms" 'mc/mark-all-symbols-like-this)
(define-key ctrl-c-extension-map "mS" 'mc/mark-all-symbols-like-this-in-defun)
(define-key ctrl-c-extension-map "md" 'mc/mark-all-like-this-dwim)

;; Using mouse!
(define-key qjp-mode-map (kbd "M-S-<mouse-1>") 'mc/add-cursor-on-click)

(provide 'qjp-misc-multiple-cursors)
;;; qjp-misc-multiple-cursors.el ends here
