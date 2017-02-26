;;; qjp-misc-qjp-leader-mode.el --- God mode settings       -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Junpeng Qiu

;; Author: Junpeng Qiu <qjpchmail@gmail.com>
;; Keywords:

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

;; --------------- ;;
;; qjp-leader-mode ;;
;; --------------- ;;
(setq qjp-leader-special '(?p ?h ?f ?g ?q ?y ?m ?o))

;; convenient window management key bindings
(define-key qjp-mode-map (kbd "C-x C-1") 'delete-other-windows)
(define-key qjp-mode-map (kbd "C-x C-2") 'split-window-below)
(define-key qjp-mode-map (kbd "C-x C-3") 'split-window-right)
(define-key qjp-mode-map (kbd "C-x C-0") 'delete-window)
(define-key qjp-mode-map (kbd "C-x C-b") 'helm-mini)
(define-key qjp-mode-map (kbd "C-x C-M-b") 'ibuffer)
(qjp-define-highest-priority-mode-function qjp-leader-local-mode)
(defun qjp-qjp-leader-mode-enable-hook ()
  (qjp-gain-highest-keys-priority-qjp-leader-local-mode nil)
  (setq cursor-type 'hollow))
(add-hook 'qjp-leader-mode-enabled-hook 'qjp-qjp-leader-mode-enable-hook)
(defun qjp-qjp-leader-mode-disable-hook ()
  (setq cursor-type 'bar))
(add-hook 'qjp-leader-mode-disabled-hook 'qjp-qjp-leader-mode-disable-hook)

(with-eval-after-load 'qjp-leader-mode
  (define-key qjp-leader-local-mode-map (kbd "SPC") #'helm-mini))

(provide 'qjp-misc-qjp-leader-mode)
;;; qjp-misc-qjp-leader-mode.el ends here
