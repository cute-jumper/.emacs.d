;;; qjp-misc-god-mode.el --- God mode settings       -*- lexical-binding: t; -*-

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

;; -------- ;;
;; god-mode ;;
;; -------- ;;
(setq god-mod-alist '((nil . "C-")
                      ("g" . "C-M-")
                      ("G" . "M-")))
;; make it usable in minibuffer
(define-key minibuffer-local-map (kbd "<escape>") #'god-local-mode)
(with-eval-after-load 'qjp-mode
  ;; convenient window management key bindings
  (define-key qjp-mode-map (kbd "C-x C-1") 'delete-other-windows)
  (define-key qjp-mode-map (kbd "C-x C-2") 'split-window-below)
  (define-key qjp-mode-map (kbd "C-x C-3") 'split-window-right)
  (define-key qjp-mode-map (kbd "C-x C-0") 'delete-window)
  (define-key qjp-mode-map (kbd "C-x C-b") 'helm-mini)
  (define-key qjp-mode-map (kbd "C-x M-b") 'ibuffer)
  (qjp-define-highest-priority-mode-function god-local-mode)
  (defun qjp-god-mode-enable-hook ()
    (set (make-local-variable 'input-method-function) nil)
    (qjp-gain-highest-keys-priority-god-local-mode nil)
    (setq cursor-type 'hollow))
  (add-hook 'god-mode-enabled-hook 'qjp-god-mode-enable-hook)
  (defun qjp-god-mode-disable-hook ()
    (setq input-method-function 'key-chord-input-method)
    (setq cursor-type 'bar))
  (add-hook 'god-mode-disabled-hook 'qjp-god-mode-disable-hook))
(with-eval-after-load 'god-mode
  (define-key god-local-mode-map "i" #'god-local-mode)
  (define-key god-local-mode-map "m" #'kill-word)
  (define-key god-local-mode-map "I" #'backward-kill-word)
  (define-key god-local-mode-map "." #'repeat)
  ;; isearch god-mode integration
  (require 'god-mode-isearch)
  (defun qjp-god-mode-isearch-activate ()
    (interactive)
    (set-cursor-color "yellow")
    (god-mode-isearch-activate))
  (define-key isearch-mode-map (kbd "<escape>") #'qjp-god-mode-isearch-activate)
  (qjp-key-chord-define isearch-mode-map "jj" #'qjp-god-mode-isearch-activate)
  (defun qjp-god-mode-isearch-disable ()
    (interactive)
    (set-cursor-color "white")
    (god-mode-isearch-disable))
  (define-key god-mode-isearch-map (kbd "<escape>") 'qjp-god-mode-isearch-disable)
  (add-hook 'isearch-mode-end-hook (lambda () (set-cursor-color "white"))))

(provide 'qjp-misc-god-mode)
;;; qjp-misc-god-mode.el ends here
