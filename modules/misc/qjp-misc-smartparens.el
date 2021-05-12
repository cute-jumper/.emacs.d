;;; qjp-misc-smartparens.el --- Smartparens configurations  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Junpeng Qiu

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

;; ----------- ;;
;; smartparens ;;
;; ----------- ;;
(with-eval-after-load 'smartparens
  (require 'smartparens-config))
(setq sp-base-key-bindings 'paredit)
(setq sp-hybrid-kill-entire-symbol nil)

(with-eval-after-load 'python-mode
  (sp-local-pair 'python-mode "(" ")" :unless '(sp-point-before-word-p)))

;; key bindings
(define-key qjp-mode-map (kbd "C-M-f") #'sp-forward-sexp)
(define-key qjp-mode-map (kbd "C-M-b") #'sp-backward-sexp)
(define-key qjp-mode-map (kbd "C-M-e") #'sp-end-of-sexp)
(define-key qjp-mode-map (kbd "C-M-u") #'sp-backward-up-sexp)
(define-key qjp-mode-map (kbd "C-M-j") #'sp-up-sexp)
(define-key qjp-mode-map (kbd "C-M-h") #'sp-backward-down-sexp)

(provide 'qjp-misc-smartparens)
;;; qjp-misc-smartparens.el ends here
