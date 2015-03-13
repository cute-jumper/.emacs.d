;;; qjp-programming-elisp.el --- Settings for Emacs Lisp

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

;; --------------- ;;
;; emacs lisp mode ;;
;; --------------- ;;
(require 'eldoc)
(defadvice eldoc-highlight-function-argument
  (around my-formatting (sym args index) compile activate preactivate)
  "Replace original to apply my style of formatting."
  ;; HACK: intercept the call to eldoc-docstring-format-sym-doc at the
  ;; end of the adviced function. This is obviously brittle, but the
  ;; alternative approach of copy/pasting the original also has
  ;; downsides...
  (cl-flet ((eldoc-docstring-format-sym-doc
          (sym doc face)
          (let* ((function-name (propertize (symbol-name sym)
                                            'face face))
                 (spec (format "%s %s" function-name doc))
                 (docstring (or (eldoc-docstring-first-line
                                 (documentation sym t))
                                "Undocumented."))
                 (docstring (propertize docstring
                                        'face 'font-lock-doc-face))
                 ;; TODO: currently it strips from the start of spec by
                 ;; character instead of whole arguments at a time.
                 (fulldoc (format "%s: %s" spec docstring))
                 (ea-width (1- (window-width (minibuffer-window)))))
            (cond ((or (<= (length fulldoc) ea-width)
                       (eq eldoc-echo-area-use-multiline-p t)
                       (and eldoc-echo-area-use-multiline-p
                            (> (length docstring) ea-width)))
                   fulldoc)
                  ((> (length docstring) ea-width)
                   (substring docstring 0 ea-width))
                  ((>= (- (length fulldoc) (length spec)) ea-width)
                   docstring)
                  (t
                   ;; Show the end of the partial symbol name, rather
                   ;; than the beginning, since the former is more likely
                   ;; to be unique given package namespace conventions.
                   (setq spec (substring spec (- (length fulldoc) ea-width)))
                   (format "%s: %s" spec docstring))))))
    ad-do-it))

(eldoc-add-command
 'paredit-backward-delete
 'paredit-close-round
 'electrify-return-if-match)

(add-hook 'emacs-lisp-mode-hook (lambda () 
                                  (hl-line-mode -1)
                                  (turn-on-eldoc-mode)
                                  (highlight-parentheses-mode)
                                  (paredit-mode)
                                  (hideshowvis-minor-mode)
                                  (local-set-key (kbd "RET") 'electrify-return-if-match)))
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

(provide 'qjp-programming-elisp)
;;; qjp-programming-elisp.el ends here
