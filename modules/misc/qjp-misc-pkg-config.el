;;; qjp-misc-pkg-config.el --- Settings for various packages

;; Copyright (C) 2014  Junpeng Qiu

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

;; -------------- ;;
;; key-chord mode ;;
;; -------------- ;;
(defun qjp-misc-key-chord ()
  (require 'key-chord)
  (key-chord-mode t))

;; -------------------- ;;
;; region-bindings mode ;;
;; -------------------- ;;
(defun qjp-misc-region-bindings ()
  (require 'region-bindings-mode)
  (region-bindings-mode-enable))

;; ---------------- ;;
;; goto-last-change ;;
;; ---------------- ;;
(defun qjp-misc-goto-last-change ()
  (require 'goto-last-change)
  (global-set-key "\C-x\C-\\" 'goto-last-change))

;; ------------- ;;
;; ace-jump-mode ;;
;; ------------- ;;
(defun qjp-misc-ace-jump ()
  (global-set-key (kbd "C-;") 'ace-jump-line-mode)
  (global-set-key (kbd "C-:") 'ace-jump-mode))

;; --------------- ;;
;; ace-jump-buffer ;;
;; --------------- ;;
(defun qjp-misc-ace-jump-buffer ()
  (key-chord-define-global "jb" 'ace-jump-buffer))

;; --------- ;;
;; jump-char ;;
;; --------- ;;
(defun qjp-misc-jump-char ()
  (key-chord-define-global "jc" 'jump-char-forward)
  (key-chord-define-global "gg" 'jump-char-backward))

;; ------------- ;;
;; expand-region ;;
;; ------------- ;;
(defun qjp-misc-expand-region ()
  (require 'expand-region)
  (global-set-key (kbd "C-=") 'er/expand-region)
  ;; (pending-delete-mode)
  )

;; ------ ;;
;; ispell ;;
;; ------ ;;
(defun qjp-misc-ispell ()
  (setq ispell-program-name "hunspell")
  (require 'rw-hunspell))

;; ---------- ;;
;; dictionary ;;
;; ---------- ;;
(defun qjp-misc-dictionary ()
  (setq dictionary-tooltip-dictionary "stardic")
  (setq dictionary-server "localhost")
  (global-set-key (kbd "C-c d") 'dictionary-search))

;; --- ;;
;; w3m ;;
;; --- ;;
(defun qjp-misc-w3m ()
  (setq w3m-default-display-inline-images t)
  (setq w3m-home-page "http://www.google.com"))

;; ---------------- ;;
;; command-log-mode ;;
;; ---------------- ;;
(defun qjp-misc-command-log ()
  (require 'command-log-mode))

;; ----- ;;
;; calfw ;;
;; ----- ;;
(defun qjp-misc-calfw ()
  (require 'calfw)
  (require 'calfw-org))

;; ------------- ;;
;; markdown-mode ;;
;; ------------- ;;
(defun qjp-misc-markdown ()
  (require 'markdown-mode+)
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

;; ----------- ;;
;; sr-speedbar ;;
;; ----------- ;;
(defun qjp-misc-sr-speedbar ()
  (require 'sr-speedbar)
  (setq sr-speedbar-right-side nil)
  (setq speedbar-use-images nil)
  (global-set-key [f8] 'sr-speedbar-toggle))

;; --------------- ;;
;; predictive mode ;;
;; --------------- ;;
(defun qjp-misc-predictive ()
  (require 'predictive))

;; --------- ;;
;; term-mode ;;
;; --------- ;;
(defun qjp-misc-term ()
  ;; Turn of yasnippet in order to let tab behave normally
  (add-hook 'term-mode-hook (lambda () (yas-minor-mode -1))))


(provide 'qjp-misc-pkg-config)
;;; qjp-misc-pkg-config.el ends here
