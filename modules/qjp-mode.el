;;; qjp-mode.el --- qjp minor mode:-)                -*- lexical-binding: t; -*-

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

;; TODO: add tips

;;; Code:

(require 'qjp-defuns)

(defvar qjp-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Bindings for `qjp-defuns-edit'
    (define-key map (kbd "C-h") #'backward-delete-char)
    (define-key map (kbd "M-h") #'backward-kill-word)
    (define-key map (kbd "M-s r") #'replace-string)
    (define-key map (kbd "C-c d") #'qjp-duplicate-line-or-region)
    (define-key map [remap move-beginning-of-line]
      #'qjp-back-to-indentation-or-beginning)
    (define-key map (kbd "M-;") #'qjp-comment-dwim-line)
    (define-key map (kbd "C-o") #'qjp-open-new-line)
    (define-key map [S-up] #'qjp-move-line-up)
    (define-key map [S-down] #'qjp-move-line-down)
    (define-key map (kbd "M-m") #'qjp-kill-back-to-indentation)

    ;; Bindings for `qjp-defuns-misc'
    (define-key map (kbd "C-x M-=") #'qjp-calc-eval-and-replace)

    ;; ------------------ ;;
    ;; Other key bindings ;;
    ;; ------------------ ;;
    (define-key map (kbd "C-x C-b") #'ibuffer)
    (define-key map (kbd "C-x C-r") #'revert-buffer)
    (define-key map [remap just-one-space] #'cycle-spacing)
    (define-key map [remap count-words-region] #'count-words)
    (define-key map (kbd "C-x I") #'imenu)
    (define-key map (kbd "C-x O") #'(lambda () (interactive) (other-window -1)))
    (define-key map (kbd "M-/") #'hippie-expand) ;; hippie-expand
    (define-key map (kbd "C-c e") #'qjp-eval-and-replace)
    (define-key map (kbd "C-z") #'qjp-switch-to-scratch-or-back)

    ;; Mouse clicks are annoying. Globally set mouse 1's single-click events to
    ;; `qjp-nop'. Navigating using doulbe-click is still available.
    (define-key map [down-mouse-1] nil)
    (define-key map [mouse-1] #'qjp-nop)
    (define-key map (kbd "<C-mouse-1>") #'qjp-nop)
    (define-key map (kbd "<C-down-mouse-1>") #'qjp-nop)
    map))

;; `M-o' prefix keymap
(define-prefix-command 'meta-o-map)
(global-unset-key (kbd "M-o"))
(define-key qjp-mode-map (kbd "M-o") 'meta-o-map)
(define-key meta-o-map (kbd "s") #'qjp-isearch-other-window)
(define-key meta-o-map (kbd "o") #'occur)

(define-minor-mode qjp-mode
  "Minor mode designed for Junpeng Qiu!

\\{qjp-mode-map}"
  nil
  " QJP"
  qjp-mode-map
  :group qjp-mode)

(define-globalized-minor-mode global-qjp-mode
  qjp-mode
  qjp-mode-on
  :group 'qjp-mode
  :require 'qjp-mode)

(defun qjp-mode-on ()
  "Turn on `qjp-mode'."
  (interactive)
  ;; key-chord-mode
  (with-eval-after-load 'qjp-misc
    (qjp-key-chord-define qjp-mode-map "bb" #'helm-mini)
    (qjp-key-chord-define qjp-mode-map "xf" #'helm-find-files))
  ;; Bindings for `isearch'
  (define-key isearch-mode-map (kbd "C-p") #'isearch-yank-x-selection)
  (define-key isearch-mode-map (kbd "C-o") #'qjp-isearch-occur)
  (define-key isearch-mode-map (kbd "C-k") #'qjp-kill-isearch-match)
  (define-key isearch-mode-map (kbd "M-z") #'qjp-zap-to-isearch)
  (define-key isearch-mode-map [(control return)] #'qjp-isearch-exit-other-end)
  (qjp-mode +1))

(defun qjp-mode-off ()
  "Turn off `qjp-mode'."
  (interactive)
  ;; Bindings `qjp-defuns-isearch'
  (define-key isearch-mode-map (kbd "C-p") nil)
  (define-key isearch-mode-map (kbd "C-o") nil)
  (define-key isearch-mode-map (kbd "C-k") nil)
  (define-key isearch-mode-map (kbd "M-z") nil)
  (define-key isearch-mode-map [(control return)] nil)
  (qjp-mode -1))

(global-qjp-mode +1)

(provide 'qjp-mode)
;;; qjp-mode.el ends here
