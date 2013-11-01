;;; qjp-keybindings.el --- Require all key bindings here

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

;; -------------------------------- ;;
;; From `keybindings' sub-directory ;;
;; -------------------------------- ;;

;; Require `qjp-keybindings-edit'
(qjp-require-subdir-feature "keybindings" "qjp-keybindings-edit")

;; Keybindings for editing
(global-set-key (kbd "C-h") 'backward-delete-char)
(global-set-key (kbd "M-h") 'backward-kill-word)
(global-set-key (kbd "M-s r") 'replace-string)
(global-set-key (kbd "M-Z") 'zap-up-to-char)
(global-set-key (kbd "M-n") 'qjp-fast-forward-lines)
(global-set-key (kbd "M-p") 'qjp-fast-backward-lines)
(global-set-key (kbd "C-a") 'qjp-back-to-indentation-or-beginning)
(global-set-key (kbd "M-;") 'qjp-comment-dwim-line)
(global-set-key (kbd "C-o") 'qjp-open-new-line)
(global-set-key [M-up] 'qjp-move-line-up)
(global-set-key [M-down] 'qjp-move-line-down)
(global-set-key [remap backward-up-list] 'qjp-backward-up-sexp)
(global-set-key (kbd "C-\\") 'qjp-kill-to-word)
(global-set-key (kbd "M-m") 'qjp-kill-back-to-indentation)

;; Require `qjp-keybindings-isearch'
(qjp-require-subdir-feature "keybindings" "qjp-keybindings-isearch")

;; Keybindings for isearch
(define-key isearch-mode-map (kbd "C-o") 'qjp-isearch-occur)
(define-key isearch-mode-map [(control k)] 'qjp-kill-isearch-match)
(define-key isearch-mode-map [(meta z)] 'qjp-zap-to-isearch)
(define-key isearch-mode-map (kbd "C-p") 'qjp-isearch-yank-region)
(global-set-key (kbd "M-o s") 'qjp-isearch-other-window)

;; ----------------- ;;
;; Misc key bindings ;;
;; ----------------- ;;
(global-set-key (kbd "C-x I") 'imenu)
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1)))
(global-set-key (kbd "M-/") 'hippie-expand) ;; hippie-expand
(global-set-key (kbd "C-c e") 'qjp-esk-eval-and-replace)
(global-set-key [f5] 'compile)

(provide 'qjp-keybindings)
;;; qjp-keybindings.el ends here
