;;; qjp-programming-matlab.el --- Settings for MATLAB

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

;; ------ ;;
;; MATLAB ;;
;; ------ ;;
(autoload 'matlab-mode "matlab" "Matlab Editing Mode" t)
(add-to-list 'auto-mode-alist '("\\.m$" . matlab-mode))
(add-hook 'matlab-mode-hook
          (lambda ()
            (local-set-key (kbd "M-;") 'my-comment-dwim-line)
            (local-set-key [return] 'electrify-return-if-match)
            (local-set-key (kbd "C-h") 'backward-delete-char)
            (local-set-key (kbd "M-h") 'backward-kill-word)
            (local-set-key (kbd "C-x I") 'imenu)
            (local-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1)))
            (local-set-key (kbd "M-/") 'hippie-expand) ;; hippie-expand
            ))

(provide 'qjp-programming-matlab)
;;; qjp-programming-matlab.el ends here
