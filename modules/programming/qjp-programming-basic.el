;;; qjp-programming-basic.el --- Basic settings for programming

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

;; save point postiion in file
(require 'saveplace)

;; Enable which-function-mode
(which-function-mode +1)

;; -------------------------------------------- ;;
;; electriy-return, useful function copied from ;;
;; -------------------------------------------- ;;
;; http://www.emacswiki.org/emacs/ParEdit
(defvar electrify-return-match
  "[\]}\)\"]"
  "If this regexp matches the text after the cursor, do an \"electric\"
  return.")

(defun electrify-return-if-match (arg)
  "If the text after the cursor matches `electrify-return-match' then
open and indent an empty line between the cursor and the text.  Move the
cursor to the new line."
  (interactive "P")
  (let ((case-fold-search nil))
    (if (looking-at electrify-return-match)
        (save-excursion (newline-and-indent)))
    (newline arg)
    (indent-according-to-mode)))

(defun qjp-local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode +1))

(defun qjp-add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
          1 font-lock-warning-face t))))

(defun qjp-prog-mode-hook ()
  (qjp-local-comment-auto-fill)
  (qjp-add-watchwords)
  (prettify-symbols-mode +1)
  (setq save-place +1)
  (idle-highlight-mode +1)
  (hl-line-mode +1)
  (flycheck-mode +1))

(add-hook 'prog-mode-hook #'qjp-prog-mode-hook)

(provide 'qjp-programming-basic)
;;; qjp-programming-basic.el ends here
