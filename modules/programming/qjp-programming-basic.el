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

;; maybe put which-function-mode config and others(nyan-mode etc.) to another
;; place?

;;; Code:

;; Enable which-function-mode
(which-function-mode +1)
(setq which-func-unknown "n/a")
;; truncate which-func information
(defun qjp-which-func-current ()
  (let ((current (gethash (selected-window) which-func-table)))
    (if current
        (string-reverse (truncate-string-to-width (string-reverse current) 20 nil nil "…"))
      which-func-unknown)))
(setq which-func-format
      `("["
        (:propertize (:eval (qjp-which-func-current))
                     local-map ,which-func-keymap
                     face which-func
                     mouse-face mode-line-highlight
                     help-echo "mouse-1: go to beginning\n\
mouse-2: toggle rest visibility\n\
mouse-3: go to end")
        "]"))
;; Move which-function to more important place
(let ((which-func '(which-func-mode ("" which-func-format " ")))
      cell)
  (setq-default mode-line-format (remove which-func mode-line-format))
  (setq-default mode-line-misc-info (remove which-func mode-line-misc-info))
  (setq cell (last mode-line-format 8))
  (setcdr cell
          (cons which-func
                (cdr cell)))
  (setq-default mode-line-format (append (butlast mode-line-format 8)
                                         cell)))
(set-face-foreground 'which-func (face-foreground font-lock-function-name-face))

;; Set which-function-mode to show in header line
;; (setq-default header-line-format
;;               '((which-func-mode ("" which-func-format " "))))
;; (setq mode-line-misc-info
;;       (assq-delete-all 'which-func-mode mode-line-misc-info))

;; --------------- ;;
;; electriy-return ;;
;; --------------- ;;
;; From http://www.emacswiki.org/emacs/ParEdit
(defvar qjp-electrify-return-match
  "[\]}\)\"]"
  "If this regexp matches the text after the cursor, do an \"electric\" return.")

(defun qjp-electrify-return-if-match (arg)
  "If the text after the cursor matches `qjp-electrify-return-match' then
open and indent an empty line between the cursor and the text.  Move the
cursor to the new line."
  (interactive "P")
  (let ((case-fold-search nil))
    (when (looking-at qjp-electrify-return-match)
      (save-excursion (newline-and-indent)))
    (newline arg)
    (indent-according-to-mode)))

;; Put *Shell Command Output* buffers into view-mode
(defadvice shell-command-on-region
    (after qjp-shell-command-in-view-mode
           (start end command &optional output-buffer replace error-buffer display-error-buffer)
           activate)
  "Put \"*Shell Command Output*\" buffers into view-mode."
  (unless output-buffer
    (with-current-buffer "*Shell Command Output*"
      (view-mode 1))))

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
  (abbrev-mode +1)
  (prettify-symbols-mode +1)
  (setq save-place +1)
  (highlight-symbol-mode +1)
  (highlight-symbol-nav-mode +1)
  (hs-minor-mode +1)
  (hl-line-mode +1)
  (flycheck-mode +1)
  (smartparens-mode +1))

(add-hook 'prog-mode-hook #'qjp-prog-mode-hook)

;; sh-mode
;; zsh
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))

(defun qjp-sh-mode-hook ()
  (add-to-list (make-local-variable 'company-backends) 'company-shell))

(add-hook 'sh-mode-hook 'qjp-sh-mode-hook)

(provide 'qjp-programming-basic)
;;; qjp-programming-basic.el ends here
