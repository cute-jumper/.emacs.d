;;; qjp-keybindings-edit.el --- Define key bindings for editing

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

;; fast forward/backward lines
(defun qjp-fast-forward-lines ()
  (interactive)
  (forward-line 5))

(defun qjp-fast-backward-lines ()
  (interactive)
  (forward-lines -5))

(global-set-key (kbd "M-n") 'fast-forward-lines)
(global-set-key (kbd "M-p") 'fast-backward-lines)

;; C-a to be `back-to-indentation' first and then `beginning-of-line'
(defun qjp-back-to-indentation-or-beginning () 
  (interactive)
  (if (= (point) (progn (back-to-indentation) (point)))
      (beginning-of-line)))

(global-set-key (kbd "C-a") 'qjp-back-to-indentation-or-beginning)

;; new comment command
(defun qjp-comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
If no region is selected and current line is not blank and we are not at the end of the line,
then comment current line.
Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

(global-set-key (kbd "M-;") 'qjp-comment-dwim-line)


;; open new line
(defun my-open-new-line (&optional prefix)
  "Open a new line just below or above the current line."
  (interactive "P")
  (if prefix (progn (beginning-of-line) (newline) (backward-char))
    (progn
      (end-of-line)
      (newline)
      (indent-according-to-mode))))

(global-set-key (kbd "C-o") 'my-open-new-line)

(global-set-key (kbd "C-c e") 'qjp-esk-eval-and-replace)

;; move line up and down
(global-set-key [M-up]
                (lambda ()
                  (interactive)
                  (transpose-lines 1)
                  (forward-line -2)
                  (indent-according-to-mode)))

(global-set-key [M-down]
                (lambda ()
                  (interactive)
                  (forward-line 1)
                  (transpose-lines 1)
                  (forward-line -1)
                  (indent-according-to-mode)))

;; Fix backward-up-list to understand quotes, see http://bit.ly/h7mdIL
(defun backward-up-sexp (arg)
  (interactive "p")
  (let ((ppss (syntax-ppss)))
    (cond ((elt ppss 3)
           (goto-char (elt ppss 8))
           (backward-up-sexp (1- arg)))
          ((backward-up-list arg)))))

(global-set-key [remap backward-up-list] 'backward-up-sexp)

;; new copy/cut
(defadvice kill-ring-save (before slickcopy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defadvice kill-region (before slickcut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

;; new command my-kill-to-word, which I think is very helpful
;; bind to C-\
(defun qjp-kill-to-word ()
  (interactive)
  (save-excursion
    (let ((beg (point)) (end (isearch-forward-word)))
      (if end
        (kill-region beg end)))))
(global-set-key (kbd "C-\\") 'qjp-kill-to-word)

(defun qjp-kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

(global-set-key (kbd "M-m") 'qjp-kill-back-to-indentation)

;; misc package
(require 'misc)
(global-set-key (kbd "M-Z") 'zap-up-to-char)

(provide 'qjp-keybindings-edit)
;;; qjp-keybindings-edit.el ends here
