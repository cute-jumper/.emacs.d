;;; qjp-defuns-edit.el --- Defuns for editting       -*- lexical-binding: t; -*-

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

;; fast forward/backward lines
(defun qjp-fast-forward-lines ()
  "Use `next-line' because I want the effect of visual movement"
  (interactive)
  (next-line 5))

(defun qjp-fast-backward-lines ()
  "Use `previous-line' because I want the effect of visual movement"
  (interactive)
  (previous-line 5))

;; C-a to be `back-to-indentation' first and then `beginning-of-line'
(defun qjp-back-to-indentation-or-beginning () 
  (interactive)
  (if (= (point) (progn (back-to-indentation) (point)))
      (beginning-of-line)))

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

;; open new line
(defun qjp-open-new-line (&optional prefix)
  "Open a new line just below or above the current line."
  (interactive "P")
  (if prefix (progn (beginning-of-line) (newline) (backward-char))
    (progn
      (end-of-line)
      (newline)
      (indent-according-to-mode))))

;; move line up and down
(defun qjp-move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun qjp-move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

;; Fix backward-up-list to understand quotes, see http://bit.ly/h7mdIL
(defun qjp-backward-up-sexp (arg)
  (interactive "p")
  (let ((ppss (syntax-ppss)))
    (cond ((elt ppss 3)
           (goto-char (elt ppss 8))
           (qjp-backward-up-sexp (1- arg)))
          ((backward-up-list arg)))))

;; I put this `require' here because in prelude it said...See following comments...
(require 'volatile-highlights)
(volatile-highlights-mode t)
(diminish 'volatile-highlights-mode)

;; note - this should be after volatile-highlights is required
;; new copy/cut
(defadvice kill-ring-save (before slickcopy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
           (line-end-position)))))

;; new command my-kill-to-word, which I think is very helpful
;; bind to C-\
(defun qjp-kill-to-word ()
  (interactive)
  (save-excursion
    (let ((beg (point)) (end (isearch-forward-word)))
      (if end
        (kill-region beg end)))))

(defun qjp-kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

;; Require package `misc' for `zap-up-to-char'
(require 'misc)

(provide 'qjp-defuns-edit)
;;; qjp-defuns-edit.el ends here
