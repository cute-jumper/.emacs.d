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

;; From http://stackoverflow.com/questions/88399/how-do-i-duplicate-a-whole-line-in-emacs
(defun qjp-duplicate-line-or-region (&optional n)
  "Duplicate current line, or region if active.
With argument N, make N copies.
With negative N, comment out original line and use the absolute value."
  (interactive "*p")
  (let ((use-region (use-region-p)))
    (save-excursion
      (let ((text (if use-region        ;Get region if active, otherwise line
                      (buffer-substring (region-beginning) (region-end))
                    (prog1 (thing-at-point 'line)
                      (end-of-line)
                      (if (< 0 (forward-line 1)) ;Go to beginning of next line, or make a new one
                          (newline))))))
        (dotimes (i (abs (or n 1)))     ;Insert N times, or once if not specified
          (insert text))))
    (if use-region nil                  ;Only if we're working with a line (not a region)
      (let ((pos (- (point) (line-beginning-position)))) ;Save column
        (if (> 0 n)                             ;Comment out original with negative arg
            (comment-region (line-beginning-position) (line-end-position)))
        (forward-line 1)
        (forward-char pos)))))

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

(defun qjp-kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

(defvar qjp-yank-indent-modes
  '(LaTeX-mode TeX-mode)
  "Modes in which to indent regions that are yanked (or yank-popped).
Only modes that don't derive from `prog-mode' should be listed here.")

(defvar qjp-yank-indent-blacklisted-modes
  '(python-mode slim-mode haml-mode)
  "Modes for which auto-indenting is suppressed.")

(defvar qjp-yank-advised-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur.")

(defun qjp-yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) qjp-yank-advised-indent-threshold)
      (indent-region beg end nil)))

(defadvice yank (after yank-indent activate)
  "If current mode is one of 'qjp-yank-indent-modes,
indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (not (member major-mode qjp-yank-indent-blacklisted-modes))
           (or (derived-mode-p 'prog-mode)
               (member major-mode qjp-yank-indent-modes)))
      (let ((transient-mark-mode nil))
    (qjp-yank-advised-indent-function (region-beginning) (region-end)))))

(defadvice yank-pop (after yank-pop-indent activate)
  "If current mode is one of `qjp-yank-indent-modes',
indent yanked text (with prefix arg don't indent)."
  (when (and (not (ad-get-arg 0))
             (not (member major-mode qjp-yank-indent-blacklisted-modes))
             (or (derived-mode-p 'prog-mode)
                 (member major-mode qjp-yank-indent-modes)))
    (let ((transient-mark-mode nil))
      (qjp-yank-advised-indent-function (region-beginning) (region-end)))))

;; autoload `zap-up-to-char' in `misc'
;; obsolete. Use `ace-jump-zap' instead
;; (autoload 'zap-up-to-char "misc")

(provide 'qjp-defuns-edit)
;;; qjp-defuns-edit.el ends here
