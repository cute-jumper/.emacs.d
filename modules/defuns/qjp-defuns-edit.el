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

;; In prelude:
;; - `prelude-annotate-todo'
;; - `prelude-duplicate-and-comment-current-line-or-region'
;; - Other fancy inserting commands

;;; Code:

;; ----------- ;;
;; Fast moving ;;
;; ----------- ;;
(defvar qjp-fast-moving-line-number 5
  "How many line should fast moving command should use.")

;; fast forward/backward lines
(defun qjp-fast-forward-lines ()
  "Use `next-line' because I want the effect of visual movement."
  (interactive)
  (next-line qjp-fast-moving-line-number))

(defun qjp-fast-backward-lines ()
  "Use `previous-line' because I want the effect of visual movement."
  (interactive)
  (previous-line qjp-fast-moving-line-number))

;; ------------------- ;;
;; Move line up & down ;;
;; ------------------- ;;
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

;; --------------------------------- ;;
;; Moving and killing by indentation ;;
;; --------------------------------- ;;
(defun qjp-back-to-indentation-or-beginning ()
  "First `back-to-indentation' and then `beginning-of-line'."
  (interactive)
  (when (= (point)
           (progn
             (back-to-indentation)
             (point)))
    (beginning-of-line)))

(defun qjp-kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

;; ----------------------- ;;
;; Indentation-aware paste ;;
;; ----------------------- ;;
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

;; ----------------------- ;;
;; Vim-style open new line ;;
;; ----------------------- ;;
(defun qjp-open-new-line (&optional prefix)
  "Open a new line just below or above the current line."
  (interactive "P")
  (if prefix
      (progn
        (beginning-of-line)
        (newline-and-indent)
        (forward-line -1)
        (indent-according-to-mode))
    (end-of-line)
    (newline-and-indent)))

;; ----------------- ;;
;; Quickly duplicate ;;
;; ----------------- ;;
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

;; ------------------------------- ;;
;; The real "dwim" comment command ;;
;; ------------------------------- ;;
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

;; ---------------- ;;
;; Eval and replace ;;
;; ---------------- ;;
;; Elisp eval and replace
(defun qjp-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

;; Eval and replace using calc
(defun qjp-calc-eval-and-replace (&optional prefix start end)
  (interactive "P\nr")
  (let ((result (calc-eval (buffer-substring-no-properties start end))))
    (when prefix
      (kill-region start end))
    (insert result)))

;; --------- ;;
;; sudo edit ;;
;; --------- ;;
(defun qjp-sudo-edit (&optional arg)
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; -------------------------- ;;
;; Enable paredit for nonlisp ;;
;; -------------------------- ;;
(defun qjp-paredit-nonlisp ()
  "Turn on paredit mode for non-lisps."
  (interactive)
  (set (make-local-variable 'paredit-space-for-delimiter-predicates)
       '((lambda (endp delimiter) nil)))
  (paredit-mode 1))

;; --------------------- ;;
;; Insert various things ;;
;; --------------------- ;;
(defun qjp-lorem ()
  "Insert a lorem ipsum."
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do "
          "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim"
          "ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut "
          "aliquip ex ea commodo consequat. Duis aute irure dolor in "
          "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
          "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in "
          "culpa qui officia deserunt mollit anim id est laborum."))

;; TODO: Generalize it!
(defun qjp-suck-it (suckee)
  "Insert a comment of appropriate length about what can suck it."
  (interactive "MWhat can suck it? ")
  (let ((prefix (concat ";; " suckee " can s"))
        (postfix "ck it!")
        (col (current-column)))
    (insert prefix)
    (dotimes (_ (- 80 col (length prefix) (length postfix))) (insert "u"))
    (insert postfix)))

(defun qjp-insert-date ()
  "Insert a time-stamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%c" (current-time))))

;; -------------- ;;
;; Buffer cleanup ;;
;; -------------- ;;
(defun qjp-untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun qjp-indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun qjp-cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (qjp-indent-buffer)
  (qjp-untabify-buffer)
  (whitespace-cleanup))

;; ------------------------ ;;
;; File & buffer operations ;;
;; ------------------------ ;;
(defun qjp-rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (if (get-buffer new-name)
        (message "A buffer named '%s' already exists!" new-name)
      (progn
        (when (file-exists-p filename)
          (rename-file filename new-name 1))
        (rename-buffer new-name)
        (set-visited-file-name new-name)))))

(defun qjp-delete-this-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (when (y-or-n-p (format "Are you sure you want to delete %s?" filename))
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

;; autoload `zap-up-to-char' in `misc'
;; obsolete. Use `ace-jump-zap' instead
;; (autoload 'zap-up-to-char "misc")

(provide 'qjp-defuns-edit)
;;; qjp-defuns-edit.el ends here
