;;; qjp-misc-avy.el --- Settings for avy             -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Junpeng Qiu

;; Author: Junpeng Qiu <qjpchmail@gmail.com>
;; Keywords:

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

(defun qjp-avy-action-copy-and-yank (pt)
  "Copy and yank sexp starting on PT."
  (avy-action-copy pt)
  (yank))

(setq avy-background t)
(setq avy-dispatch-alist '((?c . avy-action-copy)
                           (?k . avy-action-kill-move)
                           (?K . avy-action-kill-stay)
                           (?m . avy-action-mark)
                           (?p . qjp-avy-action-copy-and-yank)))

(let* ((all-keys (number-sequence ?a ?z))
       (keys (qjp-filter (lambda (x) (not
                                  (member
                                   x
                                   (mapcar (lambda (y) (car y)) avy-dispatch-alist))))
                         all-keys)))
  (setq avy-keys-alist `((avy-goto-word-0 . ,keys)
                         (avy-goto-word-1 . ,keys)
                         (avy-copy-line . ,keys)
                         (avy-move-line . ,keys))))

(defun avy-goto-word-0-in-line (arg)
  "Jump to a word start in the current line."
  (interactive "P")
  (avy-with avy-goto-word-0
    (avy--generic-jump "\\b\\sw" arg avy-style (line-beginning-position)
                       (line-end-position))))

(with-eval-after-load 'qjp-mode
  ;; convenient keychords
  (qjp-key-chord-define qjp-mode-map "jk" #'avy-goto-word-1)
  (qjp-key-chord-define qjp-mode-map "jl" #'avy-goto-word-0-in-line)
  ;; all avy commands
  (define-prefix-command 'ctrl-c-avy-map)
  (define-key qjp-mode-map (kbd "C-c a") 'ctrl-c-avy-map)
  (define-key ctrl-c-avy-map "c" #'avy-goto-char)
  (define-key ctrl-c-avy-map "C" #'avy-goto-char-2)
  (define-key ctrl-c-avy-map "w" #'avy-goto-word-1)
  (define-key ctrl-c-avy-map "W" #'avy-goto-word-2)
  (define-key ctrl-c-avy-map "y" #'avy-copy-line)
  (define-key ctrl-c-avy-map "m" #'avy-move-line)
  (define-key ctrl-c-avy-map "l" #'avy-goto-line)
  (define-key ctrl-c-avy-map "`" #'avy-pop-mark)
  (define-key isearch-mode-map (kbd "C-'") 'avy-isearch))

(provide 'qjp-misc-avy)
;;; qjp-misc-avy.el ends here
