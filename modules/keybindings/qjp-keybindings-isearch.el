;;; qjp-keybindings-isearch.el --- Define key bindings for isearch

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

;; key binding from starter kit bindings
;; occur easily inside isearch from starter kit bindings
(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))

(defun kill-isearch-match ()
  "Kill the current isearch match string and continue searching."
  (interactive)
  (kill-region isearch-other-end (point)))

(define-key isearch-mode-map [(control k)] 'kill-isearch-match)

(defun zap-to-isearch (rbeg rend)
  "Kill the region between the mark and the closest portion of
  the isearch match string. The behaviour is meant to be analogous
  to zap-to-char; let's call it zap-to-isearch. The deleted region
  does not include the isearch word. This is meant to be bound only
  in isearch mode.

  The point of this function is that oftentimes you want to delete
  some portion of text, one end of which happens to be an active
  isearch word. The observation to make is that if you use isearch
  a lot to move the cursor around (as you should, it is much more
  efficient than using the arrows), it happens a lot that you could
  just delete the active region between the mark and the point, not
  include the isearch word."
  (interactive "r")
  (when (not mark-active)
    (error "Mark is not active"))
  (let* ((isearch-bounds (list isearch-other-end (point)))
         (ismin (apply 'min isearch-bounds))
         (ismax (apply 'max isearch-bounds))
         )
    (if (< (mark) ismin)
        (kill-region (mark) ismin)
      (if (> (mark) ismax)
          (kill-region ismax (mark))
        (error "Internal error in isearch kill function.")))
    (isearch-exit)
    ))

(define-key isearch-mode-map [(meta z)] 'zap-to-isearch)

;; isearch in other window
(defun isearch-other-window ()
  (interactive)
  (save-selected-window
    (other-window 1)
    (isearch-forward)))

(global-set-key (kbd "M-o s") 'isearch-other-window)

;; isearch yank active region
(defun isearch-yank-region ()
  (interactive)
  (when (region-active-p)
      (isearch-yank-internal (lambda () (mark)))))

(define-key isearch-mode-map (kbd "C-p") 'isearch-yank-region)


(provide 'qjp-keybindings-isearch)
;;; qjp-keybindings-isearch.el ends here
