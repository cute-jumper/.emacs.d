;;; qjp-misc-dired.el --- Settings for dired related modes

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

(require 'dired-x)
;; Set files to be ommited
(setq dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..*")

(defadvice dired-find-file (around dired-find-file-single-buffer activate)
  "Replace current buffer if file is a directory."
  (interactive)
  (let ((orig (current-buffer))
        (filename (dired-get-file-for-visit)))
    ad-do-it
    (when (and (file-directory-p filename)
               (not (eq (current-buffer) orig)))
      (kill-buffer orig))))

(defadvice dired-up-directory (around dired-up-directory-single-buffer activate)
  "Replace current buffer if file is a directory."
  (interactive)
  (let ((orig (current-buffer)))
    ad-do-it
    (kill-buffer orig)))

;; Original name -- sof/dired-sort
(defun qjp-dired-sort ()
  "Dired sort hook to list directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max))))
  (and (featurep 'xemacs)
       (fboundp 'dired-insert-set-properties)
       (dired-insert-set-properties (point-min) (point-max)))
  (set-buffer-modified-p nil))

(defun dired-do-eval (form)
  "Evaluate FORM in each of the buffers."
  (interactive (list
                (read-from-minibuffer
                 "Eval in files (form): "
                 nil
                 read-expression-map
                 t
                 'read-expression-history)))
  (dired-map-over-marks
   (let* ((filename (dired-get-file-for-visit))
          (buf (find-file-noselect filename))
          (kill-buffer-query-functions nil))
     (unwind-protect
         (with-current-buffer buf
           (eval form lexical-binding)
           (save-buffer))
       (kill-buffer buf)))
   nil))
(define-key dired-mode-map "E" #'dired-do-eval)

(add-hook 'dired-after-readin-hook 'qjp-dired-sort)

(defun qjp-dired-mode-hook ()
  (setq dired-guess-shell-alist-user
        ;; fixed rule
        (list (list "\\.pdf$" "okular")))
  (guide-key/add-local-guide-key-sequence "%"))
(add-hook 'dired-mode-hook #'qjp-dired-mode-hook)

(provide 'qjp-misc-dired)
;;; qjp-misc-dired.el ends here
