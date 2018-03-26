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

(autoload 'dired-jump "dired-x" nil t)
(define-key qjp-mode-map (kbd "C-x C-j") #'dired-jump)

(with-eval-after-load 'dired
  (require 'dired-x)
  (diredfl-global-mode +1)
  ;; Set files to be ommited
  (setq dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..*")
  ;; Hunman-readable space
  (setq dired-listing-switches "-alh")
  ;; intelligent dired
  (setq dired-dwim-target t)
  ;; always recursive
  (setq dired-recursive-copies 'always)

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

  (defun qjp--human-readable-space (ret)
    (let ((kb (string-to-number ret)))
      (if (> kb 1024)
          (if (> (setq kb (/ kb 1024)) 1024)
              (format "%.0f GB" (/ kb 1024))
            (format "%.0f MB" kb))
        (format "%.0f Kb" kb))))

  (defun qjp-insert-directory (orig-func &rest args)
    "modify the total/available number by dividing it by 1024"
    (advice-add 'get-free-disk-space :filter-return 'qjp--human-readable-space)
    (prog1 (apply orig-func args)
      (advice-remove 'get-free-disk-space 'qjp--human-readable-space)))

  (advice-add 'insert-directory :around 'qjp-insert-directory)

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

  (defun qjp-dired-goto-file-and-open ()
    (interactive)
    (call-interactively 'dired-goto-file)
    (dired-find-file))
  (define-key dired-mode-map "e" #'qjp-dired-goto-file-and-open)

  ;; No sort now
  ;; (add-hook 'dired-after-readin-hook 'qjp-dired-sort)

  (define-key dired-mode-map (kbd "h") #'dired-up-directory)
  (define-key dired-mode-map (kbd "M-p") #'dired-prev-marked-file)
  (define-key dired-mode-map (kbd "M-n") #'dired-next-marked-file)

  ;; ---------------- ;;
  ;; dired extensions ;;
  ;; ---------------- ;;
  ;; dired-narrow
  (define-key dired-mode-map (kbd "/") #'dired-narrow-regexp)
  ;; dired-subtree
  (define-key dired-mode-map (kbd "<tab>") #'dired-subtree-toggle)
  (define-key dired-mode-map (kbd "<backtab>") #'dired-subtree-cycle)
  ;; async
  (dired-async-mode +1)

  (defun qjp-dired-mode-hook ()
    (setq dired-guess-shell-alist-user
          ;; fixed rule
          (list (list "\\.pdf$" "okular")))
    ;; No stripe-buffer-mode
    ;;(stripe-listify-buffer)
    )
  (add-hook 'dired-mode-hook #'qjp-dired-mode-hook))

(provide 'qjp-misc-dired)
;;; qjp-misc-dired.el ends here
