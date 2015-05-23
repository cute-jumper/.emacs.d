;;; 02qjp-global-defuns.el --- Define some useful functions globally

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

;; Filter function
(defun qjp-filter (condp lst)
  "Filter function from http://emacswiki.org/emacs/ElispCookbook#toc46"
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

;; Utility function for adding `load-path'
(defun qjp-load-path-add-subdirs (&optional base-directory)
  "Add all subdirectories to load path."
  (interactive)
  (let ((current-directory (file-name-as-directory
                            (or base-directory default-directory))))
    (mapc
     (lambda (subdir)
       (add-to-list 'load-path subdir))
     (qjp-filter
      (lambda (x)
        (and (file-directory-p x)
             (not (string-prefix-p
                   (concat current-directory ".") x))))
      (directory-files current-directory t)))))

;; Auto byte-compile
(defun qjp-byte-compile-current-buffer ()
  "`byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists."
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))

(add-hook 'after-save-hook 'qjp-byte-compile-current-buffer)

(defmacro qjp--flet (binding &rest body)
  "Temporarily override BINDING and execute BODY."
  (declare (indent 1))
  (let* ((name (car binding))
         (old (cl-gensym (symbol-name name))))
    `(let ((,old (symbol-function ',name)))
       (unwind-protect
           (progn
             (fset ',name (lambda ,@(cdr binding)))
             ,@body)
         (fset ',name ,old)))))

;; NOP function. Just show the cursor position!
(defvar qjp--nop-counter 0)
(defvar qjp--nop-functions
  '((lambda () (message "Buffer %s has %d lines."
                    (buffer-name)
                    (line-number-at-pos (point-max))))
    (lambda () (message "Emacs uptime: %s." (emacs-uptime)))))

(defun qjp-nop ()
  "Show one of the nop functions."
  (interactive)
  (let ((func-num (length qjp--nop-functions)))
    (funcall (nth (mod qjp--nop-counter func-num) qjp--nop-functions))
    (setq qjp--nop-counter (mod (1+ qjp--nop-counter) func-num))))

;; Maximize
(defun qjp-maximized ()
  (interactive)
  (if (eq window-system 'x)
      (progn
        (x-send-client-message
         nil 0 nil "_NET_WM_STATE" 32
         '(1 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
        (x-send-client-message
         nil 0 nil "_NET_WM_STATE" 32
         '(1 "_NET_WM_STATE_MAXIMIZED_VERT" 0)))
    (message "Only support X window.")))

;; Show profiling results
(defun qjp-show-startup-times ()
  (interactive)
  (let ((buf "*init-times*")
        total-time)
    (with-current-buffer (get-buffer-create buf)
      (erase-buffer)
      (org-mode)
      (insert "* init times\n")
      (cl-loop for (module-name . time-list) in (sort
                                                 qjp--init-time-alist
                                                 (lambda (x y)
                                                   (string< (car x) (car y))))
               do (insert "** " module-name "\n")
               (insert "| Name | Elapsed Time |\n")
               (insert "|------+--------------|\n")
               (setq total-time .0)
               (dolist (elem (nreverse time-list))
                 (insert (format "| %s | %.3f |\n" (car elem) (cdr elem)))
                 (setq total-time (+ total-time (cdr elem))))
               (insert (format "| Total Time | %.3f |\n\n" total-time))
               (save-excursion
                 (save-match-data
                   (goto-char (re-search-backward "^** " nil t))
                   (forward-line)
                   (org-cycle))))
      (delete-char -1))
    (switch-to-buffer buf)))

(provide '02qjp-global-defuns)
;;; 02qjp-global-defuns.el ends here
