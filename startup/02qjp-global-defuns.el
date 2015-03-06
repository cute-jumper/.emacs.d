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
(defun qjp-add-subdirectories-to-load-path (base-directory)
  "Add all subdirectories to load path."
  (interactive)
  (mapc
   (lambda (subdir)
     (add-to-list 'load-path subdir))
   (qjp-filter
    (lambda (x)
      (and (file-directory-p x) (not (string-prefix-p "\\." x))))
    (directory-files base-directory t))))

;; Require sub-directory when I don't want to put all sub-directories in
;; `load-path'
(defun qjp-require-subdir-feature (current-dir subdir feature)
  (require
   (intern feature)
   (expand-file-name
    (concat
     (file-name-as-directory current-dir)
     (file-name-as-directory subdir) feature))))

(defalias 'qjp-modules-require-subdir-feature (apply-partially 'qjp-require-subdir-feature qjp-modules-dir))

;; Auto byte-compile
(defun qjp-byte-compile-current-buffer ()
  "`byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists."
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))

(add-hook 'after-save-hook 'qjp-byte-compile-current-buffer)

;; NOP function. Just show the cursor position!
(defun qjp-NOP ()
  "Don't use `what-line' because it would output a message in minibuffer"
  (interactive)
  (message "Cursor at (%d, %d)" (line-number-at-pos) (current-column)))

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

(provide '02qjp-global-defuns)
;;; 02qjp-global-defuns.el ends here
