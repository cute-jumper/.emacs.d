;;; qjp-basic.el --- The VERY basic settings

;; Copyright (C) 2014  Junpeng Qiu

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

;; ------- ;;
;; UI part ;;
;; ------- ;;

;; Customize the looking! From up to down!
;; Set title first
(setq frame-title-format
      '((:eval
         (concat (if (buffer-file-name)
                     (abbreviate-file-name (buffer-file-name))
                   "%b")
                 "@"
                 (system-name)))))

;; Show menu bar if `ubuntu', otherwise disable it.
(let ((desktop-env (getenv "DESKTOP_SESSION")))
  (if (string= desktop-env "ubuntu")
      (menu-bar-mode +1)
    (menu-bar-mode -1)))

;; Disable toolbar if not yet
(tool-bar-mode -1)

;; Set cursor type
(setq-default cursor-type 'bar)

;; Set `fill-column' to wrap at 80
(setq-default fill-column 80)

;; Disable blinking cursor
(blink-cursor-mode -1)

;; Nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; For mode line
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)
(display-battery-mode)

;; Draw underline lower
(setq x-underline-at-descent-line t)

;; Show me empty lines
(setq-default indicate-empty-lines t)

;; Show keystrokes in minibuffer
(setq echo-keystrokes 0.1)

;; Show visible bell
(setq visible-bell t)

;; Now, let's set the fonts for English and Chinese
(when (display-graphic-p)
  (set-frame-font "Liberation Mono:pixelsize=22")
  (setq default-frame-alist
        '((font . "Liberation Mono:pixelsize=22")
          (cursor-color . "white")))
  (set-fontset-font (frame-parameter nil 'font) 'han
                    (font-spec :family "WenQuanYi Micro Hei Mono" :size 26))
  (set-fontset-font "fontset-default" 'unicode "Symbola"))

;; Finally, use `zenburn' as the default theme. It's a really cool theme!
(load-theme 'zenburn t)
;; Minor fix for volitile-highlight
(custom-theme-set-faces
 'zenburn
 '(vhl/default-face ((t (:foreground "#E0CF9F" :background "#383838")))))

;; -------------------- ;;
;; Some useful settings ;;
;; -------------------- ;;

;; Use y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; Single space ends sentences
(setq sentence-end-double-space nil)

;; Require new at the end of a file
(setq require-final-newline t)

;; Don't use shift to select
(setq shift-select-mode nil)

;; Don't use tabs pleeeeeeeeese!
(setq-default indent-tabs-mode nil)

;; use 'complete when auto-complete is disabled
(setq tab-always-indent 'complete)

;; auto save and backups
(setq
 ;; Put auto save file to temporary file directory
 auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
 ;; Backups
 backup-directory-alist `((".*" . ,temporary-file-directory))
 ;; Tramp backups
 tramp-backup-directory-alist backup-directory-alist)

;; ---------------------- ;;
;; Built-in functionality ;;
;; ---------------------- ;;

;; show-paren-mode
(show-paren-mode +1)
(setq show-paren-style 'mixed)

;; uniquify
(setq uniquify-buffer-name-style 'forward
      uniquify-ignore-buffers-re "^\\*")

;; winner-mode for window configurations
(winner-mode +1)

;; recentf settings
;; ignore magit's commit message files
(add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")

;; ediff in same frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; whitespace-mode
(setq
 whitespace-line-column 80
 whitespace-style '(face tabs empty trailing lines-tail))
(global-whitespace-mode)

;; imenu
(setq-default imenu-auto-rescan t)

;; grep
(with-eval-after-load 'grep
  (add-to-list 'grep-find-ignored-files "*.class"))

;; hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

;; ----------- ;;
;; Basic hooks ;;
;; ----------- ;;
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; -------------- ;;
;; Remove warning ;;
;; -------------- ;;

;; Narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
;; Change region case commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
;; Erase-buffer command
(put 'erase-buffer 'disabled nil)

(provide 'qjp-basic)
;;; qjp-basic.el ends here
