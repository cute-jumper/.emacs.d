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

;; Disable menu-bar
(menu-bar-mode -1)

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
(display-battery-mode)

;; Draw underline lower
(setq x-underline-at-descent-line t)

;; Show me empty lines
(setq-default indicate-empty-lines t)

;; Show keystrokes in the echo area
(setq echo-keystrokes 0.1)

;; Show visible bell
(setq visible-bell t)

;; save abbrevs automatically
(setq save-abbrevs t)

;; Now, let's set the fonts for English and Chinese
(when (display-graphic-p)
  (let (en-font zh-font zh-size unicode-font)
    (when (cond ((eq system-type 'gnu/linux)
                 (setq en-font "Consolas:pixelsize=22")
                 (setq zh-font "WenQuanYi Micro Hei Mono")
                 (setq zh-size 26)
                 (setq unicode-font "Symbola"))
                ((eq system-type 'windows-nt)
                 (setq en-font "Consolas:pixelsize=16")
                 (setq zh-font "Microsoft YaHei")
                 (setq zh-size 18)
                 (setq unicode-font "Arial Unicode MS")))
      (set-frame-font en-font)
      (setq default-frame-alist `((font . ,en-font)))
      (set-fontset-font
       (frame-parameter nil 'font)
       'han
       (font-spec :family zh-font :size zh-size))
      (set-fontset-font "fontset-default" 'unicode unicode-font))))

;; Temporarily, try out another theme other than `zenburn'
(defvar qjp-dark-theme nil)
(defvar qjp-light-theme nil)
(defvar qjp-cursor-color nil)
;; ------------- ;;
;; Change themes ;;
;; ------------- ;;
;; TODO: generalize it maybe!
(defun qjp-switch-theme (theme)
  (interactive (list (completing-read
                      "Choose a theme set (dark or light): "
                      '(dark light))))
  (cond
   ((string= theme "dark")
    (when qjp-light-theme
      (disable-theme qjp-light-theme))
    (when qjp-dark-theme
      (load-theme qjp-dark-theme t))
    (when (fboundp 'sml/apply-theme)
      (sml/apply-theme 'dark))
    (setq qjp-cursor-color (frame-parameter nil 'cursor-color)))
   ((string= theme "light")
    (when qjp-dark-theme
      (disable-theme qjp-dark-theme))
    (when qjp-light-theme
      (load-theme qjp-light-theme t))
    (when (fboundp 'sml/apply-theme)
      (sml/apply-theme 'light))
    (setq qjp-cursor-color (frame-parameter nil 'cursor-color)))))
(setq qjp-dark-theme 'moe-dark)
(setq qjp-light-theme 'moe-light)
(qjp-switch-theme 'dark)
;; Minor fix for volitile-highlight
;; (custom-theme-set-faces
;;  'zenburn
;;  '(vhl/default-face ((t (:foreground "#383838" :background "gray50")))))

;; -------------------- ;;
;; Some useful settings ;;
;; -------------------- ;;

;; Use y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; Single space ends sentences
(setq sentence-end-double-space nil)

;; Require new at the end of a file
(setq require-final-newline t)

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

;; Easier to pop up mark
(setq set-mark-command-repeat-pop t)

;; ---------------------- ;;
;; Built-in functionality ;;
;; ---------------------- ;;

;; show-paren-mode
(show-paren-mode +1)
;; (set-face-attribute 'show-paren-match nil
;;                     :foreground "yellow")

;; uniquify
(setq uniquify-buffer-name-style 'forward
      uniquify-ignore-buffers-re "^\\*")

;; winner-mode for window configurations
(winner-mode +1)

;; `recursive-edit' depth
(minibuffer-depth-indicate-mode +1)

;; recentf settings
;; ignore magit's commit message files
(setq recentf-save-file (expand-file-name "recentf" qjp-base-dir))
(with-eval-after-load 'recentf
  (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

;; ediff in same frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; save point postiion in file
(if (version< emacs-version "25.1")
    (setq save-place +1)
  (save-place-mode +1))

;; save history
(savehist-mode +1)

;; save desktop
(defun qjp-desktop-save-to-home ()
  (interactive)
  (require 'cl-lib)
  (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
    (desktop-save qjp-base-dir)))
(add-hook 'kill-emacs-hook 'qjp-desktop-save-to-home)

;; whitespace-mode
(setq
 whitespace-line-column 80
 whitespace-style '(face tabs empty trailing lines-tail))
(global-whitespace-mode)
;; Disable whitespace-mode in some modes
(defun qjp-turn-off-whitespace-mode ()
  (whitespace-mode -1))
(dolist (hook '(special-mode-hook
                Info-mode-hook
                eww-mode-hook
                term-mode-hook
                comint-mode-hook
                compilation-mode-hook
                minibuffer-setup-hook))
  (add-hook hook #'qjp-turn-off-whitespace-mode))

;; imenu
(setq-default imenu-auto-rescan t)

;; use Emacs own tooltip since system tooltip only supports text
(setq x-gtk-use-system-tooltips nil)

;; grep
(with-eval-after-load 'grep
  (add-to-list 'grep-find-ignored-files "*.class"))

;; no scroll bar
(scroll-bar-mode -1)

;; scroll margin to 10
(setq scroll-margin 10)

;; hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list '(try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line))

;; ----------- ;;
;; Basic hooks ;;
;; ----------- ;;
(defun qjp-text-mode-hook ()
  (turn-on-auto-fill)
  (abbrev-mode +1))
(add-hook 'text-mode-hook #'qjp-text-mode-hook)

;; ------------------------ ;;
;; Fix PATH and `exec-path' ;;
;; ------------------------ ;;
(defun qjp-fix-path-and-exec-path ()
  (when (eq system-type 'gnu/linux)
    (let ((paths (shell-command-to-string
                  (format "%s -c 'echo -n $PATH'" (getenv "SHELL")))))
      (setq exec-path (split-string paths ":")))))
(qjp-fix-path-and-exec-path)

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
