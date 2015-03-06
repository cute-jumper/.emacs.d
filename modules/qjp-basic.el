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

;; ---------------------------------------------- ;;
;; UI part. Some code borrowed from prelude-ui.el ;;
;; ---------------------------------------------- ;;

;; Customize the looking! From up to down!
;; Set title first
(setq frame-title-format (format "%%b@%s:%%f" (system-name)))

;; Show menu bar if `ubuntu', otherwise disable it.
(let ((desktop-env (getenv "DESKTOP_SESSION")))
  (cond ((string= desktop-env "ubuntu") (menu-bar-mode))
        (t (menu-bar-mode -1))))

;; Disable toolbar if not yet
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; Disable splash screen
(setq inhibit-startup-screen t)

;; Set `fill-column' to wrap at 80
(setq-default fill-column 80)

;; Disable blinking cursor
(blink-cursor-mode -1)

;; Enable tooltip
(tooltip-mode)

;; Enable mouse wheel support
(mouse-wheel-mode t)

;; Nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; Fringe settings. Reduce its size from 8 to 4 pixels.
(when (fboundp 'fringe-mode)
  (fringe-mode 4))

;; For mode line
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)
(which-function-mode)
(display-battery-mode)

;; Show keystrokes in minibuffer
(setq echo-keystrokes 0.1)

;; Use y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; Now, let's set the fonts for English and Chinese
(set-frame-font "Liberation Mono:pixelsize=22")
(setq default-frame-alist
      '((font . "Liberation Mono:pixelsize=22")
	(cursor-color . "white")))
(if (display-graphic-p)
    (set-fontset-font (frame-parameter nil 'font) 'han
                       (font-spec :family "WenQuanYi Micro Hei Mono" :size 26)))

;; Put auto save file to temporary file directory
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Finally, use `zenburn' as the default theme. It's a really cool theme!
(load-theme 'zenburn t)

;; -------------------------- ;;
;; Some useful built-in modes ;;
;; -------------------------- ;;

;; show-paren-mode
(show-paren-mode)
(setq show-paren-style 'mixed)

;; ---------------------------------- ;;
;; Some useful functions from prelude ;;
;; ---------------------------------- ;;

(qjp-modules-require-subdir-feature "prelude" "qjp-basic-prelude")

;; ----------------------------- ;;
;; Put Unused configuration here ;;
;; ----------------------------- ;;

;; electric-pair-mode --> smartparens
; (electric-pair-mode)

;; ibuffer --> helm
; (setq ibuffer-use-other-window t)
; (global-set-key (kbd "C-x C-b") 'ibuffer)

(provide 'qjp-basic)
;;; qjp-basic.el ends here
