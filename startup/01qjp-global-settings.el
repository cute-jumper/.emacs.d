;;; 01qjp-global-settings.el --- Miscellaneous global settings stuffs

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

;; User information
(setq user-full-name "Junpeng Qiu")
(setq user-mail-address "qjpchmail@gmail.com")

;; Directories for modules and other configuration
(defvar qjp-modules-dir (expand-file-name "modules" qjp-base-dir)
  "The directory to place configuration for various modules.")
(defvar qjp-site-lisp-dir (expand-file-name "site-lisp" qjp-base-dir)
  "The directory to hold personal packages.")

(defvar qjp-document-dir (expand-file-name "~/Documents/Personal/")
  "Personal document base directory.")

;; The default value is 0.78MB, which is kind of too small.
;; Making this larger could result in reducing the frequency of GC.
(setq gc-cons-threshold 50000000)

;; Show keystrokes
(setq echo-keystrokes 0.1)

;; Set `fill-column' to wrap
(setq-default fill-column 80)

;; Customize the looking!
;; Set title
(setq frame-title-format (format "%%b@%s:%%f" (system-name)))
;; Show menu bar if `ubuntu'
(let ((desktop-env (getenv "DESKTOP_SESSION")))
  (cond ((string= desktop-env "ubuntu") (menu-bar-mode))
        (t (menu-bar-mode -1))))
;; Font settings
(set-frame-font "monaco:pixelsize=14")
(if (display-graphic-p)
    (set-fontset-font (frame-parameter nil 'font) 'han
                  (font-spec :family "Microsoft Yahei" :size 16)))

;; Start server
(require 'server)
(unless (server-running-p)
  (server-start))
(add-hook 'server-switch-hook
          (lambda ()
            (when (current-local-map)
              (use-local-map (copy-keymap (current-local-map))))
            (when server-buffer-clients
              (local-set-key (kbd "C-x k") 'server-edit))))

(provide '01qjp-global-settings)
;;; 01qjp-global-settings.el ends here
