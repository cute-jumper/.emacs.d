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

;; no-littering!
(require 'no-littering nil :noerror)

;; User information
(setq user-full-name "Junpeng Qiu")
(setq user-mail-address "qjpchmail@gmail.com")

;; Set the root for org files
(defvar qjp-document-dir (expand-file-name "~/Documents/Personal/")
  "Personal document base directory.")

(defvar qjp-bibtex-database-file (expand-file-name "~/texmf/bibtex/bib/refs.bib")
  "Bibtex database file.")

;; The default value is 0.78MB, which is kind of too small.
;; Making this larger could result in reducing the frequency of GC.
(setq gc-cons-threshold 50000000)

;; inhibit startup related
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)

;; Treat all custom theme safe
(setq custom-safe-themes t)

;; Custom file
(setq custom-file (expand-file-name "custom.el" qjp-base-dir))
(when (file-exists-p custom-file)
  (load custom-file))

;; Start server
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (or (server-running-p) (server-start))
            (add-hook 'server-switch-hook
                      (lambda ()
                        (when (current-local-map)
                          (use-local-map (copy-keymap (current-local-map))))
                        (when server-buffer-clients
                          (local-set-key (kbd "C-x k") 'server-edit))))))

(provide '01qjp-global-settings)
;;; 01qjp-global-settings.el ends here
