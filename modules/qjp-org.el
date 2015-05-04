;;; qjp-org.el --- Settings for org-mode

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

;; Quick way to open root directory in Dired
(defun org-open-personal ()
  (interactive)
  (dired qjp-document-dir))

;; ----------------- ;;
;; Basic key binding ;;
;; ----------------- ;;
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(with-eval-after-load 'org
  (qjp-modules-require-subdir-feature "org" "qjp-org-edit")
  (qjp-modules-require-subdir-feature "org" "qjp-org-src")
  (qjp-modules-require-subdir-feature "org" "qjp-org-export")
  (qjp-modules-require-subdir-feature "org" "qjp-org-misc")
  (qjp-modules-require-subdir-feature "org" "qjp-org-notes")
  (qjp-modules-require-subdir-feature "org" "qjp-org-publish"))

(provide 'qjp-org)
;;; qjp-org.el ends here
