;;; qjp-org-misc.el --- Extra settings for org-mode

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

;; ------------ ;;
;; org protocol ;;
;; ------------ ;;
(require 'org-protocol)

;; --------- ;;
;; org crypt ;;
;; --------- ;;
(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
(setq org-crypt-key nil)

;; ---------- ;;
;; org-caldav ;;
;; ---------- ;;
(require 'url-dav)
(require 'org-caldav)
(setq org-caldav-url "https://www.google.com/calendar/dav")
(setq org-caldav-calendar-id "qjpchmail@gmail.com")
(setq org-caldav-files 
      (mapcar (lambda (x)
                (concat qjp-document-dir x))
              '("Agenda/homework.org" "Agenda/tasks.org")))
(setq org-caldav-inbox (concat qjp-document-dir "Agenda/GoogleCalendar.org"))

(provide 'qjp-org-misc)
;;; qjp-org-misc.el ends here
