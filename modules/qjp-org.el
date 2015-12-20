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

(add-to-list 'load-path (concat qjp-modules-dir "/org"))

;; Quick way to open Personal directory in Dired
(defun qjp-open-personal ()
  (interactive)
  (dired qjp-document-dir))

;; ----------------- ;;
;; Basic key binding ;;
;; ----------------- ;;
(with-eval-after-load 'qjp-mode
  (define-key qjp-mode-map (kbd "C-c l") 'org-store-link)
  (define-key qjp-mode-map (kbd "C-c c") 'org-capture)
  (define-key qjp-mode-map (kbd "C-c a") 'org-agenda)
  (define-key qjp-mode-map (kbd "C-c b") 'org-iswitchb))

;; ------------- ;;
;; Org-mode hook ;;
;; ------------- ;;
(defun qjp-org-mode-hook ()
  ;; CDLaTeX support
  (turn-on-org-cdlatex)
  ;; highlight current line
  (hl-line-mode +1)
  (turn-on-stripe-table-mode))

(add-hook 'org-mode-hook #'qjp-org-mode-hook)

(with-eval-after-load 'org
  (require 'qjp-org-edit)
  (require 'qjp-org-src)
  (require 'qjp-org-export)
  (require 'qjp-org-misc)
  (require 'qjp-org-notes))

(require 'qjp-org-publish)

(provide 'qjp-org)
;;; qjp-org.el ends here
