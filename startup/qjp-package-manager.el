;;; qjp-elpa-settings.el --- Settings for package management

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

;; Package sources
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")))

;; Update installed package list and install them at first initialization,
;; Besides, they can be used to sync the newly installed packages.
;; Idea from: http://blub.co.za/posts/Installing-packages-on-Emacs-startup.html
(defun qjp-get-package-list-filename () 
  "return the filename containing the installed packages' list"
  (expand-file-name "installed-package-list" qjp-base-dir))

(defun qjp-install-all-packages ()
  "Use this command to install all the packages.
It will read from the file and get the installed packages's list, 
then it will install these packages one by one."
  (defun read-installed-package-list ()
    "Read the installed packages' list"
    (read
     (with-current-buffer
         (find-file-noselect
          (get-package-list-filename))
       (buffer-substring-no-properties (point-min) (point-max)))))
  (interactive)
  (package-refresh-contents)
  (let ((installed-packages (read-installed-package-list)))
    (dolist (pkg installed-packages)
      (if (not (package-installed-p pkg))
          (ignore-errors
            (progn
              (package-install pkg)
              (message "Installing %s done." pkg)))
        (message "Package %s already installed!" pkg)))))

;; This function should be used when maintaining the settings, not at the first initialization.
(defun qjp-update-installed-package-list ()
  "Update the file with the lastest installed packages' list.
It should be used after new packages are installed in order
to maintain the right list."
  (interactive)
  (package-initialize)
  (let ((installed-package-list nil))
    (dolist (pkg package-alist)
      (if (assoc (car pkg) package-archive-contents)
          (add-to-list 'installed-package-list (car pkg))))
    (with-temp-file (get-package-list-filename)
      (prin1 installed-package-list (current-buffer)))
    (message "Successfully update installed-package-list!")))

;; define advice for package-install to automatically update list
(defadvice package-install (after update-installed-package-list)
  (qjp-update-installed-package-list))
(ad-activate 'package-install)

(provide 'qjp-package-manager)
;;; qjp-elpa-settings.el ends here
