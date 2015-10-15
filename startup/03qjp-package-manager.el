;;; 03qjp-package-manager.el --- Settings for package management

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

;; Package sources for Emacs 24
(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;; I don't use melpa-stable often. Don't enable it.
;;(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

;; Update installed package list and install them at first initialization,
;; Besides, they can be used to sync the newly installed packages.
;; Idea from: http://blub.co.za/posts/Installing-packages-on-Emacs-startup.html
(defvar qjp-installed-package-list-filename
  (expand-file-name "installed-package-list" qjp-base-dir)
  "The place to save the installed package list.")

(defvar qjp-installed-package-list
  (when (file-exists-p qjp-installed-package-list-filename)
    (read
     (with-current-buffer
         (find-file-noselect qjp-installed-package-list-filename)
       (buffer-substring-no-properties (point-min) (point-max)))))
  "A list of installed packages.")

(defun qjp-install-all-packages ()
  "Use this command to install all the packages.
It reads from the file and get the installed packages's list,
and installs these packages one by one."
  (interactive)
  (package-refresh-contents)
  (let ((counter 0)
        (total (length qjp-installed-package-list)))
    (switch-to-buffer (get-buffer-create "*Installing Packages*"))
    (insert
     (format "Installing %d packages...\n" total))
    (dolist (pkg qjp-installed-package-list)
      (setq counter (1+ counter))
      (if (package-installed-p pkg)
          (insert (format "\t--> [%d/%d] %s has beenf installed.\n" counter total pkg))
        (ignore-errors
          (insert (format "\t--> [%d/%d] Installing %s..." counter total pkg))
          (package-install pkg)
          (insert "done.\n"))))
    (insert "Package installation is done. \
Enjoy your journey with Emacs:-)\n")))

;; This function should be used when maintaining the settings, not at the first initialization.
(defun qjp-update-installed-package-list (&rest args)
  "Update the file with the lastest installed packages' list.
It should be used after new packages are installed in order
to maintain the right list."
  (interactive)
  (package-initialize)
  (setq qjp-installed-package-list)
  (dolist (pkg package-alist)
    (when (assoc (car pkg) package-archive-contents)
      (add-to-list 'qjp-installed-package-list (car pkg))))
  (with-temp-file qjp-installed-package-list-filename
    (prin1 qjp-installed-package-list (current-buffer)))
  (message "Successfully update installed-package-list!"))

(advice-add 'package-menu-execute :after #'qjp-update-installed-package-list)

(package-initialize)

(when (catch 'break
        (dolist (pkg qjp-installed-package-list)
          (unless (package-installed-p pkg)
            (throw 'break t))))
  (qjp-install-all-packages))

(provide '03qjp-package-manager)
;;; 03qjp-package-manager.el ends here
