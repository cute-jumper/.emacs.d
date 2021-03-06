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

;; Update installed package list and install them at first initialization,
;; Besides, they can be used to sync the newly installed packages.
;; Idea from: http://blub.co.za/posts/Installing-packages-on-Emacs-startup.html
(defvar qjp-installed-package-list-filename
  (expand-file-name "installed-package-list" qjp-base-dir)
  "The place to save the installed package list.")

(defvar qjp-installed-package-list
  (if (version< emacs-version "25")
      (when (file-exists-p qjp-installed-package-list-filename)
        (read
         (with-temp-buffer
           (insert-file-contents qjp-installed-package-list-filename)
           (buffer-string))))
    package-selected-packages)
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
          (insert (format "\t--> [%d/%d] %s has been installed.\n" counter total pkg))
        (ignore-errors
          (insert (format "\t--> [%d/%d] Installing %s..." counter total pkg))
          (package-install pkg)
          (insert "done.\n"))))
    (insert "Package installation is done. \
Enjoy your journey with Emacs:-)\n")))

(when (catch 'break
        (dolist (pkg qjp-installed-package-list)
          (unless (package-installed-p pkg)
            (throw 'break t))))
  (qjp-install-all-packages))

(provide '03qjp-package-manager)
;;; 03qjp-package-manager.el ends here
