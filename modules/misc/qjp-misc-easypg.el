;;; qjp-misc-easypg-config.el --- Settings for EasyPG

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

;; For Arch Linux, force the gpg to use version 1, not 2.
(when (file-executable-p "/usr/bin/gpg1")
  (setq epg-gpg-program "/usr/bin/gpg1"))

(setenv "GPG_AGENT_INFO" nil)
;; save the password
(setq epa-file-cache-passphrase-for-symmetric-encryption t)
;; auto-save
(setq epa-file-inhibit-auto-save nil)


(provide 'qjp-misc-easypg-config)
;;; qjp-misc-easypg-config.el ends here
