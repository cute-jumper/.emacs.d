;;; qjp-misc-projectile-config.el --- Config for projectile  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Junpeng Qiu

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

;; ---------- ;;
;; projectile ;;
;; ---------- ;;

(eval-after-load "projectile"
  '(progn
     (setq projectile-cache-file (expand-file-name "projectile.cache"
                                                   qjp-base-dir))
     (setq projectile-enable-caching t)
     (projectile-global-mode t)

     ;; Helm support
     (setq projectile-compilation-command 'helm)
     (helm-projectile-on)))

(global-set-key (kbd "C-c p h") 'helm-projectile)

(provide 'qjp-misc-projectile-config)
;;; qjp-misc-projectile-config.el ends here
