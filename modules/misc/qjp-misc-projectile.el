;;; qjp-misc-projectile.el --- Config for projectile  -*- lexical-binding: t; -*-

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

(setq projectile-cache-file (expand-file-name "projectile.cache"
                                              qjp-base-dir))
(setq projectile-enable-caching t)
(autoload 'helm-projectile-switch-project "helm-projectile" nil t)
(setq projectile-keymap-prefix (kbd "C-c p"))

(with-eval-after-load 'projectile
  (projectile-global-mode +1)

  ;; Helm support
  (setq projectile-compilation-command #'helm)
  (helm-projectile-on)

  ;; Must be after `helm-projectile-on'
  (setq projectile-switch-project-action #'projectile-commander))

(with-eval-after-load 'qjp-mode
  (define-key qjp-mode-map (kbd "C-c p h") #'helm-projectile)
  (define-key qjp-mode-map (kbd "C-c p p") #'helm-projectile-switch-project))

(provide 'qjp-misc-projectile)
;;; qjp-misc-projectile.el ends here
