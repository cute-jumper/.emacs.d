;;; qjp-misc.el --- Simple settings for various modes/features

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

;; Guideline:
;; 1. If the settings are short, put them directly in this file
;; 2. Otherwise, put it into a separate file under `misc' directory.
;;    The feature name should be `qjp-misc-{mode name}'

;;; Code:

;; Macro definition
(defmacro qjp-misc-subdir-defun-macro (feature-name)
  "A macro to simplify the `defun' for requiring feature in
  sub-directory"
  (let ((func-name (intern (concat "qjp-misc-" (symbol-name feature-name)))))
    `(defun ,func-name ()
       (qjp-modules-require-subdir-feature
        "misc"
        (concat "qjp-misc-" ,(symbol-name feature-name) "-config")))))

;; --------------------------------------------------------- ;;
;; multiple-cursors, using region, global and mouse bindings ;;
;; --------------------------------------------------------- ;;
(qjp-misc-subdir-defun-macro multiple-cursors)

;; ------ ;;
;; EasyPG ;;
;; ------ ;;
(qjp-misc-subdir-defun-macro easypg)

;; --------- ;;
;; evil-mode ;;
;; --------- ;;
(qjp-misc-subdir-defun-macro evil)

;; ----- ;;
;; dired ;;
;; ----- ;;
(qjp-misc-subdir-defun-macro dired)

;; ----------- ;;
;; tabbar-mode ;;
;; ----------- ;;
(qjp-misc-subdir-defun-macro tabbar)

;; --------------------- ;;
;; hideshow, hideshowvis ;;
;; --------------------- ;;
(qjp-misc-subdir-defun-macro hs)

;; ----------------------------- ;;
;; Settings for various packages ;;
;; ----------------------------- ;;
(qjp-modules-require-subdir-feature "misc" "qjp-misc-pkg-config")

;; -------------------------------------- ;;
;; List the modes you want to enable here ;;
;; -------------------------------------- ;;
(defvar qjp-enabled-misc-settings-list '(builtin key-chord
  region-bindings goto-last-change ace-jump ace-jump-buffer
  jump-char expand-region multiple-cursors easypg ispell
  dictionary w3m command-log calfw markdown sr-speedbar evil term
  dired hs fetch-bibtex)
  "The short mode function name that should be enabled")

;; Enable these settings
(defun qjp-enable-misc-settings ()
  (mapc (lambda (m) (funcall (intern (concat "qjp-misc-" (symbol-name m)))))
        qjp-enabled-misc-settings-list))

(qjp-enable-misc-settings)

(provide 'qjp-misc)
;;; qjp-misc.el ends here
