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
(defun qjp-misc-make-func-name-string (feature-name)
  "Helper function to construct function name of `misc' module"
  (concat "qjp-misc-" (symbol-name feature-name)))

(defmacro qjp-misc-make-macro (feature-name &rest body)
  "Toplevel macro to define macro of `misc' module"
  (let* ((func-name-string (qjp-misc-make-func-name-string feature-name))
         (func-name (intern func-name-string))
         (flag-name (intern (concat func-name-string "-flag"))))
    `(defsubst ,func-name (&optional v)
       (defvar ,flag-name t)
       (when (or ,flag-name v)
         (setq ,flag-name nil)
         ,@body))))

(defmacro qjp-misc-inline-defun (feature-name &rest args)
  "The `defun' macro for inline settings"
  `(qjp-misc-make-macro ,feature-name
                        ,@args))

(defmacro qjp-misc-file-defun (feature-name)
  "The `defun' macro for inline settings"
  `(qjp-misc-make-macro
    ,feature-name
    (qjp-modules-require-subdir-feature
     "misc"
     (concat ,(qjp-misc-make-func-name-string feature-name) "-config"))))

;; ---------------------------------- ;;
;; Load settings for various packages ;;
;; ---------------------------------- ;;
(qjp-modules-require-subdir-feature "misc" "qjp-misc-package-config-defuns")

;; -------------------------------------- ;;
;; List the modes you want to enable here ;;
;; -------------------------------------- ;;
(defvar qjp-enabled-misc-settings-list
  '(ace-jump ace-jump-buffer ace-jump-zap ace-flyspell ace-jump-helm-line ace-pinyin anchored-transpose anzu auto-insert
             bing-dict
             company
             dired
             easypg expand-region
             flyspell
             goto-last-change gscholar-bibtex
             helm hs fcitx hydra;; loaded after helm
             idle-highlight ispell
             jump-char
             key-chord
             lacarte
             magit markdown multiple-cursors
             nyan
             projectile
             quickrun
             rebox region-bindings
             sml sr-speedbar
             term
             whole-line-or-region
             ;;yasnippet
             )
  "The short mode function name that should be enabled")

;; Enable these settings
(defun qjp-misc-enable-setting (feature-name)
  (let ((func-name (qjp-misc-make-func-name-string feature-name)))
    (qjp-timed (funcall (intern func-name)) func-name "21qjp-misc-details")))

(defun qjp-misc-enable-all ()
  (mapc 'qjp-misc-enable-setting qjp-enabled-misc-settings-list))

(qjp-misc-enable-all)

(provide 'qjp-misc)
;;; qjp-misc.el ends here
