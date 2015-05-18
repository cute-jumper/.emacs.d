;;; qjp-misc-diminish.el --- Settings for diminish   -*- lexical-binding: t; -*-

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

(defun qjp-misc-diminish-essential ()
  (interactive)
  (diminish 'auto-fill-function)
  (diminish 'abbrev-mode)
  (with-eval-after-load 'qjp-mode
    (diminish 'qjp-mode " Q"))
  (with-eval-after-load 'volatile-highlights
    (diminish 'volatile-highlights-mode))
  (with-eval-after-load 'whole-line-or-region
    (diminish 'whole-line-or-region-mode))
  (defun qjp-misc-diminish-helm ()
    (diminish 'helm-mode)
    (remove-hook 'helm-mode-hook #'qjp-misc-diminish-helm))
  (with-eval-after-load 'helm
    (add-hook 'helm-mode-hook #'qjp-misc-diminish-helm))
  (with-eval-after-load 'redshank
    (diminish 'redshank-mode))
  (with-eval-after-load 'anzu
    (diminish 'anzu-mode))
  (with-eval-after-load 'hideshow
    (diminish 'hs-minor-mode))
  (with-eval-after-load 'magit
    (diminish 'magit-auto-revert-mode))
  (with-eval-after-load 'undo-tree
    (diminish 'undo-tree-mode " UT"))
  (with-eval-after-load 'paredit
    (diminish 'paredit-mode " Par"))
  (with-eval-after-load 'eldoc
    (diminish 'eldoc-mode " ElD"))
  (with-eval-after-load 'elisp-slime-nav
    (diminish 'elisp-slime-nav-mode " SNav"))
  (with-eval-after-load 'ace-pinyin
    (diminish 'ace-pinyin-mode)))

(defun qjp-misc-diminish-spacemacs ()
  (interactive)
  (diminish 'auto-fill-function " 🅵")
  (diminish 'abbrev-mode " 🅐")
  (diminish 'global-whitespace-mode " 🅦")
  (with-eval-after-load 'company
    (diminish 'company-mode " 🅒"))
  (with-eval-after-load 'flyspell
    (diminish 'flyspell-mode " 🅢"))
  (with-eval-after-load 'flycheck
    (diminish 'flycheck-mode " 🅕"))
  (with-eval-after-load 'helm
    (diminish 'helm-mode " 🅗"))
  (with-eval-after-load 'smartparens
    (diminish 'smartparens-mode " 🅢"))
  (with-eval-after-load 'qjp-mode
    (diminish 'qjp-mode " 🅠")))

(qjp-misc-diminish-essential)

(provide 'qjp-misc-diminish)
;;; qjp-misc-diminish.el ends here
