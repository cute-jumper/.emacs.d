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

(defvar qjp-diminish-aggressive t)

(defmacro qjp-diminish (file mode-name &optional new-name)
  `(with-eval-after-load ',file
     (if qjp-diminish-aggressive
         (diminish ',mode-name)
       (diminish ',mode-name ,new-name))))

(defun qjp-misc-diminish-essential ()
  (interactive)
  (diminish 'auto-fill-function)
  (diminish 'abbrev-mode)
  (diminish 'which-key-mode)
  (diminish 'beacon-mode)
  (qjp-diminish volatile-highlights volatile-highlights-mode)
  (qjp-diminish whole-line-or-region whole-line-or-region-mode)
  (defun qjp-misc-diminish-helm ()
    (diminish 'helm-mode)
    (remove-hook 'helm-mode-hook #'qjp-misc-diminish-helm))
  (with-eval-after-load 'helm
    (add-hook 'helm-mode-hook #'qjp-misc-diminish-helm))
  (qjp-diminish redshank redshank-mode)
  (qjp-diminish anzu anzu-mode)
  (qjp-diminish hideshow hs-minor-mode)
  (qjp-diminish ace-pinyin ace-pinyin-mode)
  (qjp-diminish qjp-mode qjp-mode " Q")
  (qjp-diminish undo-tree undo-tree-mode " UT")
  (qjp-diminish paredit paredit-mode " Par")
  (qjp-diminish eldoc eldoc-mode " ElD")
  (qjp-diminish elisp-slime-nav elisp-slime-nav-mode " SNav")
  (qjp-diminish company company-mode " Com")
  (qjp-diminish haskell-mode interactive-haskell-mode " IntHS")
  (when qjp-diminish-aggressive
    (diminish 'global-whitespace-mode)
    (qjp-diminish autorevert auto-revert-mode)
    (qjp-diminish aggressive-indent aggressive-indent-mode)
    (qjp-diminish smartparens smartparens-mode)
    (qjp-diminish highlight-symbol highlight-symbol-mode)
    (qjp-diminish which-key which-key-mode)
    (qjp-diminish whitespace-cleanup-mode whitespace-cleanup-mode)
    (qjp-diminish indent-guide indent-guide-mode)
    (qjp-diminish highlight-parentheses highlight-parentheses-mode)))

(defun qjp-misc-diminish-spacemacs ()
  (interactive)
  (diminish 'auto-fill-function " 🅵")
  (diminish 'abbrev-mode " 🅐")
  (diminish 'global-whitespace-mode " 🅦")
  (qjp-diminish company company-mode " 🅒")
  (qjp-diminish flyspell flyspell-mode " 🅢")
  (qjp-diminish flycheck flycheck-mode " 🅕")
  (qjp-diminish helm helm-mode " 🅗")
  (qjp-diminish smartparens smartparens-mode " 🅢")
  (qjp-diminish qjp-mode qjp-mode " 🅠"))

(qjp-misc-diminish-essential)

(provide 'qjp-misc-diminish)
;;; qjp-misc-diminish.el ends here
