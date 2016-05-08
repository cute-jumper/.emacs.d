;;; qjp-programming-python.el --- Settings for Python

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

;; Fix flycheck and virtualenv
;; Modified from:
;; https://github.com/lunaryorn/.emacs.d/blob/master/lisp/flycheck-virtualenv.el

;; We can safely declare this function, since we'll only call it in Python Mode,
;; that is, when python.el was already loaded.
(declare-function python-shell-calculate-exec-path "python")

(defun qjp-flycheck-virtualenv-set-python-executables ()
  "Set Python executables for the current buffer."
  (let ((exec-path (python-shell-calculate-exec-path)))
    (if (setq-local flycheck-python-pylint-executable
                    (executable-find "pylint"))
        (setq-local flycheck-checker 'python-pylint)
      (when (setq-local flycheck-python-flake8-executable
                        (executable-find "flake8"))
        (setq-local flycheck-checker 'python-flake8)))))

(defun qjp-pyvenv-activate-after (directory)
  "Setup flycheck checker every time virtualenv is changed."
  (qjp-flycheck-virtualenv-set-python-executables))

(advice-add 'pyvenv-activate :after #'qjp-pyvenv-activate-after)

(with-eval-after-load 'company
  (add-to-list 'company-backends '(company-anaconda :with company-capf)))

(with-eval-after-load 'python
  (define-key python-mode-map (kbd "C-c e d") #'pydoc)
  (define-key python-mode-map (kbd "C-c e w") #'pyvenv-workon))

;; Python hook
(defun qjp-python-mode-hook ()
  (subword-mode +1)
  (indent-guide-mode +1)
  (pyvenv-mode +1)
  (anaconda-mode +1)
  (anaconda-eldoc-mode +1))

(add-hook 'python-mode-hook #'qjp-python-mode-hook)

(provide 'qjp-programming-python)
;;; qjp-programming-python.el ends here
