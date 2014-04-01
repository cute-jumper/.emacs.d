;;; qjp-starter-kit.el --- Functions learned from Emacs24-starter-kit

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

;; uniquify
(require 'uniquify)

;; Enable saveplace
(require 'saveplace)
(setq save-place t)

(setq smex-save-file (concat qjp-base-dir ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)


;; Should be able to eval-and-replace anywhere.
;; From emacs-starter-kit
(defun starter-kit-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(provide 'qjp-starter-kit)
;;; qjp-starter-kit.el ends here
