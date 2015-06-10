;;; qjp-programming-ruby.el --- Settings for Ruby

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

;; Remove ruby-end and ruby-block because of smartparens

;;; Code:

;; auto-mode-alist for Ruby
(qjp-add-auto-mode 'ruby-mode
                   "Rakefile\\'" "\\.rake\\'" "\\.rxml\\'"
                   "\\.rjs\\'" "\\.irbrc\\'" "\\.pryrc\\'"
                   "\\.builder\\'" "\\.ru\\'" "\\.gemspec\\'"
                   "Gemfile\\'" "Kirkfile\\'")

(defun qjp-ruby-mode-hook ()
  "My mode hook for Ruby."
  (subword-mode +1)
  ;; Keybindings
  (local-set-key [(return)] #'electrify-return-if-match)
  ;; Ruby tools
  (ruby-tools-mode +1))

(add-hook 'ruby-mode-hook #'qjp-ruby-mode-hook)

(add-hook 'enh-ruby-mode-hook #'qjp-ruby-mode-hook)

(provide 'qjp-programming-ruby)
;;; qjp-programming-ruby.el ends here
